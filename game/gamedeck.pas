unit gameDeck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCard, fgl, commonConstType;

type
  TGameCardList = specialize TFPGList<GameCardClass>;
  TGameCardListMap = specialize TFPGMap<TGameZone, TGameCardList>;

  GameDeckClass = class
    private
      mCardZones : TGameCardListMap;
    public
      constructor Create;
      destructor Destroy; override;
      function getCard(const zone : TGameZone; const index : integer) : GameCardClass;
      function count(const zone : TGameZone) : integer;
      procedure add(const card : GameCardClass);
      procedure add(const zone: TGameZone; const list : string);
      procedure moveTop(
        const fromZone : TGameZone;
        const toZone   : TGameZone;
        const number   : integer = 1);
      procedure draw(const number : integer = 1);
      procedure shuffle(const zone : TGameZone);
      procedure cleanup;
  end;

implementation

constructor GameDeckClass.Create;
var
  zone : TGameZone = gz_unknown;
begin
  inherited Create;
  mCardZones := TGameCardListMap.Create;
  for zone in TGameZone do begin
    mCardZones.Add(zone, TGameCardList.Create);
  end;
end;

destructor GameDeckClass.Destroy;
var
  i       : integer = 0;
  j       : integer = 0;
  zoneRef : TGameCardList = nil;
  cardRef : GameCardClass = nil;
begin
  for i := mCardZones.Count - 1 downto 0 do begin
    zoneRef := mCardZones.Data[i];
    for j := zoneRef.Count - 1 downto 0 do begin
      cardRef := zoneRef.Items[j];
      FreeAndNil(cardRef);
      zoneRef.Delete(j);
    end;
    FreeAndNil(zoneRef);
  end;
  FreeAndNil(mCardZones);
  inherited Destroy;
end;

function GameDeckClass.getCard(const zone : TGameZone; const index : integer) : GameCardClass;
begin
  result := nil;
  if index < mCardZones.KeyData[zone].Count then begin
    result := mCardZones.KeyData[zone].Items[index];
  end;
end;

function GameDeckClass.count(const zone : TGameZone) : integer;
begin
  result := mCardZones.KeyData[zone].Count;
end;

procedure GameDeckClass.add(const card : GameCardClass);
begin
  mCardZones.KeyData[card.zone].Add(card);
end;

procedure GameDeckClass.add(const zone: TGameZone; const list : string);
var
  card    : GameCardClass = nil;
  cardIDs : TStringList = nil;
  i       : integer = 0;
begin
  cardIDs := TStringList.Create;
  try
    cardIDs.CommaText := list;
    for i := 0 to cardIDs.Count - 1 do begin
      card := GameCardClass.Create(cardIDs.Strings[i]);
      card.zone := zone;
      add(card);
    end;
  finally
    FreeAndNil(cardIDs);
  end;
end;

procedure GameDeckClass.moveTop(
  const fromZone : TGameZone;
  const toZone   : TGameZone;
  const number   : integer = 1);
var
  fromRef : TGameCardList = nil;
  toRef   : TGameCardList = nil;
  i       : integer = 0;
  card    : GameCardClass = nil;
begin
  fromRef := mCardZones.KeyData[fromZone];
  toRef := mCardZones.KeyData[toZone];
  i := fromRef.Count - 1;
  while (i >= 0) and (i >= fromRef.Count - number) do begin
    card := fromRef.Items[i];
    card.zone := toZone;
    toRef.Add(card);
    fromRef.Delete(i);
    Dec(i);
  end;
end;

procedure GameDeckClass.draw(const number : integer = 1);
begin
  moveTop(gz_deck, gz_hand, number);
end;

procedure GameDeckClass.shuffle(const zone : TGameZone);
var
  zoneRef : TGameCardList = nil;
  tmpRef  : TGameCardList = nil;
  i       : integer = 0;
  rand    : integer = 0;
begin
  zoneRef := mCardZones.KeyData[zone];
  tmpRef := TGameCardList.Create;
  try
    for i := zoneRef.Count - 1 to 0 do begin
      tmpRef.Add(zoneRef.Items[i]);
      zoneRef.Delete(i);
    end;
    while tmpRef.Count > 0 do begin
      if tmpRef.Count > 1 then begin
        rand := Random(tmpRef.Count);
      end else begin
        rand := 0;
      end;
      zoneRef.Add(tmpRef.Items[rand]);
      tmpRef.Delete(rand);
    end;
  finally
    FreeAndNil(tmpRef);
  end;
end;

procedure GameDeckClass.cleanup;
var
  j : integer = 0;
  i : integer = 0;
begin
  for i := mCardZones.Count - 1 downto 0 do begin
    for j := mCardZones.Data[i].Count - 1 downto 0 do begin
      if mCardZones.Data[i].Items[j].zone <> mCardZones.Keys[i] then begin
        mCardZones.KeyData[mCardZones.Data[i].Items[j].zone].Add(mCardZones.Data[i].Items[j]);
        mCardZones.Data[i].Delete(j);
      end;
    end;
  end;
end;

end.

