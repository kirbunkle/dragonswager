unit gameDeck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCardGeneric, fgl, commonConstType;

type
  TGameCardGenericList = specialize TFPGList<GameCardGenericClass>;
  TGameCardGenericListMap = specialize TFPGMap<TGameZone, TGameCardGenericList>;

  GameDeckClass = class
    private
      mCardZones : TGameCardGenericListMap;
    public
      constructor Create;
      destructor Destroy; override;
      function getCard(const zone : TGameZone; const index : integer) : GameCardGenericClass;
      function count(const zone : TGameZone) : integer;
      procedure add(const card : GameCardGenericClass);
      procedure cleanup;
  end;

implementation

constructor GameDeckClass.Create;
var
  zone : TGameZone = gz_unknown;
begin
  inherited Create;
  mCardZones := TGameCardGenericListMap.Create;
  for zone in TGameZone do begin
    mCardZones.Add(zone, TGameCardGenericList.Create);
  end;
end;

destructor GameDeckClass.Destroy;
var
  i       : integer = 0;
  j       : integer = 0;
  zoneRef : TGameCardGenericList = nil;
  cardRef : GameCardGenericClass = nil;
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

function GameDeckClass.getCard(const zone : TGameZone; const index : integer) : GameCardGenericClass;
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

procedure GameDeckClass.add(const card : GameCardGenericClass);
begin
  mCardZones.KeyData[card.zone].Add(card);
end;

procedure GameDeckClass.cleanup;
var
  j : integer = 0;
  i : integer = 0;
begin
  for i := mCardZones.Count - 1 downto 0 do begin
    for j := mCardZones.Data[i].Count downto 0 do begin
      if mCardZones.Data[i].Items[j].zone <> mCardZones.Keys[i] then begin
        mCardZones.KeyData[mCardZones.Data[i].Items[j].zone].Add(mCardZones.Data[i].Items[j]);
        mCardZones.Data[i].Delete(j);
      end;
    end;
  end;
end;

end.

