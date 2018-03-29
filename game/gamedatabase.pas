unit gameDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DB_TYPE_CHARACTER     = 0;
  DB_TYPE_CARD          = 1;
  DB_TYPE_STATUS_EFFECT = 2;

  NUM_OF_DB_TYPES = 3;

type

  GameDatabaseClass = class
    private
      mData : array of TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      function getData(const dataType : word; const id : string) : string;
      function getData(const dataType : word; const id : integer) : string;
  end;

var
  globalDatabase : GameDatabaseClass = nil;

implementation

constructor GameDatabaseClass.Create;
var
  i : integer = 0;
begin
  inherited Create;
  SetLength(mData, NUM_OF_DB_TYPES);
  for i := 0 to NUM_OF_DB_TYPES - 1 do begin
    mData[i] := TStringList.Create;
    mData[i].Sorted := true;
    mData[i].Duplicates := dupIgnore;
    mData[i].StrictDelimiter := true;
    mData[i].Delimiter := ',';
  end;
  mData[DB_TYPE_CHARACTER].LoadFromFile('db/characters.db');
  mData[DB_TYPE_CARD].LoadFromFile('db/cards.db');
  mData[DB_TYPE_STATUS_EFFECT].LoadFromFile('db/statusEffects.db');
end;

destructor GameDatabaseClass.Destroy;
var
  i : integer = 0;
begin
  for i := 0 to NUM_OF_DB_TYPES - 1 do begin
    FreeAndNil(mData[i]);
  end;
  SetLength(mData, 0);
  inherited Destroy;
end;

function GameDatabaseClass.getData(
  const dataType : word;
  const id       : string) : string;
begin
  result := '';
  if dataType < NUM_OF_DB_TYPES then begin
    result := mData[dataType].Values[id];
  end;
end;

function GameDatabaseClass.getData(
  const dataType : word;
  const id       : integer) : string;
begin
  result := getData(dataType, IntToStr(id));
end;

end.
