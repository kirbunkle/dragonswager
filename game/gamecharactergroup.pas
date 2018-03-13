unit gameCharacterGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacter;

type
  GameCharacterGroupClass = class
    private
      mCount            : integer;
      mCharacters       : array of GameCharacterClass;

      procedure setupCharacters(const idList : TStringList);
    public
      constructor Create(const idList : TStringList);
      constructor Create(const idList : string);
      destructor Destroy; override;
      function getCharacter(const index : integer) : GameCharacterClass;
      property count : integer read mCount;
  end;

implementation

procedure GameCharacterGroupClass.setupCharacters(const idList : TStringList);
var
  i : integer = 0;
begin
  mCount := idList.Count;
  SetLength(mCharacters, mCount);
  for i := 0 to mCount - 1 do begin
    mCharacters[i] := GameCharacterClass.Create(idList.Strings[i]);
  end;
end;

constructor GameCharacterGroupClass.Create(const idList : TStringList);
begin
  inherited Create;
  setupCharacters(idList);
end;

constructor GameCharacterGroupClass.Create(const idList : string);
var
  list : TStringList = nil;
begin
  inherited Create;
  list := TStringList.Create;
  try
    list.CommaText := idList;
    setupCharacters(list);
  finally
    FreeAndNil(list);
  end;
end;

destructor GameCharacterGroupClass.Destroy;
var
  i : integer = 0;
begin
  for i := 0 to mCount - 1 do begin
    FreeAndNil(mCharacters[i]);
  end;
  SetLength(mCharacters, 0);
  inherited Destroy;
end;

function GameCharacterGroupClass.getCharacter(const index : integer) : GameCharacterClass;
begin
  result := nil;
  if index < mCount then begin
    result := mCharacters[index];
  end;
end;

end.

