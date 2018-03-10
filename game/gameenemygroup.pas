unit gameEnemyGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacter;

type
  GameEnemyGroupClass = class
    private
      mCount            : integer;
      mCharacters       : array of GameCharacterClass;
    public
      constructor Create;
      destructor Destroy; override;
      function getCharacter(const index : integer) : GameCharacterClass;
      property count : integer read mCount;
  end;

implementation

constructor GameEnemyGroupClass.Create;
var
  i : integer = 0;
begin
  inherited Create;
  mCount := 4;
  SetLength(mCharacters, mCount);
  for i := 0 to mCount - 1 do begin
    mCharacters[i] := GameCharacterClass.Create('1');
  end;
end;

destructor GameEnemyGroupClass.Destroy;
var
  i : integer = 0;
begin
  for i := 0 to mCount - 1 do begin
    FreeAndNil(mCharacters[i]);
  end;
  SetLength(mCharacters, 0);
  inherited Destroy;
end;

function GameEnemyGroupClass.getCharacter(const index : integer) : GameCharacterClass;
begin
  result := nil;
  if index < mCount then begin
    result := mCharacters[index];
  end;
end;

end.

