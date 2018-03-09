unit gameEnemyGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacter;

type
  GameEnemyGroupClass = class
    private
      mCharacters       : array of GameCharacterClass;
    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

constructor GameEnemyGroupClass.Create;
var
  i : integer = 0;
begin
  inherited Create;
  SetLength(mCharacters, 4);
  for i := 0 to Length(mCharacters) - 1 do begin
    mCharacters[i] := GameCharacterClass.Create;
  end;
end;

destructor GameEnemyGroupClass.Destroy;
var
  i : integer = 0;
begin
  for i := 0 to Length(mCharacters) - 1 do begin
    FreeAndNil(mCharacters[i]);
  end;
  SetLength(mCharacters, 0);
  inherited Destroy;
end;

end.

