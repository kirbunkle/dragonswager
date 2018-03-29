unit cliInteractableReference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacter, gameCharacterGroup, gameState, commonConstType;

type
  CLIInteractableReferenceClass = class
    public
      constructor Create;
      destructor Destroy; override;
      function getRef(
        const index  : integer;
        const typeIn : byte) : string;
      function getCharacter(const ref : string) : GameCharacterClass;
  end;

var
  globalInteractableRef : CLIInteractableReferenceClass = nil;

implementation

const
  HERO_REF_CHARS = 'asdf';
  ENEMY_REF_CHARS = 'qwertyuiop';
  CARD_REF_CHARS = '1234567890';

constructor CLIInteractableReferenceClass.Create;
begin
  inherited Create;
end;

destructor CLIInteractableReferenceClass.Destroy;
begin
  inherited Destroy;
end;

function CLIInteractableReferenceClass.getRef(
  const index  : integer;
  const typeIn : byte) : string;
begin
  result := '';
  if index < 0 then exit;
  case typeIn of
    INTERACTABLE_TYPE_HERO : begin
      if index < Length(HERO_REF_CHARS) then result := HERO_REF_CHARS[index+1];
    end;
    INTERACTABLE_TYPE_ENEMY : begin
      if index < Length(ENEMY_REF_CHARS) then result := ENEMY_REF_CHARS[index+1];
    end;
    INTERACTABLE_TYPE_CARD : begin
      if index < Length(CARD_REF_CHARS) then result := CARD_REF_CHARS[index+1];
    end;
  end;
end;

function CLIInteractableReferenceClass.getCharacter(const ref : string) : GameCharacterClass;
var
  index : integer = 0;
  myRef : string = '';
begin
  result := nil;
  if Length(ref) > 1 then exit;
  myRef := LowerCase(ref);
  index := Pos(myRef, HERO_REF_CHARS) - 1;
  if index >= 0 then begin
    result := globalGameState.heroGroup.getCharacter(index);
    exit;
  end;
  index := Pos(myRef, ENEMY_REF_CHARS) - 1;
  if index >= 0 then begin
    result := globalGameState.enemyGroup.getCharacter(index);
    exit;
  end;
end;

end.

