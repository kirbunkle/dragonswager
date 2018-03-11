unit cliInfoStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliGenericStateHandler,
  gameState,
  gameEnemyGroup,
  gameCharacter,
  cliCommandReference,
  cliWriter;

type
  CLIInfoStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
      function doSpecialCommand(const command : TCommandType) : boolean override;
  end;

implementation

const REF_INDEX_ENEMY_GROUP = 1000;

constructor CLIInfoStateHandlerClass.Create;
var
  enemyGroup   : GameEnemyGroupClass = nil;
  character    : GameCharacterClass = nil;
  i            : integer = 0;
  index        : integer = 0;
begin
  inherited Create;
  mStateName := 'INFO';
  mCommandTable.add('back,no', STATE_CANCEL);

  // read gamestate to see all visible items/objects/characters
  enemyGroup := globalGameState.enemyGroup;
  if enemyGroup <> nil then begin
    for i := 0 to enemyGroup.count - 1 do begin
      character := enemyGroup.getCharacter(i);
      if character <> nil then begin
        index := i + REF_INDEX_ENEMY_GROUP;
        mCommandTable.add(IntToStr(i + 1)+','+character.name, index);
      end;
    end;
  end;
end;

destructor CLIInfoStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIInfoStateHandlerClass.display;
begin
  inherited display;
  globalWriter.addLine('Choose a character or item.');
end;

function CLIInfoStateHandlerClass.doSpecialCommand(const command : TCommandType) : boolean;
var
  myIndex    : integer = 0;
  enemyGroup : GameEnemyGroupClass = nil;
  character  : GameCharacterClass = nil;
begin
  result := false;
  if command >= REF_INDEX_ENEMY_GROUP then begin
    if command < 2000 then begin
      myIndex := command - REF_INDEX_ENEMY_GROUP;
      enemyGroup := globalGameState.enemyGroup;
      if enemyGroup <> nil then begin
        character := enemyGroup.getCharacter(myIndex);
        if character <> nil then begin
          globalWriter.addLine('Name: ' + character.name);
          globalWriter.addLine('HP: ' + IntToStr(character.hp));
          globalWriter.addLine('"' + character.description + '"');
          result := true;
          cancel;
        end;
      end;
    end;
  end;
end;

end.

