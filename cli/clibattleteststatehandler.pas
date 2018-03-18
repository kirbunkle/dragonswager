unit cliBattleTestStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliGenericGameStateHandler,
  cliGenericStateHandler,
  gameState,
  gameCharacterGroup,
  gameCharacter,
  cliTableWriter,
  cliInteractableReference,
  cliWriter,
  commonConstType;

type
  CLIBattleTestStateHandlerClass = class(CLIGenericGameStateHandlerClass)
    private
      procedure displayGroup(
        const group     : GameCharacterGroupClass;
        const interType : TInteractableType);
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
      procedure doAction; override;
  end;

implementation

procedure CLIBattleTestStateHandlerClass.displayGroup(
  const group     : GameCharacterGroupClass;
  const interType : TInteractableType);
var
  character   : GameCharacterClass = nil;
  i           : integer = 0;
  tableWriter : CLITableWriterClass = nil;
begin
  if group <> nil then begin
    tableWriter := CLITableWriterClass.Create;
    try
      for i := 0 to group.count - 1 do begin
        character := group.getCharacter(i);
        tableWriter.addElement('(' + globalInteractableRef.getRef(i, interType) + ') '
          + character.name, 'HP: ' + IntToStr(character.hp));
      end;
      tableWriter.display;
    finally
      FreeAndNil(tableWriter);
    end;
  end;
end;

constructor CLIBattleTestStateHandlerClass.Create;
begin
  inherited Create;
  mStateName := 'BATTLE TEST';
end;

destructor CLIBattleTestStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIBattleTestStateHandlerClass.display;

begin
  inherited display;
  displayGroup(globalGameState.enemyGroup, it_enemy);
  globalWriter.addLine('');
  globalWriter.addLine(' *** VERSUS *** ');
  globalWriter.addLine('');
  displayGroup(globalGameState.heroGroup, it_hero);
end;

procedure CLIBattleTestStateHandlerClass.doAction;
begin
  inherited doAction;
  globalGameState.setupEnemyGroup;
  globalGameState.setupHeroGroup;
end;

end.

