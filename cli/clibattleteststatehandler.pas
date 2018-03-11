unit cliBattleTestStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliGenericGameStateHandler,
  cliGenericStateHandler,
  gameState,
  gameEnemyGroup,
  gameCharacter,
  cliTableWriter;

type
  CLIBattleTestStateHandlerClass = class(CLIGenericGameStateHandlerClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
      procedure doAction; override;
  end;

implementation

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
var
  enemyGroup  : GameEnemyGroupClass = nil;
  enemy       : GameCharacterClass = nil;
  i           : integer = 0;
  tableWriter : CLITableWriterClass = nil;
begin
  inherited display;
  tableWriter := CLITableWriterClass.Create;
  try
    enemyGroup := globalGameState.enemyGroup;
    if enemyGroup <> nil then begin
      for i := 0 to enemyGroup.count - 1 do begin
        enemy := enemyGroup.getCharacter(i);
        tableWriter.addElement('(' + IntToStr(i + 1) + ') ' + enemy.name, 'HP: ' + IntToStr(enemy.hp));
      end;
      tableWriter.display;
    end;
  finally
    FreeAndNil(tableWriter);
  end;
end;

procedure CLIBattleTestStateHandlerClass.doAction;
begin
  inherited doAction;
  globalGameState.setupEnemyGroup;
end;

end.

