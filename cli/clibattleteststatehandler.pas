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
  gameCard,
  cliTableWriter,
  cliInteractableReference,
  cliWriter,
  commonConstType;

type
  CLIBattleTestStateHandlerClass = class(CLIGenericGameStateHandlerClass)
    private
      procedure displayGroup(
        const group     : GameCharacterGroupClass;
        const interType : byte);
      procedure displayCards;
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
      procedure doAction; override;
  end;

implementation

procedure CLIBattleTestStateHandlerClass.displayGroup(
  const group     : GameCharacterGroupClass;
  const interType : byte);
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

procedure CLIBattleTestStateHandlerClass.displayCards;
var
  card        : GameCardClass = nil;
  i           : integer = 0;
  tableWriter : CLITableWriterClass = nil;
begin
  tableWriter := CLITableWriterClass.Create;
  try
    for i := 0 to globalGameState.cardZones.count(gz_hand) - 1 do begin
      card := globalGameState.cardZones.getCard(gz_hand, i);
      tableWriter.addElement('(' + globalInteractableRef.getRef(i, INTERACTABLE_TYPE_CARD) + ') '
        + card.name);
    end;
    tableWriter.display;
  finally
    FreeAndNil(tableWriter);
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
  displayGroup(globalGameState.enemyGroup, INTERACTABLE_TYPE_ENEMY);
  globalWriter.addLine('');
  globalWriter.addLine(' *** VERSUS *** ');
  globalWriter.addLine('');
  displayGroup(globalGameState.heroGroup, INTERACTABLE_TYPE_HERO);
  globalWriter.addLine('');
  globalWriter.addLine('Cards:');
  displayCards;
end;

procedure CLIBattleTestStateHandlerClass.doAction;
begin
  inherited doAction;
  globalGameState.setupEnemyGroup;
  globalGameState.setupHeroGroup;
  globalGameState.setupCards;
  globalGameState.cardZones.draw(3);
end;

end.

