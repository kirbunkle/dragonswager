unit cliGenericTargetableStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliGenericStateHandler,
  cliInteractableReference,
  cliWriter,
  gameState,
  commonConstType;

type
  TInteractibleTypeSet = set of byte;

  CLIGenericTargetableStateHandlerClass = class(CLIGenericStateHandlerClass)
    private
      mValidInteractables : TInteractibleTypeSet;
    public
      constructor Create(const interactablesIn : TInteractibleTypeSet);
      destructor Destroy; override;
      procedure showHelp; override;
end;

implementation

constructor CLIGenericTargetableStateHandlerClass.Create(
  const interactablesIn : TInteractibleTypeSet);
begin
  inherited Create;
  mValidInteractables := interactablesIn;
end;

destructor CLIGenericTargetableStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIGenericTargetableStateHandlerClass.showHelp;
var
  i : integer = 0;
begin
  inherited showHelp;
  if INTERACTABLE_TYPE_HERO in mValidInteractables then begin
    for i := 0 to globalGameState.heroGroup.count - 1 do begin
      globalWriter.addLine('- ' + globalInteractableRef.getRef(i, INTERACTABLE_TYPE_HERO) + ' (for hero #' + IntToStr(i+1) + ': ' +
        globalGameState.heroGroup.getCharacter(i).name + ')');
    end;
  end;
  if INTERACTABLE_TYPE_ENEMY in mValidInteractables then begin
    for i := 0 to globalGameState.enemyGroup.count - 1 do begin
      globalWriter.addLine('- ' + globalInteractableRef.getRef(i, INTERACTABLE_TYPE_ENEMY) + ' (for enemy #' + IntToStr(i+1) + ': ' +
        globalGameState.enemyGroup.getCharacter(i).name + ')');
    end;
  end;
  if INTERACTABLE_TYPE_CARD in mValidInteractables then begin
    for i := 0 to globalGameState.cardZones.count(gz_hand) - 1 do begin
      globalWriter.addLine('- ' + globalInteractableRef.getRef(i, INTERACTABLE_TYPE_CARD) + ' (for card #' + IntToStr(i+1) + ': ' +
        globalGameState.cardZones.getCard(gz_hand, i).name + ')');
    end;
  end;
end;

end.

