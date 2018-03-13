unit cliGenericTargetableStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliGenericStateHandler,
  cliInteractableReference,
  cliWriter,
  gameState;

type
  TInteractibleTypeSet = set of TInteractableType;

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
  if it_hero in mValidInteractables then begin
    for i := 0 to globalGameState.heroGroup.count - 1 do begin
      globalWriter.addLine('- ' + globalInteractableRef.getRef(i, it_hero) + ' (for hero #' + IntToStr(i+1) + ': ' +
        globalGameState.heroGroup.getCharacter(i).name + ')');
    end;
  end;
  if it_enemy in mValidInteractables then begin
    for i := 0 to globalGameState.enemyGroup.count - 1 do begin
      globalWriter.addLine('- ' + globalInteractableRef.getRef(i, it_enemy) + ' (for enemy #' + IntToStr(i+1) + ': ' +
        globalGameState.enemyGroup.getCharacter(i).name + ')');
    end;
  end;
end;

end.

