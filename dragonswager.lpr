program dragonswager;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, gameState, cliMainMenuStateHandler, cliGenericStateHandler,
  cliWriter, gameDatabase, cliTableWriter, cliInfoStateHandler,
  cliGenericGameStateHandler, cliInteractableReference,
  cliGenericTargetableStateHandler, gameDeck, commonConstType;

procedure setup;
begin
  globalDatabase := GameDatabaseClass.Create;
  globalGameState := GameStateClass.Create;
  globalWriter := CLIWriterClass.Create;
  globalInteractableRef := CLIInteractableReferenceClass.Create;
end;

procedure shutdown;
begin
  FreeAndNil(globalInteractableRef);
  FreeAndNil(globalWriter);
  FreeAndNil(globalGameState);
  FreeAndNil(globalDatabase);
end;

procedure run;
var
  text : string = '';
  mainMenuStateHandler : CLIMainMenuStateHandlerClass = nil;
begin
  mainMenuStateHandler := CLIMainMenuStateHandlerClass.Create;
  mainMenuStateHandler.doAction;
  while globalGameState.currentState <> nil do begin
    CLIGenericStateHandlerClass(globalGameState.currentState).display;
    globalWriter.display;
    Write('>> ');
    ReadLn(text);
    CLIGenericStateHandlerClass(globalGameState.currentState).processInput(text);
  end;
end;

begin
  setup;
  try
    run;
  finally
    shutdown;
  end;
end.

