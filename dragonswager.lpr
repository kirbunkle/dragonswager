program dragonswager;

{$mode objfpc}{$H+}

uses
  Classes,
  sysutils,
  gameState,
  cliMainMenuStateHandler,
  cliGenericStateHandler,
  cliWriter;

procedure setup;
begin
  globalGameState := GameStateClass.Create;
  globalWriter := CLIWriterClass.Create;
end;

procedure shutdown;
begin
  FreeAndNil(globalWriter);
  FreeAndNil(globalGameState);
end;

procedure run;
var
  text : string = '';
  mainMenuStateHandler : CLIMainMenuStateHandlerClass = nil;
begin
  mainMenuStateHandler := CLIMainMenuStateHandlerClass.Create;
  mainMenuStateHandler.doAction; // set as current state
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

