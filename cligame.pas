unit cligame;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliWriter,
  cliMainMenuStateHandler,
  cliGenericStateHandler,
  gameState;

type

  CLIGameClass = class
    private
      procedure display;
    public
      constructor Create;
      destructor Destroy; override;
      function run : integer;
  end;

implementation

procedure CLIGameClass.display;
begin
  while globalWriter.displayNext do begin
    Sleep(250);
  end;
end;

constructor CLIGameClass.Create;
begin
  inherited Create;
end;

destructor CLIGameClass.Destroy;
begin
  inherited Destroy;
end;

function CLIGameClass.run : integer;
var
  text : string = '';
begin
  result := 0;
  STATE_HANDLER_MAIN_MENU.doAction;
  while globalGameState.currentState <> nil do begin
    CLIGenericStateHandlerClass(globalGameState.currentState).display;
    display;
    Write('>> ');
    ReadLn(text);
    CLIGenericStateHandlerClass(globalGameState.currentState).processInput(text);
  end;
end;

end.

