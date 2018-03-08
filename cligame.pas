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
      mMainMenuHandler : CLIMainMenuStateHandlerClass;
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
  globalGameState := GameStateClass.Create;
  globalWriter := CLIWriterClass.Create;
  mMainMenuHandler := CLIMainMenuStateHandlerClass.Create(nil);
  globalGameState.pushState(mMainMenuHandler);
end;

destructor CLIGameClass.Destroy;
begin
  FreeAndNil(mMainMenuHandler);
  FreeAndNil(globalWriter);
  FreeAndNil(globalGameState);
  inherited Destroy;
end;

function CLIGameClass.run : integer;
var
  text : string = '';
begin
  result := 0;
  while globalGameState.running do begin
    CLIGenericStateHandlerClass(globalGameState.currentState).display;
    display;
    Write('>> ');
    ReadLn(text);
    CLIGenericStateHandlerClass(globalGameState.currentState).processInput(text);
  end;
end;

end.

