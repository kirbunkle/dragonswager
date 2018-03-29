unit cliGenericGameStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliGenericStateHandler,
  commonConstType,
  gameState;

type
  CLIGenericGameStateHandlerClass = class(CLIGenericStateHandlerClass)
  public
    constructor Create;
    destructor Destroy; override;
    procedure display; override;
end;

implementation

constructor CLIGenericGameStateHandlerClass.Create;
begin
  inherited Create;
  mCommandTable.add('info', STATE_INFO);
  mCommandTable.add('quit', STATE_QUIT);
  // probably add save/quit stuff here
end;

destructor CLIGenericGameStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIGenericGameStateHandlerClass.display;
begin
  inherited display;
  globalGameState.cardZones.cleanup;
end;

end.

