unit cliQuitStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliWriter,
  cliGenericStateHandler,
  gameState;

type
  CLIQuitStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure display override;
      procedure confirm override;
  end;

implementation

constructor CLIQuitStateHandlerClass.Create;
begin
  inherited Create;
  mStateName := 'QUIT';
  mCommandTable.add('yes', STATE_CONFIRM);
  mCommandTable.add('no,back', STATE_CANCEL);
end;

destructor CLIQuitStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIQuitStateHandlerClass.display;
begin
  inherited display;
  globalWriter.addLine('Are you sure you want to quit?');
end;

procedure CLIQuitStateHandlerClass.confirm;
begin
  globalGameState.popState;
  globalGameState.popState;
end;

end.

