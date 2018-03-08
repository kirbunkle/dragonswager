unit cliQuitStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliObjectReference,
  cliWriter,
  cliGenericStateHandler,
  cliSimpleAction,
  gameState;

type
  CLIQuitStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create(const parent : CLIGenericStateHandlerClass);
      destructor Destroy; override;
      procedure display override;
      procedure confirm override;
  end;

implementation

constructor CLIQuitStateHandlerClass.Create(const parent : CLIGenericStateHandlerClass);
begin
  inherited Create(parent);
  mCommandTable.add('yes', SIMPLE_ACTION_CONFIRM);
  mCommandTable.add('no,back', SIMPLE_ACTION_CANCEL);
end;

destructor CLIQuitStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIQuitStateHandlerClass.display;
begin
  globalWriter.addLine('- QUIT -');
  globalWriter.addLine('Are you sure you want to quit?');
end;

procedure CLIQuitStateHandlerClass.confirm;
begin
  globalGameState.running := false;
end;

end.

