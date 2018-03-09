unit cliBattleTestStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliGenericStateHandler,
  cliConst,
  cliWriter;

type
  CLIBattleTestStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
      procedure doAction; override;
  end;

implementation

constructor CLIBattleTestStateHandlerClass.Create;
begin
  inherited Create;
  mStateName := 'BATTLE TEST';
  mCommandTable.add('quit', STATE_QUIT);
end;

destructor CLIBattleTestStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIBattleTestStateHandlerClass.display;
begin
  globalWriter.addLine('pen15');
end;

procedure CLIBattleTestStateHandlerClass.doAction;
begin
  inherited doAction;
  // let's set up some example bois

end;

end.

