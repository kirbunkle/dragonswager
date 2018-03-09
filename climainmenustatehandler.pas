unit cliMainMenuStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliGenericStateHandler;

type
  CLIMainMenuStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
  end;

implementation

constructor CLIMainMenuStateHandlerClass.Create;
begin
  inherited Create;
  mStateName := 'MAIN MENU';
  mCommandTable.add('battle,test', STATE_TESTBATTLE);
  mCommandTable.add('quit', STATE_QUIT);
end;

destructor CLIMainMenuStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIMainMenuStateHandlerClass.display;
begin
  inherited display;
  showHelp;
end;

end.

