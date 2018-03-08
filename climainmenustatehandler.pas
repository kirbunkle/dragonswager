unit cliMainMenuStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliObjectReference,
  cliWriter,
  cliQuitStateHandler,
  cliGenericStateHandler;

type
  CLIMainMenuStateHandlerClass = class(CLIGenericStateHandlerClass)
    private
      mQuitHandler : CLIQuitStateHandlerClass;
    public
      constructor Create(const parent : CLIGenericStateHandlerClass);
      destructor Destroy; override;
      procedure display; override;
  end;

implementation

constructor CLIMainMenuStateHandlerClass.Create(const parent : CLIGenericStateHandlerClass);
begin
  inherited Create(parent);

  mQuitHandler := CLIQuitStateHandlerClass.Create(self);

  mCommandTable.add('quit', mQuitHandler);
end;

destructor CLIMainMenuStateHandlerClass.Destroy;
begin
  FreeAndNil(mQuitHandler);
  inherited Destroy;
end;

procedure CLIMainMenuStateHandlerClass.display;
begin
  globalWriter.addLine('- MAIN MENU -');
  showHelp;
end;

end.

