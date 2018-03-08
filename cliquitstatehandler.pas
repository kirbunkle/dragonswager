unit cliQuitStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sysutils,
  cliObjectReference,
  cliWriter,
  cliGenericStateHandler,
  cliConfirmStateHandler,
  cliCancelStateHandler,
  gameState;

type
  CLIQuitStateHandlerClass = class(CLIGenericStateHandlerClass)
    private
      mConfirmHandler : CLIConfirmStateHandlerClass;
      mCancelHandler  : CLICancelStateHandlerClass;
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

  mConfirmHandler := CLIConfirmStateHandlerClass.Create(self);
  mCancelHandler := CLICancelStateHandlerCLass.Create(self);

  mCommandTable.add('yes', mConfirmHandler);
  mCommandTable.add('no,back', mCancelHandler);
end;

destructor CLIQuitStateHandlerClass.Destroy;
begin
  FreeAndNil(mConfirmHandler);
  FreeAndNil(mCancelHandler);
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

