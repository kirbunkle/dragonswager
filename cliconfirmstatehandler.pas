unit cliConfirmStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  cliGenericStateHandler;

type
  CLIConfirmStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create(const parent : CLIGenericStateHandlerClass);
      destructor Destroy; override;
      procedure doAction; override;
  end;

implementation

constructor CLIConfirmStateHandlerClass.Create(const parent : CLIGenericStateHandlerClass);
begin
  inherited Create(parent);
end;

destructor CLIConfirmStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIConfirmStateHandlerClass.doAction;
begin
  mParentRef.confirm;
end;

end.

