unit cliCancelStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  cliGenericStateHandler;

type
  CLICancelStateHandlerClass = class(CLIGenericStateHandlerClass)
    public
      constructor Create(const parent : CLIGenericStateHandlerClass);
      destructor Destroy; override;
      procedure doAction; override;
  end;

implementation

constructor CLICancelStateHandlerClass.Create(const parent : CLIGenericStateHandlerClass);
begin
  inherited Create(parent);
end;

destructor CLICancelStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLICancelStateHandlerClass.doAction;
begin
  mParentRef.cancel;
end;

end.

