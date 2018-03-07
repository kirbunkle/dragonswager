unit cliGenericActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliWriter;

type
  CLIGenericActionHandlerClass = class
    protected
      mWriter : CLIWriterClass;
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) virtual abstract;
  end;

implementation

constructor CLIGenericActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create;
  mWriter := writerIn;
end;

destructor CLIGenericActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

end.

