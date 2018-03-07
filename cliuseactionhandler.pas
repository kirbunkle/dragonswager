unit cliUseActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliGenericActionHandler, gameState, cliWriter;

type
  CLIUseActionHandlerClass = class(CLIGenericActionHandlerClass)
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) override;
  end;

implementation

constructor CLIUseActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create(writerIn);
end;

destructor CLIUseActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIUseActionHandlerClass.doAction(const text : string);
begin
  mWriter.addLine('- USE -');
  mWriter.addLine('Use the binch force, binch');
end;

end.

