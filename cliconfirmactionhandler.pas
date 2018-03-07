unit cliConfirmActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliGenericActionHandler, gameState, cliWriter;

type
  CLIConfirmActionHandlerClass = class(CLIGenericActionHandlerClass)
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) override;
  end;

implementation

constructor CLIConfirmActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create(writerIn);
end;

destructor CLIConfirmActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIConfirmActionHandlerClass.doAction(const text : string);
begin
  mWriter.addLine('- CONFIRM -');
  mWriter.addLine('That sure, huh?');
end;

end.

