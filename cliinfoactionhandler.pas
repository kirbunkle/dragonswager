unit cliInfoActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliGenericActionHandler, gameState, cliWriter;

type
  CLIInfoActionHandlerClass = class(CLIGenericActionHandlerClass)
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) override;
  end;

implementation

constructor CLIInfoActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create(writerIn);
end;

destructor CLIInfoActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIInfoActionHandlerClass.doAction(const text : string);
begin
  mWriter.addLine('- INFO -');
  mWriter.addLine('Here''s some info for you. Benus.');
end;

end.

