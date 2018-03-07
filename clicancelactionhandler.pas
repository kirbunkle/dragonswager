unit cliCancelActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliGenericActionHandler, gameState, cliWriter;

type
  CLICancelActionHandlerClass = class(CLIGenericActionHandlerClass)
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) override;
  end;

implementation

constructor CLICancelActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create(writerIn);
end;

destructor CLICancelActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLICancelActionHandlerClass.doAction(const text : string);
begin
  mWriter.addLine('- CANCEL -');
  mWriter.addLine('Whoa ok jeez');
end;

end.

