unit cliQuitActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliGenericActionHandler, gameState, cliWriter;

type
  CLIQuitActionHandlerClass = class(CLIGenericActionHandlerClass)
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) override;
  end;

implementation

constructor CLIQuitActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create(writerIn);
end;

destructor CLIQuitActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIQuitActionHandlerClass.doAction(const text : string);
begin
  gs_Running := false;
  mWriter.addLine('- QUIT -');
  mWriter.addLine('Bye.');
end;

end.

