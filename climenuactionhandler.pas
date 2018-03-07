unit cliMenuActionHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliGenericActionHandler, gameState, cliWriter;

type
  CLIMenuActionHandlerClass = class(CLIGenericActionHandlerClass)
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure doAction(const text : string) override;
  end;

implementation

constructor CLIMenuActionHandlerClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create(writerIn);
end;

destructor CLIMenuActionHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIMenuActionHandlerClass.doAction(const text : string);
begin
  mWriter.addLine('- MENU -');
  mWriter.addLine('Menu options: quit');
end;

end.

