unit cliController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliWriter, cliInputProcessor;

type
  CLIControllerClass = class
    private
      mWriter         : CLIWriterClass;
      mInputProcessor : CLIInputProcessorClass;

    public
      constructor Create(writer : CLIWriterClass);
      destructor Destroy; override;
      procedure start;
      procedure next;
  end;

implementation

constructor CLIControllerClass.Create(
  writer : CLIWriterClass);
begin
  inherited Create;
  mWriter := writer;
  mInputProcessor := CLIInputProcessorClass.Create(mWriter);
end;

destructor CLIControllerClass.Destroy;
begin
  FreeAndNil(mInputProcessor);
  inherited Destroy;
end;

procedure CLIControllerClass.start;
begin
  mWriter.addLine('Yo, it''s time to do the thing, you ready to do the thing?');
end;

procedure CLIControllerClass.next;
begin
  mInputProcessor.processInput;
end;

end.

