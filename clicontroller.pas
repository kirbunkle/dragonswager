unit cliController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliWriter, inputAction, cliInputProcessor, utils;

type
  CLIControllerClass = class
    private
      mRun            : boolean;
      mWriter         : CLIWriterClass;
      mInput          : InputActionClass;
      mInputProcessor : CLIInputProcessorClass;

    public
      constructor Create(writer : CLIWriterClass);
      destructor Destroy; override;
      procedure start;
      procedure next;
      property running : boolean read mRun;
  end;

implementation

constructor CLIControllerClass.Create(
  writer : CLIWriterClass);
begin
  inherited Create;
  mWriter := writer;
  mInput := InputActionClass.Create;
  mInputProcessor := CLIInputProcessorClass.Create;
  mRun := true;
end;

destructor CLIControllerClass.Destroy;
begin
  FreeAndNil(mInput);
  FreeAndNil(mInputProcessor);
  inherited Destroy;
end;

procedure CLIControllerClass.start;
begin
  mWriter.addLine('Yo, it''s time to do the thing, you ready to do the thing?');
end;

procedure CLIControllerClass.next;
begin
  mInputProcessor.processInput(mInput);
  case mInput.actionType of
    IA_USE : mWriter.addLine('Use the binch force, binch');
    IA_INFO : mWriter.addLine('Here''s some info for you. Benus.');
    IA_MENU : mWriter.addLine('Menu options: quit');
    IA_CANCEL : mWriter.addLine('Whoa ok jeez');
    IA_CONFIRM : mWriter.addLine('That sure, huh?');
    IA_QUIT : begin
      mWriter.addLine('Bye.');
      mRun := false;
    end
    else mWriter.addLine('I don''t know what that means');
  end;
end;

end.

