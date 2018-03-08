unit cliGenericStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliObjectReference,
  cliWriter,
  gameState,
  cliSimpleAction;

type
  CLIGenericStateHandlerClass = class
    protected
      mInputText      : TStringList;
      mCommandTable   : CLIObjectReferenceClass;
      mParentRef      : CLIGenericStateHandlerClass;
    public
      constructor Create(const parent : CLIGenericStateHandlerClass);
      destructor Destroy; override;


      procedure processInput(const text : string);
      procedure showHelp;

      procedure confirm virtual;
      procedure cancel virtual;

      procedure doAction virtual;
      procedure display virtual;
  end;

implementation

const
  HELP_STR = 'options,help';

constructor CLIGenericStateHandlerClass.Create(const parent : CLIGenericStateHandlerClass);
begin
  inherited Create;
  mInputText := TStringList.Create;
  mInputText.Delimiter := ' ';
  mCommandTable := CLIObjectReferenceClass.Create;
  mCommandTable.add(HELP_STR, SIMPLE_ACTION_HELP);
  mParentRef := parent;
end;

destructor CLIGenericStateHandlerClass.Destroy;
begin
  FreeAndNil(mInputText);
  FreeAndNil(mCommandTable);
  inherited Destroy;
end;

procedure CLIGenericStateHandlerClass.processInput(const text : string);
var
  ref : TObject = nil;
begin
  mInputText.Clear;
  mInputText.DelimitedText := text;
  if mInputText.Count > 0 then begin
    ref := mCommandTable.getUnique(mInputText.Strings[0]);
    if ref = nil then begin
      globalWriter.addLine('- UNKNOWN COMMAND -');
      globalWriter.addLine('I don''t know what "' + mInputText.Strings[0] + '" means');
    end else begin
      mInputText.Delete(0);
      if ref.ClassType = CLISimpleActionClass then begin
        // perform the simple action
        case CLISimpleActionClass(ref).action of
          a_Confirm : confirm;
          a_Cancel  : cancel;
          a_Help    : showHelp;
          else begin
            globalWriter.addLine('- UNKNOWN ACTION -');
            globalWriter.addLine('I couldn''t figure out how to do the thing');
          end;
        end;
      end else begin
        // another state handler, continue down the chain
        if mInputText.Count > 0 then begin
          CLIGenericStateHandlerClass(ref).processInput(mInputText.DelimitedText);
        end else begin
          CLIGenericStateHandlerClass(ref).doAction;
        end;
      end;
    end;
  end;
end;

procedure CLIGenericStateHandlerClass.showHelp;
var
  i        : integer = 0;
  commands : TStringList = nil;
begin
  commands := TStringList.Create;
  try
    commands.Delimiter:='/';
    globalWriter.addLine('- CURRENT OPTIONS -');
    for i := 0 to mCommandTable.count - 1 do begin
      if mCommandTable.getOption(i) <> HELP_STR then begin
        commands.Clear;
        commands.CommaText := mCommandTable.getOption(i);
        globalWriter.addLine('- ' + commands.DelimitedText);
      end;
    end;
  finally
    FreeAndNil(commands);
  end;
end;

procedure CLIGenericStateHandlerClass.confirm;
begin
  globalWriter.addLine('** CLIGenericStateHandlerClass.confirm called, nothing to confirm... **');
end;

procedure CLIGenericStateHandlerClass.cancel;
begin
  if self = globalGameState.currentState then globalGameState.popState;
end;

procedure CLIGenericStateHandlerClass.doAction;
begin
  globalGameState.pushState(self);
end;

procedure CLIGenericStateHandlerClass.display;
begin
  globalWriter.addLine('** CLIGenericStateHandlerClass.display called, nothing to display... **');
end;


end.

