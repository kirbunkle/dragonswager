unit cliGenericStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliObjectReference,
  cliWriter,
  cliConst,
  gameState;

type
  CLIGenericStateHandlerClass = class
    protected
      mInputText      : TStringList;
      mCommandTable   : CLIObjectReferenceClass;
      mStateName      : string;
    public
      constructor Create;
      destructor Destroy; override;

      procedure processInput(const text : string);
      procedure advanceState(const state : CLIGenericStateHandlerClass);
      procedure showHelp;
      procedure displayName;
      procedure unknown;

      procedure confirm virtual;
      procedure cancel virtual;

      procedure doAction virtual;
      procedure display virtual;
  end;

implementation

uses
  cliMainMenuStateHandler,
  cliQuitStateHandler;

const
  HELP_STR = 'options,help';

constructor CLIGenericStateHandlerClass.Create;
begin
  inherited Create;
  mStateName := '';
  mInputText := TStringList.Create;
  mInputText.Delimiter := ' ';
  mCommandTable := CLIObjectReferenceClass.Create;
  mCommandTable.add(HELP_STR, STATE_HELP);
end;

destructor CLIGenericStateHandlerClass.Destroy;
begin
  FreeAndNil(mInputText);
  FreeAndNil(mCommandTable);
  inherited Destroy;
end;

procedure CLIGenericStateHandlerClass.processInput(const text : string);
var
  state : TObjectType = STATE_UNKNOWN;
begin
  mInputText.Clear;
  mInputText.DelimitedText := text;
  if mInputText.Count > 0 then begin
    state := mCommandTable.getUnique(mInputText.Strings[0]);
    case state of
      STATE_CONFIRM  : confirm;
      STATE_CANCEL   : cancel;
      STATE_HELP     : showHelp;
      STATE_MAINMENU : advanceState(CLIMainMenuStateHandlerClass.Create);
      STATE_QUIT     : advanceState(CLIQuitStateHandlerClass.Create);
      else begin
        unknown;
      end;
    end;
  end;
end;

procedure CLIGenericStateHandlerClass.advanceState(const state : CLIGenericStateHandlerClass);
begin
  mInputText.Delete(0);
  state.doAction;
  if mInputText.Count > 0 then begin
    state.processInput(mInputText.DelimitedText);
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

procedure CLIGenericStateHandlerClass.displayName;
begin
  globalWriter.addLine('*-- ' + mStateName + ' --*');
end;

procedure CLIGenericStateHandlerClass.unknown;
begin
  globalWriter.addLine('- UNKNOWN COMMAND -');
  globalWriter.addLine('I don''t know what "' + mInputText.Strings[0] + '" means');
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
  displayName;
  globalGameState.pushState(self);
end;

procedure CLIGenericStateHandlerClass.display;
begin
  globalWriter.addLine('** CLIGenericStateHandlerClass.display called, nothing to display... **');
end;


end.

