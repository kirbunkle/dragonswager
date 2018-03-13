unit cliGenericStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliCommandReference,
  cliWriter,
  gameState;

const
  STATE_UNKNOWN = 0;

  // simple actions
  STATE_CONFIRM     = 1;
  STATE_CANCEL      = 2;
  STATE_HELP        = 3;

  // game states
  STATE_MAINMENU    = 100;
  STATE_TESTBATTLE  = 101;
  STATE_QUIT        = 102;
  STATE_INFO        = 103;

type
  CLIGenericStateHandlerClass = class
    protected
      mInputText      : TStringList;
      mCommandTable   : CLICommandReferenceClass;
      mStateName      : string;
      mAnnounced      : boolean;

    public
      constructor Create;
      destructor Destroy; override;

      procedure processInput(const text : string);
      procedure advanceState(const state : CLIGenericStateHandlerClass);
      procedure announce;
      procedure showHelp virtual;
      procedure displayName;
      procedure unknown;

      procedure confirm virtual;
      procedure cancel virtual;
      procedure doAction virtual;
      procedure display virtual;
      function selectTarget(const reference : string) : boolean virtual;

  end;

implementation

uses
  cliMainMenuStateHandler,
  cliQuitStateHandler,
  cliBattleTestStateHandler,
  cliInfoStateHandler;

const
  HELP_STR = 'options,help';

constructor CLIGenericStateHandlerClass.Create;
begin
  inherited Create;
  mStateName := '';
  mInputText := TStringList.Create;
  mInputText.Delimiter := ' ';
  mCommandTable := CLICommandReferenceClass.Create;
  mCommandTable.add(HELP_STR, STATE_HELP);
  mAnnounced := false;
  globalGameState.pushState(self);
end;

destructor CLIGenericStateHandlerClass.Destroy;
begin
  FreeAndNil(mInputText);
  FreeAndNil(mCommandTable);
  inherited Destroy;
end;

procedure CLIGenericStateHandlerClass.processInput(const text : string);
var
  command : TCommandType = STATE_UNKNOWN;
begin
  mInputText.Clear;
  mInputText.DelimitedText := text;
  if mInputText.Count > 0 then begin
    command := mCommandTable.getUnique(mInputText.Strings[0]);
    case command of
      STATE_CONFIRM    : confirm;
      STATE_CANCEL     : cancel;
      STATE_HELP       : showHelp;
      STATE_MAINMENU   : advanceState(CLIMainMenuStateHandlerClass.Create);
      STATE_QUIT       : advanceState(CLIQuitStateHandlerClass.Create);
      STATE_TESTBATTLE : advanceState(CLIBattleTestStateHandlerClass.Create);
      STATE_INFO       : advanceState(CLIInfoStateHandlerClass.Create);
      else begin
        if not selectTarget(mInputText.Strings[0]) then unknown;
      end;
    end;
  end;
end;

procedure CLIGenericStateHandlerClass.advanceState(const state : CLIGenericStateHandlerClass);
begin
  mAnnounced := false;
  mInputText.Delete(0);
  state.doAction;
  if mInputText.Count > 0 then begin
    state.processInput(mInputText.DelimitedText);
  end;
end;

procedure CLIGenericStateHandlerClass.announce;
begin
  if not mAnnounced then begin
    mAnnounced := true;
    displayName;
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
  globalWriter.addLine('The command "' + mInputText.Strings[0] + '" doesn''t do anything here. (try "help" for options)');
end;

procedure CLIGenericStateHandlerClass.confirm;
begin
  globalWriter.addLine('** CLIGenericStateHandlerClass.confirm called, nothing to confirm... **');
end;

procedure CLIGenericStateHandlerClass.cancel;
begin
  globalGameState.popState;
end;

procedure CLIGenericStateHandlerClass.doAction;
begin
  // by default do nothing
end;

procedure CLIGenericStateHandlerClass.display;
begin
  announce;
end;

function CLIGenericStateHandlerClass.selectTarget(const reference : string) : boolean;
begin
  result := false;
end;


end.

