unit cliInputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utils, inputAction;

type
  CLIInputProcessorClass = class
    private
      mInputText : TStringList;

      procedure getInput;
      function parseTextToAction(const index : integer) : TInputActionType;
    public
      constructor Create;
      destructor Destroy; override;
      procedure processInput(var inputAction : InputActionClass);
  end;

  textCommand = record
    text    : string;
    command : TInputActionType;
  end;

const
  TEXT_COMMAND_COUNT = 9;

  TEXT_TO_COMMAND_TABLE : array[0..TEXT_COMMAND_COUNT-1] of textCommand =
  ( (text : 'quit';   command : ia_quit)
  , (text : 'yes';    command : ia_confirm)
  , (text : 'no';     command : ia_cancel)
  , (text : 'back';   command : ia_cancel)
  , (text : 'menu';   command : ia_menu)
  , (text : 'info';   command : ia_info)
  , (text : 'use';    command : ia_use)
  , (text : 'cast';   command : ia_use)
  , (text : 'play';   command : ia_use)
  );

implementation

procedure CLIInputProcessorClass.getInput;
var
  text : string = '';
begin
  mInputText.Clear;
  Write('>> ');
  ReadLn(text);
  split(text, mInputText);
end;

function CLIInputProcessorClass.parseTextToAction(
  const index : integer) : TInputActionType;
var
  myText     : string = '';
  i          : integer = 0;
  prevAction : TInputActionType = ia_unknown;
begin
  result := ia_unknown;
  myText := LowerCase(mInputText.Strings[index]);
  for i := 0 to TEXT_COMMAND_COUNT - 1 do begin
    if Pos(myText, TEXT_TO_COMMAND_TABLE[i].text) = 1 then begin
      if prevAction = ia_unknown then begin
        prevAction := TEXT_TO_COMMAND_TABLE[i].command;
      end else if prevAction <> TEXT_TO_COMMAND_TABLE[i].command then begin
        // multiple matches, didn't find a valid exclusive one
        exit;
      end;
    end;
  end;
  result := prevAction;
end;

constructor CLIInputProcessorClass.Create;
begin
  inherited Create;
  mInputText := TStringList.Create;
end;

destructor CLIInputProcessorClass.Destroy;
begin
  FreeAndNil(mInputText);
  inherited Destroy
end;

procedure CLIInputProcessorClass.processInput(
  var inputAction : InputActionClass);
var
  i      : integer = 0;
  action : TInputActionType = ia_unknown;
begin
  inputAction.clear;
  getInput;

  for i := 0 to mInputText.Count - 1 do begin
    action := parseTextToAction(i);
    if action <> ia_unknown then break;
  end;

  inputAction.actionType := action;
end;

end.

