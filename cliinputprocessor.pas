unit cliInputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utils, inputAction, cliObjectReference;

type
  CLIInputProcessorClass = class
    private
      mInputText    : TStringList;
      mCommandTable : CLIObjectReferenceClass;

      procedure getInput;
      function parseTextToAction(const index : integer) : TInputActionType;
    public
      constructor Create;
      destructor Destroy; override;
      procedure processInput(var inputAction : InputActionClass);
  end;

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
  ref        : TObject = nil;
begin
  result := IA_UNKNOWN;
  ref := mCommandTable.getUnique(mInputText.Strings[index]);
  if ref <> nil then result := TInputActionType(ref);
end;

constructor CLIInputProcessorClass.Create;
begin
  inherited Create;
  mInputText := TStringList.Create;
  mCommandTable := CLIObjectReferenceClass.Create;

  // set up commands
  mCommandTable.add('quit', TObject(IA_QUIT));
  mCommandTable.add('yes', TObject(IA_CONFIRM));
  mCommandTable.add('no,back', TObject(IA_CANCEL));
  mCommandTable.add('menu', TObject(IA_MENU));
  mCommandTable.add('info', TObject(IA_INFO));
  mCommandTable.add('use,cast,play', TObject(IA_USE));
end;

destructor CLIInputProcessorClass.Destroy;
begin
  FreeAndNil(mInputText);
  inherited Destroy;
end;

procedure CLIInputProcessorClass.processInput(
  var inputAction : InputActionClass);
var
  action : TInputActionType = ia_unknown;
begin
  inputAction.clear;
  getInput;

  action := parseTextToAction(0);

  inputAction.actionType := action;
end;

end.

