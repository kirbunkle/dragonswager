unit cliSimpleAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSimpleActionType = (a_Confirm, a_Cancel, a_Help);

  CLISimpleActionClass = class
    private
      mAction : TSimpleActionType;
    public
      constructor Create(const actionIn : TSimpleActionType);
      destructor Destroy; override;
      property action : TSimpleActionType read mAction write mAction;
  end;

var
  SIMPLE_ACTION_CONFIRM : CLISimpleActionClass = nil;
  SIMPLE_ACTION_CANCEL  : CLISimpleActionClass = nil;
  SIMPLE_ACTION_HELP    : CLISimpleActionClass = nil;

implementation

constructor CLISimpleActionClass.Create(const actionIn : TSimpleActionType);
begin
  inherited Create;
  mAction := actionIn;
end;

destructor CLISimpleActionClass.Destroy;
begin
  inherited Destroy;
end;

initialization
  SIMPLE_ACTION_CONFIRM := CLISimpleActionClass.Create(a_Confirm);
  SIMPLE_ACTION_CANCEL := CLISimpleActionClass.Create(a_Cancel);
  SIMPLE_ACTION_HELP := CLISimpleActionClass.Create(a_Help);

finalization
  FreeAndNil(SIMPLE_ACTION_CONFIRM);
  FreeAndNil(SIMPLE_ACTION_CANCEL);
  FreeAndNil(SIMPLE_ACTION_HELP);

end.

