unit cliController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TActionType = (a_unknown, a_quit);

  CLIControllerClass = class
    private
      mRun : boolean;

      function getInput : string;
      procedure outputMessage(s : string);
      function inputToAction(s : string) : TActionType;
    public
      constructor Create;
      destructor Destroy; override;
      procedure start;
      procedure next;
      property running : boolean read mRun;
    end;

implementation

function CLIControllerClass.getInput : string;
begin
  Write('>> ');
  ReadLn(result);
end;

procedure CLIControllerClass.outputMessage(s : string);
begin
  WriteLn;
  WriteLn(s);
end;

function CLIControllerClass.inputToAction(s : string) : TActionType;
begin
  result := a_unknown;
  if Pos('quit', s) = 1 then result := a_quit;
end;

constructor CLIControllerClass.Create;
begin
  inherited Create;
  mRun := true;
end;

destructor CLIControllerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIControllerClass.start;
begin
  outputMessage('Yo, it''s time to do the thing, you ready to do the thing?');
end;

procedure CLIControllerClass.next;
var
  input : string = '';
  action : TActionType;
begin
  input := getInput;
  action := inputToAction(input);
  case action of
    a_quit : begin
      outputMessage('Get out of there then.');
      mRun := false;
    end;
    else begin
      outputMessage('You said "' + input + '", which doesn''t make any sense to me, son');
    end;
  end;
end;

end.

