unit cligame;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliController;

type

  CLIGameClass = class
    private
      mController : CLIControllerClass;
    public
      constructor Create;
      destructor Destroy; override;
      function run : integer;
    end;

implementation

constructor CLIGameClass.Create;
begin
  inherited Create;
  mController := CLIControllerClass.Create;
end;

destructor CLIGameClass.Destroy;
begin
  inherited Destroy;
end;

function CLIGameClass.run : integer;
var
  s : string = '';
begin
  result := 0;
  mController.start;
  while mController.running do begin
    mController.next;
  end;
end;

end.

