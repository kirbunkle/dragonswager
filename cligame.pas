unit cligame;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliController,
  cliWriter;

type

  CLIGameClass = class
    private
      mController : CLIControllerClass;
      mWriter     : CLIWriterClass;

      procedure display;
    public
      constructor Create;
      destructor Destroy; override;
      function run : integer;
  end;

implementation

procedure CLIGameClass.display;
begin
  while mWriter.displayNext do begin
    Sleep(500);
  end;
end;

constructor CLIGameClass.Create;
begin
  inherited Create;
  mWriter := CLIWriterClass.Create;
  mController := CLIControllerClass.Create(mWriter);
end;

destructor CLIGameClass.Destroy;
begin
  FreeAndNil(mController);
  FreeAndNil(mWriter);
  inherited Destroy;
end;

function CLIGameClass.run : integer;
begin
  result := 0;
  mController.start;
  display;
  while mController.running do begin
    mController.next;
    display;
  end;
end;

end.

