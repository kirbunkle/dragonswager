unit cliWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  CLIWriterClass = class
    private
      mText    : TStringList;
      newBlock : boolean;

    public
      constructor Create;
      destructor Destroy; override;
      procedure addLine(const s : string);
      function displayNext : boolean;
  end;

var
  globalWriter : CLIWriterClass = nil;

implementation

constructor CLIWriterClass.Create;
begin
  inherited Create;
  mText := TStringList.Create;
  newBlock := true;
end;

destructor CLIWriterClass.Destroy;
begin
  if Assigned(mText) then FreeAndNil(mText);
  inherited Destroy;
end;

procedure CLIWriterClass.addLine(const s : string);
begin
  mText.Add(s);
end;

function CLIWriterClass.displayNext : boolean;
begin
  result := false;
  if mText.Count <= 0 then exit;
  if newBlock then begin
    WriteLn;
    newBlock := false;
  end;
  WriteLn(mText.Strings[0]);
  mText.Delete(0);
  if mText.Count > 0 then begin
    result := true;
  end else begin
    newBlock := true;
  end;
end;

end.

