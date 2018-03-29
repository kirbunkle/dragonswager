unit cliTableWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliWriter, strutils;

const
  MAX_LINE_WIDTH = 80;

type
  CLITableWriterClass = class
    private
      mLines : TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure addElement(
        const leftJustified : string;
        const rightJustified : string = '');
      procedure display;
  end;

implementation

constructor CLITableWriterClass.Create;
begin
  inherited Create;
  mLines := TStringList.Create;
end;

destructor CLITableWriterClass.Destroy;
begin
  FreeAndNil(mLines);
  inherited Destroy;
end;

procedure CLITableWriterClass.addElement(
  const leftJustified : string;
  const rightJustified : string = '');
begin
  mLines.Add(leftJustified+'='+rightJustified);
end;

procedure CLITableWriterClass.display;
var
  i            : integer = 0;
  j            : integer = 0;
  k            : integer = 0;
  left         : string = '';
  right        : string = '';
  linesOut     : TStringList = nil;
  maxLen       : integer = 0;
  curLen       : integer = 0;
  horizLine    : string = '';
  vertLine     : string = '';
  line         : string = '';
  rLen         : integer = 0;
  maxLineWidth : integer = 0;
  colCount     : integer = 1;
  linesToWrite : TStringList = nil;
begin
  if mLines.Count > 0 then begin;
    linesOut := TStringList.Create;
    linesToWrite := TStringList.Create;
    try
      linesOut.AddStrings(mLines);
      maxLineWidth := MAX_LINE_WIDTH;
      while true do begin
      // adjust widths
        for i := 0 to linesOut.Count - 1 do begin
          right := linesOut.ValueFromIndex[i];
          rLen := Length(right);
          if rLen <> 0 then Inc(rLen); // we need an extra space for this if it exists
          curLen := Length(linesOut.Names[i]) + rLen + 7;
          if curLen > maxLineWidth then begin
            left := LeftStr(linesOut.Names[i], maxLineWidth - (rLen + 10));
            left := left + '...';
            linesOut.Strings[i] := left + '=' + right;
            maxLen := maxLineWidth;
          end else begin
            if maxLen < curLen then maxLen := curLen;
          end;
        end;
        if maxLen < (maxLineWidth div 2) then begin
          maxLineWidth := maxLineWidth div 2;
          colCount := colCount * 2;
        end else begin
          break;
        end;
      end;

      // build rows
      horizLine := AddCharR('-', '', maxLen);
      vertLine := AddCharR(' ', '|', maxLen - 1) + '|';
      i := 0;
      while i < linesOut.Count do begin
        j := 0;
        linesToWrite.Clear;
        while j < colCount do begin
          left := linesOut.Names[i + j];
          right := linesOut.ValueFromIndex[i + j];
          rLen := Length(right);
          if rLen <> 0 then Inc(rLen); // we need an extra space for this if it exists

          line := '| ' + left;
          line := AddCharR(' ', line, maxLen - (rLen + 4));
          if Length(right) > 0 then line := line + '-- ' + right + ' ';
          line := line + '|';

          if j = 0 then begin
            linesToWrite.Add(horizLine);
            linesToWrite.Add(vertLine);
            linesToWrite.Add(line);
            linesToWrite.Add(vertLine);
          end else begin
            linesToWrite.Strings[0] := linesToWrite.Strings[0] + horizLine;
            linesToWrite.Strings[1] := linesToWrite.Strings[1] + vertLine;
            linesToWrite.Strings[2] := linesToWrite.Strings[2] + line;
            linesToWrite.Strings[3] := linesToWrite.Strings[3] + vertLine;
          end;
          Inc(j);
        end;
        for k := 0 to linesToWrite.Count - 1 do begin
          globalWriter.addLine(linesToWrite.Strings[k]);
        end;
        Inc(i, j);
      end;
      line := '';
      for i := 0 to colCount - 1 do begin
        line := line + horizLine;
      end;
      globalWriter.addLine(line);

    finally
      FreeAndNil(linesOut);
      FreeAndNil(linesToWrite);
    end;
  end;
end;

end.

