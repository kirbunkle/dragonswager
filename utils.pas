unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TInputActionType =
    (ia_unknown
    ,ia_quit
    ,ia_menu
    ,ia_use
    ,ia_info
    ,ia_confirm
    ,ia_cancel
  );

procedure split(
  const stringToSplit : string;
  var   returnList    : TStringList;
  const delimiter     : string = ' ');

implementation

procedure split(
  const stringToSplit : string;
  var   returnList    : TStringList;
  const delimiter     : string = ' ');
{ helper function to turn a string seperated by delimiter into many strings.
  the delimiter is not preserved in the strings, but is deleted. }
var
  tmpStr   : string = '';
  loc      : integer = 0;
  delimLen : integer = 0;
  copyLen  : integer = 0;
  addStr   : string = '';
begin
  returnList.Clear;
  tmpStr := stringToSplit;
  delimLen := Length(delimiter);
  loc := Pos(delimiter, tmpStr);
  while loc > 0 do begin
    if loc > 1 then begin
      addStr := Copy(tmpStr, 1, loc - 1);
      returnList.Add(addStr);
    end;
    copyLen := ((Length(tmpStr) - loc) - delimLen) + 1;
    if copyLen > 0 then begin
      tmpStr := Copy(tmpStr, loc + delimLen, copyLen);
      loc := Pos(delimiter, tmpStr);
    end else begin
      tmpStr := '';
      loc := 0;
    end;
  end;
  if tmpStr <> '' then begin
    returnList.Add(tmpStr); // add the rest of the string after the previous delimiter
  end;
end;

end.
