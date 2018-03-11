unit clicommandreference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TCommandType = word;

  TStringWordMap = specialize TFPGMap<String, word>;

  CLICommandReferenceClass = class
    private
      mMap : TStringWordMap;

      function findUnique(const nameIn : string) : integer;
      function findFirst(const nameIn : string) : integer;
      function find(const objectTypeIn : TCommandType) : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure add(const namesIn : string; const objectTypeIn : TCommandType);
      procedure add(const namesIn : TStringList; const objectTypeIn : TCommandType);
      function getUnique(const nameIn : string) : TCommandType;
      function getFirst(const nameIn : string) : TCommandType;
      procedure removeAll(const nameIn : string);
      procedure remove(const objectTypeIn : TCommandType);
      function contains(const objectTypeIn : TCommandType) : boolean;
      function count : integer;
      function getOption(const index : integer) : string;
  end;

implementation

uses
  cliGenericStateHandler;

function CLICommandReferenceClass.findUnique(const nameIn : string) : integer;
var
  i          : integer = 0;
  loc        : integer = 0;
  foundIndex : integer = -1;
  name       : string = '';
begin
  result := -1;
  name := LowerCase(nameIn);
  for i := 0 to mMap.Count - 1 do begin
    loc := Pos(name, mMap.Keys[i]);
    if (loc > 1) then loc := Pos(',' + name, mMap.Keys[i]);
    if loc > 0 then begin
      if foundIndex = -1 then begin
        foundIndex := i;
      end else if foundIndex <> i then begin
        exit; // couldn't find a unique result based on the name
      end;
    end;
  end;
  result := foundIndex;
end;

function CLICommandReferenceClass.findFirst(const nameIn : string) : integer;
var
  i          : integer = 0;
  loc        : integer = 0;
  name       : string = '';
begin
  result := -1;
  name := LowerCase(nameIn);
  for i := 0 to mMap.Count - 1 do begin
    loc := Pos(name, mMap.Keys[i]);
    if (loc = 1)
    or ((loc > 1) and (mMap.Keys[i][loc-1] = ',')) then begin
      result := i;
      break;
    end;
  end;
end;

function CLICommandReferenceClass.find(const objectTypeIn : TCommandType) : integer;
var
  i : integer = 0;
begin
  result := -1;
  for i := 0 to mMap.Count - 1 do begin
    if objectTypeIn = mMap.Data[i] then begin
      result := i;
      break;
    end;
  end;
end;

constructor CLICommandReferenceClass.Create;
begin
  inherited Create;
  mMap := TStringWordMap.Create;
end;

destructor CLICommandReferenceClass.Destroy;
begin
  FreeAndNil(mMap);
  inherited Destroy;
end;

procedure CLICommandReferenceClass.add(
  const namesIn : string;
  const objectTypeIn : TCommandType);
var
  names : string = '';
begin
  names := LowerCase(namesIn);
  mMap.Add(names, objectTypeIn);
end;

procedure CLICommandReferenceClass.add(
  const namesIn : TStringList;
  const objectTypeIn : TCommandType);
begin
  add(namesIn.CommaText, objectTypeIn);
end;

function CLICommandReferenceClass.getUnique(
  const nameIn : string) : TCommandType;
var
  index : integer = -1;
begin
  result := STATE_UNKNOWN;
  index := findUnique(nameIn);
  if index >= 0 then result := mMap.Data[index];
end;

function CLICommandReferenceClass.getFirst(
  const nameIn : string) : TCommandType;
var
  index : integer = -1;
begin
  result := STATE_UNKNOWN;
  index := findFirst(nameIn);
  if index >= 0 then result := mMap.Data[index];
end;

procedure CLICommandReferenceClass.removeAll(const nameIn : string);
var
  index : integer = -1;
begin
  while true do begin
    index := findFirst(nameIn);
    if index >= 0 then begin
      mMap.Delete(index);
    end else begin
      break;
    end;
  end;
end;

procedure CLICommandReferenceClass.remove(const objectTypeIn : TCommandType);
var
  index : integer = -1;
begin
  index := find(objectTypeIn);
  if index >= 0 then mMap.Delete(index);
end;

function CLICommandReferenceClass.contains(const objectTypeIn : TCommandType) : boolean;
begin
  result := find(objectTypeIn) >= 0;
end;

function CLICommandReferenceClass.count : integer;
begin
  result := mMap.Count;
end;

function CLICommandReferenceClass.getOption(const index : integer) : string;
begin
  result := mMap.Keys[index];
end;

end.

