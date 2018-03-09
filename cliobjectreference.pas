unit cliObjectReference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cliConst, fgl;

type
  TObjectType = word;

  TStringWordMap = specialize TFPGMap<String, word>;

  CLIObjectReferenceClass = class
    private
      mMap : TStringWordMap;

      function findUnique(const nameIn : string) : integer;
      function findFirst(const nameIn : string) : integer;
      function find(const objectTypeIn : TObjectType) : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure add(const namesIn : string; const objectTypeIn : TObjectType);
      procedure add(const namesIn : TStringList; const objectTypeIn : TObjectType);
      function getUnique(const nameIn : string) : TObjectType;
      function getFirst(const nameIn : string) : TObjectType;
      procedure removeAll(const nameIn : string);
      procedure remove(const objectTypeIn : TObjectType);
      function contains(const objectTypeIn : TObjectType) : boolean;
      function count : integer;
      function getOption(const index : integer) : string;
  end;

implementation

function CLIObjectReferenceClass.findUnique(const nameIn : string) : integer;
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
    if (loc = 1)
    or ((loc > 1) and (mMap.Keys[i][loc-1] = ',')) then begin
      if foundIndex = -1 then begin
        foundIndex := i;
      end else if foundIndex <> i then begin
        exit; // couldn't find a unique result based on the name
      end;
    end;
  end;
  result := foundIndex;
end;

function CLIObjectReferenceClass.findFirst(const nameIn : string) : integer;
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

function CLIObjectReferenceClass.find(const objectTypeIn : TObjectType) : integer;
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

constructor CLIObjectReferenceClass.Create;
begin
  inherited Create;
  mMap := TStringWordMap.Create;
end;

destructor CLIObjectReferenceClass.Destroy;
begin
  FreeAndNil(mMap);
  inherited Destroy;
end;

procedure CLIObjectReferenceClass.add(
  const namesIn : string;
  const objectTypeIn : TObjectType);
var
  names : string = '';
begin
  names := LowerCase(namesIn);
  mMap.Add(names, objectTypeIn);
end;

procedure CLIObjectReferenceClass.add(
  const namesIn : TStringList;
  const objectTypeIn : TObjectType);
begin
  add(namesIn.CommaText, objectTypeIn);
end;

function CLIObjectReferenceClass.getUnique(
  const nameIn : string) : TObjectType;
var
  index : integer = -1;
begin
  result := STATE_UNKNOWN;
  index := findUnique(nameIn);
  if index >= 0 then result := mMap.Data[index];
end;

function CLIObjectReferenceClass.getFirst(
  const nameIn : string) : TObjectType;
var
  index : integer = -1;
begin
  result := STATE_UNKNOWN;
  index := findFirst(nameIn);
  if index >= 0 then result := mMap.Data[index];
end;

procedure CLIObjectReferenceClass.removeAll(const nameIn : string);
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

procedure CLIObjectReferenceClass.remove(const objectTypeIn : TObjectType);
var
  index : integer = -1;
begin
  index := find(objectTypeIn);
  if index >= 0 then mMap.Delete(index);
end;

function CLIObjectReferenceClass.contains(const objectTypeIn : TObjectType) : boolean;
begin
  result := find(objectTypeIn) >= 0;
end;

function CLIObjectReferenceClass.count : integer;
begin
  result := mMap.Count;
end;

function CLIObjectReferenceClass.getOption(const index : integer) : string;
begin
  result := mMap.Keys[index];
end;

end.

