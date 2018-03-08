unit cliObjectReference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  CLIObjectReferenceClass = class
    private
      mMap     : TStringList;

      function findUnique(const nameIn : string) : integer;
      function findFirst(const nameIn : string) : integer;
      function find(const objectIn : TObject) : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure add(const namesIn : string; const objectIn : TObject);
      procedure add(const namesIn : TStringList; const objectIn : TObject);
      function getUnique(const nameIn : string) : TObject;
      function getFirst(const nameIn : string) : TObject;
      procedure removeAll(const nameIn : string);
      procedure remove(const objectIn : TObject);
      function contains(const objectIn : TObject) : boolean;
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
    loc := Pos(name, mMap.Strings[i]);
    if (loc = 1)
    or ((loc > 1) and (mMap.Strings[i][loc-1] = ',')) then begin
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
    loc := Pos(name, mMap.Strings[i]);
    if (loc = 1)
    or ((loc > 1) and (mMap.Strings[i][loc-1] = ',')) then begin
      result := i;
      break;
    end;
  end;
end;

function CLIObjectReferenceClass.find(const objectIn : TObject) : integer;
var
  i : integer = 0;
begin
  result := -1;
  for i := 0 to mMap.Count - 1 do begin
    if objectIn.Equals(mMap.Objects[i]) then begin
      result := i;
      break;
    end;
  end;
end;

constructor CLIObjectReferenceClass.Create;
begin
  inherited Create;
  mMap := TStringList.Create;
end;

destructor CLIObjectReferenceClass.Destroy;
begin
  FreeAndNil(mMap);
  inherited Destroy;
end;

procedure CLIObjectReferenceClass.add(
  const namesIn : string;
  const objectIn : TObject);
var
  names : string = '';
begin
  names := LowerCase(namesIn);
  mMap.AddObject(names, objectIn);
end;

procedure CLIObjectReferenceClass.add(
  const namesIn : TStringList;
  const objectIn : TObject);
begin
  add(namesIn.CommaText, objectIn);
end;

function CLIObjectReferenceClass.getUnique(
  const nameIn : string) : TObject;
var
  index : integer = -1;
begin
  result := nil;
  index := findUnique(nameIn);
  if index >= 0 then result := mMap.Objects[index];
end;

function CLIObjectReferenceClass.getFirst(
  const nameIn : string) : TObject;
var
  index : integer = -1;
begin
  result := nil;
  index := findFirst(nameIn);
  if index >= 0 then result := mMap.Objects[index];
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

procedure CLIObjectReferenceClass.remove(const objectIn : TObject);
var
  index : integer = -1;
begin
  index := find(objectIn);
  if index >= 0 then mMap.Delete(index);
end;

function CLIObjectReferenceClass.contains(const objectIn : TObject) : boolean;
begin
  result := find(objectIn) >= 0;
end;

function CLIObjectReferenceClass.count : integer;
begin
  result := mMap.Count;
end;

function CLIObjectReferenceClass.getOption(const index : integer) : string;
begin
  result := mMap.Strings[index];
end;

end.

