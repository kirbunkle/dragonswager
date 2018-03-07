unit cliObjectReference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  CLIObjectReferenceClass = class
    private
      mMap     : TStringList;

      function findUnique(const name : string) : integer;
      function findFirst(const name : string) : integer;
      function find(const objectIn : TObject) : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure add(const names : string; const objectIn : TObject);
      procedure add(const names : TStringList; const objectIn : TObject);
      function getUnique(const name : string) : TObject;
      function getFirst(const name : string) : TObject;
      procedure removeAll(const name : string);
      procedure remove(const objectIn : TObject);
      function contains(const objectIn : TObject) : boolean;
  end;

implementation

function CLIObjectReferenceClass.findUnique(const name : string) : integer;
var
  i          : integer = 0;
  loc        : integer = 0;
  foundIndex : integer = -1;
begin
  result := -1;
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

function CLIObjectReferenceClass.findFirst(const name : string) : integer;
var
  i          : integer = 0;
  loc        : integer = 0;
begin
  result := -1;
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
  const names : string;
  const objectIn : TObject);
begin
  mMap.AddObject(names, objectIn);
end;

procedure CLIObjectReferenceClass.add(
  const names : TStringList;
  const objectIn : TObject);
begin
  add(names.CommaText, objectIn);
end;

function CLIObjectReferenceClass.getUnique(
  const name : string) : TObject;
var
  index : integer = -1;
begin
  result := nil;
  index := findUnique(name);
  if index >= 0 then result := mMap.Objects[index];
end;

function CLIObjectReferenceClass.getFirst(
  const name : string) : TObject;
var
  index : integer = -1;
begin
  result := nil;
  index := findFirst(name);
  if index >= 0 then result := mMap.Objects[index];
end;

procedure CLIObjectReferenceClass.removeAll(const name : string);
var
  index : integer = -1;
begin
  while true do begin
    index := findFirst(name);
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

end.

