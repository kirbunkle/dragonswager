unit gameDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  GameDatabaseClass = class
    private
      characters : TStringList;

    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

constructor GameDatabaseClass.Create;
begin
  inherited Create;
  characters := TStringList.Create;
  characters.LoadFromFile('characters.db');
end;

destructor GameDatabaseClass.Destroy;
begin
  inherited Destroy;
end;

end.

