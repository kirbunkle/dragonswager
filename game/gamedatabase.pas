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

var
  globalDatabase : GameDatabaseClass = nil;

implementation

constructor GameDatabaseClass.Create;
begin
  inherited Create;
  characters := TStringList.Create;
  characters.LoadFromFile('db/characters.db');
end;

destructor GameDatabaseClass.Destroy;
begin
  inherited Destroy;
end;

end.
