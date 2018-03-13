unit gameCharacter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameDatabase;

type
  GameCharacterClass = class
    private
      mName  : string;
      mHP    : integer;
      mDesc  : string;
      mClass : string;

    public
      constructor Create(const id : string);
      destructor Destroy; override;
      property name : string read mName write mName;
      property hp : integer read mHP write mHP;
      property description : string read mDesc write mDesc;
  end;

implementation

constructor GameCharacterClass.Create(const id : string);
var
  data : TStringList = nil;
begin
  inherited Create;
  data := TStringList.Create;
  try
    data.StrictDelimiter := true;
    data.Delimiter := ',';
    data.CommaText := globalDatabase.getData(DB_TYPE_CHARACTER, id);
    mName       := data.Strings[0];
    mClass      := data.Strings[1];
    mHP         := StrToInt(data.Strings[2]);
    mDesc       := data.Strings[3];
  finally
    FreeAndNil(data);
  end;
end;

destructor GameCharacterClass.Destroy;
begin
  inherited Destroy;
end;

end.

