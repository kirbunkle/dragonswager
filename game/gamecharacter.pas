unit gameCharacter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  GameCharacterClass = class
    private
      mName : string;
      mHP   : integer;

    public
      constructor Create;
      destructor Destroy; override;
      property name : string read mName write mName;
      property hp : integer read mHP write mHP;
  end;

implementation

constructor GameCharacterClass.Create;
begin
  inherited Create;
end;

destructor GameCharacterClass.Destroy;
begin
  inherited Destroy;
end;

end.

