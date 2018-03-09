unit gameCharacter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  GameCharacterClass = class
    public
      constructor Create;
      destructor Destroy; override;
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

