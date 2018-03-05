unit character;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseObject;

type
  CharacterClass = class(BaseObjectClass)
    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

constructor CharacterClass.Create;
begin
  inherited Create;
end;

destructor CharacterClass.Destroy;
begin
  inherited Destroy;
end;

end.

