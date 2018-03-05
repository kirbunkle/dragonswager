unit baseObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  BaseObjectClass = class
    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

constructor BaseObjectClass.Create;
begin
  inherited Create;
end;

destructor BaseObjectClass.Destroy;
begin
  inherited Destroy;
end;

end.

