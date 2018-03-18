unit gameCardIceBall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, commonConstType, gameCardGeneric;

type
  GameCardIceBallClass = class(GameCardGenericClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure use; override;
  end;

implementation

constructor GameCardIceBallClass.Create;
begin
  inherited Create;
  mName := 'Ice Ball';
  mDesc := '"A highly volatile ball of magical cold energy. Or is it lack of energy? Probably best to not think about it too hard."';
  mPossibleClasses.Add(gc_wizard);
  mPossibleTargets.Add(it_enemy);
end;

destructor GameCardIceBallClass.Destroy;
begin
  inherited Destroy;
end;

procedure GameCardIceBallClass.use;
begin
  // deal damage to target
end;

end.

