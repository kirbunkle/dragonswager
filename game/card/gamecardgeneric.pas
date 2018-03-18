unit gameCardGeneric;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacter, fgl, commonConstType;

type
  TGameClassList = specialize TFPGList<TGameClass>;
  TTargetList = specialize TFPGList<TInteractableType>;

  GameCardGenericClass = class
    protected
      mName            : string;
      mDesc            : string;
      mTarget          : GameCharacterClass; // TODO: could be not a character
      mUser            : GameCharacterClass;
      mZone            : TGameZone;
      mPossibleClasses : TGameClassList;
      mPossibleTargets : TTargetList;


    public
      constructor Create;
      destructor Destroy; override;
      property target : GameCharacterClass read mTarget write mTarget;
      property user : GameCharacterClass read mUser write mUser;
      property zone : TGameZone read mZone write mZone;
      procedure use; virtual abstract;
  end;

implementation

constructor GameCardGenericClass.Create;
begin
  inherited Create;
  mPossibleClasses := TGameClassList.Create;
  mPossibleTargets := TTargetList.Create;
  mTarget := nil;
  mUser := nil;
  mName := '';
  mDesc := '';
  mZone := gz_unknown;
end;

destructor GameCardGenericClass.Destroy;
begin
  FreeAndNil(mPossibleClasses);
  FreeAndNil(mPossibleTargets);
  inherited Destroy;
end;

end.

