unit gameCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacter, commonConstType, gameDatabase;

type
  GameCardClass = class
    protected
      mName            : string;
      mDesc            : string;
      mTarget          : GameCharacterClass; // TODO: could be not a character
      mUser            : GameCharacterClass;
      mZone            : TGameZone;
      mPossibleClasses : TStringList;
      mPossibleTargets : TStringList;
      mDamage          : integer;
      mStatusEffects   : TStringList;
      mSpecialEffects  : TStringList;


    public
      constructor Create(const id : string);
      destructor Destroy; override;
      property name : string read mName write mName;
      property target : GameCharacterClass read mTarget write mTarget;
      property user : GameCharacterClass read mUser write mUser;
      property zone : TGameZone read mZone write mZone;
  end;

implementation

constructor GameCardClass.Create(const id : string);
var
  data : TStringList = nil;
begin
  inherited Create;

  mPossibleClasses := TStringList.Create;
  mPossibleClasses.StrictDelimiter := true;
  mPossibleClasses.Delimiter := ',';

  mStatusEffects := TStringList.Create;
  mStatusEffects.StrictDelimiter := true;
  mStatusEffects.Delimiter := ',';

  mSpecialEffects := TStringList.Create;
  mSpecialEffects.StrictDelimiter := true;
  mSpecialEffects.Delimiter := ',';

  mPossibleTargets := TStringList.Create;
  mPossibleTargets.StrictDelimiter := true;
  mPossibleTargets.Delimiter := ',';

  mTarget := nil;
  mUser := nil;
  mZone := gz_unknown;
  data := TStringList.Create;
  try
    data.StrictDelimiter := true;
    data.Delimiter := '|';
    data.DelimitedText := globalDatabase.getData(DB_TYPE_CARD, id);
    mName := data.Strings[0];
    mDesc := data.Strings[1];
    mPossibleClasses.DelimitedText := data.Strings[2];
    mDamage := StrToInt(data.Strings[3]);
    mStatusEffects.DelimitedText := data.Strings[4];
    mSpecialEffects.DelimitedText := data.Strings[5];
    mPossibleTargets.DelimitedText := data.Strings[6];
  finally
    FreeAndNil(data);
  end;
end;

destructor GameCardClass.Destroy;
begin
  FreeAndNil(mPossibleClasses);
  FreeAndNil(mPossibleTargets);
  FreeAndNil(mStatusEffects);
  FreeAndNil(mSpecialEffects);
  inherited Destroy;
end;

end.

