unit inputAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseObject, utils, cliWriter;

type
  InputActionClass = class
    private
      mType           : TInputActionType;
      mUser           : BaseObjectClass;
      mTarget         : BaseObjectClass;
      mItem           : BaseObjectClass;

    public
      constructor Create;
      destructor Destroy; override;
      procedure clear;
      property actionType : TInputActionType read mType write mType;
      property user : BaseObjectClass read mUser write mUser;
      property target : BaseObjectClass read mTarget write mTarget;
      property item : BaseObjectClass read mItem write mItem;
      procedure setAction(
        const actionTypeIn : TInputActionType;
        const itemIn       : BaseObjectClass = nil;
        const userIn       : BaseObjectClass = nil;
        const targetIn     : BaseObjectClass = nil);
  end;

implementation

constructor InputActionClass.Create;
begin
  inherited Create;
  clear;
end;

destructor InputActionClass.Destroy;
begin
  inherited Destroy;
end;

procedure InputActionClass.clear;
begin
  mType := ia_unknown;
  mUser := nil;
  mTarget := nil;
  mItem := nil;
end;

procedure InputActionClass.setAction(
  const actionTypeIn : TInputActionType;
  const itemIn       : BaseObjectClass = nil;
  const userIn       : BaseObjectClass = nil;
  const targetIn     : BaseObjectClass = nil);
begin
  mType   := actionTypeIn;
  mItem   := itemIn;
  mUser   := userIn;
  mTarget := targetIn;
end;

end.

