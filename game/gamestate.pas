unit gameState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameEnemyGroup;

type
  GameStateClass = class
    private
      mStateStack : TList;
      mCount      : integer;
      mEnemyGroup : GameEnemyGroupClass;

      procedure cleanUp;
    public
      constructor Create;
      destructor Destroy; override;
      procedure pushState(const stateHandler : TObject);
      procedure popState;
      function currentState : TObject;
      procedure clearStates;
      procedure setupEnemyGroup;
      property enemyGroup : GameEnemyGroupClass read mEnemyGroup;
    end;

var
  globalGameState : GameStateClass = nil;

implementation

procedure GameStateClass.cleanUp;
var
  i   : integer = 0;
  ref : TObject = nil;
begin
  while mStateStack.Count > mCount do begin
    i := mStateStack.Count - 1;
    ref := TObject(mStateStack.Items[i]);
    FreeAndNil(ref);
    mStateStack.Delete(i);
  end;
end;

constructor GameStateClass.Create;
begin
  inherited Create;
  mStateStack := TList.Create;
  mCount := 0;
  mEnemyGroup := nil;
end;

destructor GameStateClass.Destroy;
begin
  if mEnemyGroup <> nil then FreeAndNil(mEnemyGroup);
  clearStates;
  FreeAndNil(mStateStack);
  inherited Destroy;
end;

procedure GameStateClass.pushState(const stateHandler : TObject);
begin
  cleanUp;
  mStateStack.Add(stateHandler);
  Inc(mCount);
end;

procedure GameStateClass.popState;
begin
  if mCount > 0 then Dec(mCount);
end;

function GameStateClass.currentState : TObject;
begin
  result := nil;
  if mCount > 0 then result := TObject(mStateStack.Items[mCount-1]);
end;

procedure GameStateClass.clearStates;
begin
  mCount := 0;
  cleanUp;
end;

procedure GameStateClass.setupEnemyGroup;
begin
  if mEnemyGroup <> nil then FreeAndNil(mEnemyGroup);
  mEnemyGroup := GameEnemyGroupClass.Create;
end;

end.

