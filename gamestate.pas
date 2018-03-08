unit gameState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  GameStateClass = class
    private
      mRunning    : boolean;
      mStateStack : TList;

    public
      constructor Create;
      destructor Destroy; override;
      property running : boolean read mRunning write mRunning;
      procedure pushState(const stateHandler : TObject);
      procedure popState;
      function currentState : TObject;
      procedure clearStates;
    end;

var
  globalGameState : GameStateClass = nil;

implementation

constructor GameStateClass.Create;
begin
  inherited Create;
  mRunning := true;
  mStateStack := TList.Create;
end;

destructor GameStateClass.Destroy;
begin
  FreeAndNil(mStateStack);
  inherited Destroy;
end;

procedure GameStateClass.pushState(const stateHandler : TObject);
begin
  mStateStack.Add(stateHandler);
end;

procedure GameStateClass.popState;
begin
  if mStateStack.Count > 0 then mStateStack.Delete(mStateStack.Count-1);
end;

function GameStateClass.currentState : TObject;
begin
  result := nil;
  if mStateStack.Count > 0 then result := TObject(mStateStack.Items[mStateStack.Count-1]);
end;

procedure GameStateClass.clearStates;
begin
  mStateStack.Clear;
end;

end.

