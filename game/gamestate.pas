unit gameState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gameCharacterGroup, gameDeck, commonConstType;

type
  GameStateClass = class
    private
      mStateStack  : TList;
      mCount       : integer;
      mEnemyGroup  : GameCharacterGroupClass;
      mHeroGroup   : GameCharacterGroupClass;
      mCardZones   : GameDeckClass;

      procedure cleanUp;
    public
      constructor Create;
      destructor Destroy; override;
      procedure pushState(const stateHandler : TObject);
      procedure popState;
      function currentState : TObject;
      procedure clearStates;
      procedure setupEnemyGroup;
      procedure setupHeroGroup;
      procedure setupCards;
      property enemyGroup : GameCharacterGroupClass read mEnemyGroup;
      property heroGroup : GameCharacterGroupClass read mHeroGroup;
      property cardZones : GameDeckClass read mCardZones;
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
  mCardZones := GameDeckClass.Create;
  mCount := 0;
  mEnemyGroup := nil;
  mHeroGroup := nil;
end;

destructor GameStateClass.Destroy;
begin
  if mEnemyGroup <> nil then FreeAndNil(mEnemyGroup);
  if mHeroGroup <> nil then FreeAndNil(mHeroGroup);
  clearStates;
  FreeAndNil(mCardZones);
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
  mEnemyGroup := GameCharacterGroupClass.Create('1,1,1,1');
end;

procedure GameStateClass.setupHeroGroup;
begin
  if mHeroGroup <> nil then FreeAndNil(mHeroGroup);
  mHeroGroup := GameCharacterGroupClass.Create('2,3,4,5');
end;

procedure GameStateClass.setupCards;
begin
  if mCardZones <> nil then FreeAndNil(mCardZones);
  mCardZones := GameDeckClass.Create;
  mCardZones.add(gz_deck, '1,1,2,2,3,3');
end;

end.

