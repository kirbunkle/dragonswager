unit commonConstType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGameZone = (gz_unknown, gz_deck, gz_hand, gz_discard);

const
  STATE_UNKNOWN = 0;

  // simple actions
  STATE_CONFIRM     = 1;
  STATE_CANCEL      = 2;
  STATE_HELP        = 3;

  // game states
  STATE_MAINMENU    = 100;
  STATE_TESTBATTLE  = 101;
  STATE_QUIT        = 102;
  STATE_INFO        = 103;

  // interactable types
  INTERACTABLE_TYPE_HERO  = 1;
  INTERACTABLE_TYPE_ENEMY = 2;
  INTERACTABLE_TYPE_CARD  = 3;

  ALL_INTERACTABLE_TYPES = [INTERACTABLE_TYPE_HERO, INTERACTABLE_TYPE_ENEMY, INTERACTABLE_TYPE_CARD];

  // game classes
  GAME_CLASS_FIGHTER = 1;
  GAME_CLASS_THIEF   = 2;
  GAME_CLASS_WIZARD  = 3;
  GAME_CLASS_CLERIC  = 4;
  GAME_CLASS_RANGER  = 5;
  GAME_CLASS_MONK    = 6;

  // special card effects
  CARD_EFFECT_DRAW_1 = 1;

implementation

end.

