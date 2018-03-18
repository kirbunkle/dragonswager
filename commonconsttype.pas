unit commonConstType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGameClass = (gc_warrior, gc_thief, gc_wizard, gc_cleric, gc_ranger, gc_monk);
  TGameZone = (gz_unknown, gz_deck, gz_hand, gz_discard);
  TInteractableType = (it_hero, it_enemy, it_card);

const
  ALL_INTERACTABLE_TYPES = [it_hero, it_enemy, it_card];

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

implementation

end.

