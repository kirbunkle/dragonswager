unit cliConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

implementation

end.

