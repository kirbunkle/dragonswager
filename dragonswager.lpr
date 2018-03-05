program dragonswager;

{$mode objfpc}{$H+}

uses
  Classes, cligame;

var
  game : CLIGameClass = nil;

begin
  game := CLIGameClass.Create;
  game.run;
end.

