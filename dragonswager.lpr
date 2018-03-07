program dragonswager;

{$mode objfpc}{$H+}

uses
  Classes, cligame, cliGenericActionHandler, cliQuitActionHandler, gameState, cliMenuActionHandler, 
cliInfoActionHandler, cliConfirmActionHandler, cliCancelActionHandler, cliUseActionHandler;

var
  game : CLIGameClass = nil;

begin
  game := CLIGameClass.Create;
  game.run;
end.

