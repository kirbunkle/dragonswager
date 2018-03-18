unit cliInfoStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliGenericStateHandler,
  gameCharacter,
  cliCommandReference,
  cliInteractableReference,
  cliGenericTargetableStateHandler,
  cliWriter,
  commonConstType;

type
  CLIInfoStateHandlerClass = class(CLIGenericTargetableStateHandlerClass)
    public
      constructor Create;
      destructor Destroy; override;
      procedure display; override;
      function selectTarget(const reference : string) : boolean override;
  end;

implementation

constructor CLIInfoStateHandlerClass.Create;
begin
  inherited Create(ALL_INTERACTABLE_TYPES);
  mStateName := 'INFO';
  mCommandTable.add('back,no', STATE_CANCEL);
end;

destructor CLIInfoStateHandlerClass.Destroy;
begin
  inherited Destroy;
end;

procedure CLIInfoStateHandlerClass.display;
begin
  inherited display;
  globalWriter.addLine('Choose a character or item.');
end;

function CLIInfoStateHandlerClass.selectTarget(const reference : string) : boolean;
var
  character  : GameCharacterClass = nil;
begin
  result := false;
  character := globalInteractableRef.getCharacter(reference);
  if character <> nil then begin
    globalWriter.addLine('Name: ' + character.name);
    globalWriter.addLine('HP: ' + IntToStr(character.hp));
    globalWriter.addLine('"' + character.description + '"');
    result := true;
    cancel;
  end;
end;

end.

