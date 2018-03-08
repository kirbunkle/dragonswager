unit cliGenericStateHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  cliObjectReference,
  cliWriter,
  gameState;

type
  CLIGenericStateHandlerClass = class
    protected
      mInputText      : TStringList;
      mCommandTable   : CLIObjectReferenceClass;
      mParentRef      : CLIGenericStateHandlerClass;

    public
      constructor Create(const parent : CLIGenericStateHandlerClass);
      destructor Destroy; override;

      procedure processInput(const text : string);

      procedure confirm virtual;
      procedure cancel virtual;
      procedure doAction virtual;

      procedure display virtual abstract;
  end;

implementation

constructor CLIGenericStateHandlerClass.Create(const parent : CLIGenericStateHandlerClass);
begin
  inherited Create;
  mInputText := TStringList.Create;
  mInputText.Delimiter := ' ';
  mCommandTable := CLIObjectReferenceClass.Create;
  mParentRef := parent;
end;

destructor CLIGenericStateHandlerClass.Destroy;
begin
  FreeAndNil(mInputText);
  FreeAndNil(mCommandTable);
  inherited Destroy;
end;

procedure CLIGenericStateHandlerClass.processInput(const text : string);
var
  ref : TObject = nil;
begin
  mInputText.Clear;
  mInputText.DelimitedText := text;
  if mInputText.Count > 0 then begin;
    ref := mCommandTable.getUnique(mInputText.Strings[0]);
    if ref = nil then begin
      globalWriter.addLine('- UNKNOWN COMMAND -');
      globalWriter.addLine('I don''t know what "' + mInputText.Strings[0] + '" means');
    end else begin
      mInputText.Delete(0);
      if mInputText.Count > 0 then begin
        CLIGenericStateHandlerClass(ref).processInput(mInputText.DelimitedText);
      end else begin
        CLIGenericStateHandlerClass(ref).doAction;
      end;
    end;
  end;
end;

procedure CLIGenericStateHandlerClass.confirm;
begin
  if Assigned(mParentRef) then mParentRef.confirm;
end;

procedure CLIGenericStateHandlerClass.cancel;
begin
  if self = globalGameState.currentState then globalGameState.popState;
end;

procedure CLIGenericStateHandlerClass.doAction;
begin
  globalGameState.pushState(self);
end;

end.

