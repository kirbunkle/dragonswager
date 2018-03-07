unit cliInputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  utils,
  cliObjectReference,
  cliQuitActionHandler,
  cliGenericActionHandler,
  cliUseActionHandler,
  cliConfirmActionHandler,
  cliCancelActionHandler,
  cliMenuActionHandler,
  cliInfoActionHandler,
  cliWriter;

type
  CLIInputProcessorClass = class
    private
      mInputText      : TStringList;
      mWriter         : CLIWriterClass;
      mCommandTable   : CLIObjectReferenceClass;

      // input handlers
      mQuitHandler    : CLIQuitActionHandlerClass;
      mUseHandler     : CLIUseActionHandlerClass;
      mMenuHandler    : CLIMenuActionHandlerClass;
      mConfirmHandler : CLIConfirmActionHandlerClass;
      mCancelHandler  : CLICancelActionHandlerClass;
      mInfoHandler    : CLIInfoActionHandlerClass;
    public
      constructor Create(const writerIn : CLIWriterClass);
      destructor Destroy; override;
      procedure processInput;
  end;

implementation

constructor CLIInputProcessorClass.Create(const writerIn : CLIWriterClass);
begin
  inherited Create;
  mWriter := writerIn;

  mInputText := TStringList.Create;
  mCommandTable := CLIObjectReferenceClass.Create;

  // action handlers
  mQuitHandler    := CLIQuitActionHandlerClass.Create(mWriter);
  mUseHandler     := CLIUseActionHandlerClass.Create(mWriter);
  mMenuHandler    := CLIMenuActionHandlerClass.Create(mWriter);
  mConfirmHandler := CLIConfirmActionHandlerClass.Create(mWriter);
  mCancelHandler  := CLICancelActionHandlerClass.Create(mWriter);
  mInfoHandler    := CLIInfoActionHandlerClass.Create(mWriter);

  // set up action commands
  mCommandTable.add('quit', mQuitHandler);
  mCommandTable.add('yes', mConfirmHandler);
  mCommandTable.add('no,back', mCancelHandler);
  mCommandTable.add('menu', mMenuHandler);
  mCommandTable.add('info', mInfoHandler);
  mCommandTable.add('use,cast,play', mUseHandler);
end;

destructor CLIInputProcessorClass.Destroy;
begin
  FreeAndNil(mInputText);
  FreeAndNil(mQuitHandler);
  FreeAndNil(mMenuHandler);
  FreeAndNil(mConfirmHandler);
  FreeAndNil(mCancelHandler);
  FreeAndNil(mInfoHandler);
  FreeAndNil(mUseHandler);
  FreeAndNil(mCommandTable);
  inherited Destroy;
end;

procedure CLIInputProcessorClass.processInput;
var
  text : string = '';
  ref  : TObject = nil;
begin
  mInputText.Clear;
  Write('>> ');
  ReadLn(text);
  split(text, mInputText);

  ref := mCommandTable.getUnique(mInputText.Strings[0]);
  if ref = nil then begin
    mWriter.addLine('- UNKNOWN COMMAND -');
    mWriter.addLine('I don''t know what that means');
  end else begin
    CLIGenericActionHandlerClass(ref).doAction(mInputText.CommaText);
  end;
end;

end.

