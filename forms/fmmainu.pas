unit FmMainU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, TwHexBinConverterU, Clipbrd, Menus, XMLPropStorage, FmAboutU;

type

  { TMainForm }

  TMainForm = class(TForm)
    edDec: TLabeledEdit;
    edHex: TLabeledEdit;
    gbLog: TGroupBox;
    BtnImages: TImageList;
    MenuItem1: TMenuItem;
    mMain: TMainMenu;
    mClose: TMenuItem;
    mNormal: TMenuItem;
    mMini: TMenuItem;
    mStayOnTop: TMenuItem;
    mView: TMenuItem;
    mFile: TMenuItem;
    mLog: TMemo;
    pLog: TPanel;
    pCalc: TPanel;
    btnClear: TSpeedButton;
    btnHexToClipboard: TSpeedButton;
    btnDecToClipboard: TSpeedButton;
    TimerLog: TTimer;
    XMLPropStorage1: TXMLPropStorage;
    procedure edHexChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Convert(Sender: TObject; aType: TConvertType);
    procedure Clear();
    procedure FormDestroy(Sender: TObject);
    procedure mCloseClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mMiniClick(Sender: TObject);
    procedure mNormalClick(Sender: TObject);
    procedure mStayOnTopClick(Sender: TObject);
    procedure btnDecToClipboardClick(Sender: TObject);
    procedure btnHexToClipboardClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure TimerLogTimer(Sender: TObject);
    procedure WriteResult(aText: string; aType: TConvertType);
    procedure Log(aText:string);
    procedure edDecChange(Sender: TObject);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);

  private
    Converter: TwHexBinConverter;
    aLogText: string;
    aStayOnTop: boolean;
    procedure CustomResize(Sender: TObject);
    procedure StayOnTop();
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.edHexChange(Sender: TObject);
begin
  Convert(Sender, ctToDec);
end;

procedure TMainForm.Convert(Sender: TObject; aType: TConvertType);
var
  aSource, aResult: String;
begin

  aSource:= TLabeledEdit(Sender).Text;
  aResult:= Converter.Convert(aSource, aType);
  WriteResult(aResult, aType);

  case aType of
    ctToHex: Log(aSource+' → '+aResult);
    ctToDec: Log('0x'+Converter.HexClear(aSource)+' → '+aResult);
  end;

end;

procedure TMainForm.Clear();
begin
  edHex.Clear;
  edDec.Clear;
  mLog.Clear;
  aLogText:= EmptyStr;
  TimerLog.Enabled:= false;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Converter);
end;

procedure TMainForm.mCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  FmAbout.ShowModal;
end;

procedure TMainForm.mMiniClick(Sender: TObject);
begin
  CustomResize(Sender);
end;

procedure TMainForm.mNormalClick(Sender: TObject);
begin
   CustomResize(Sender);
end;

procedure TMainForm.CustomResize(Sender: TObject);
begin
case TMenuItem(Sender).Name of
    'mNormal':
      begin
         btnHexToClipboard.Visible:= true;
         btnDecToClipboard.Visible:= true;
         btnClear.Visible:= true;
         pLog.Visible:= true;
         MainForm.AutoSize:= false;
         MainForm.Height:= 550;
         edDec.AnchorSideRight.Control:= btnDecToClipboard;
         edDec.BorderSpacing.Right:= 5;

         TMenuItem(Sender).Checked:= true;
      end;
    'mMini':
      begin
         MainForm.AutoSize:= true;
         btnHexToClipboard.Visible:= false;
         btnDecToClipboard.Visible:= false;
         btnClear.Visible:= false;
         pLog.Visible:= false;
         edDec.AnchorSideRight.Control:= pCalc;
         edDec.BorderSpacing.Right:= 15;

         TMenuItem(Sender).Checked:= true;
      end;
  end;
end;

procedure TMainForm.mStayOnTopClick(Sender: TObject);
begin
  aStayOnTop:= TMenuItem(Sender).Checked;
  StayOnTop();
end;

procedure TMainForm.btnDecToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText:= edDec.Text;
end;

procedure TMainForm.btnHexToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText:= edHex.Text;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  Clear;
end;


procedure TMainForm.StayOnTop();
begin
  if aStayOnTop then
       FormStyle:= fsSystemStayOnTop
    else
       FormStyle:= fsNormal;
end;

procedure TMainForm.TimerLogTimer(Sender: TObject);
begin
  mLog.Lines.Add(aLogText);
  TimerLog.Enabled:= false;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Converter:= TwHexBinConverter.Create;
end;

procedure TMainForm.WriteResult(aText: string; aType: TConvertType);
var
  tmpOnChange: TNotifyEvent;
  aTarget: TLabeledEdit;
begin
  case aType of
    ctToHex:
      aTarget:= edHex;
    ctToDec:
      aTarget:= edDec;
  end;

  with aTarget do
  begin
    tmpOnChange:= OnChange;
    OnChange:=nil;
    Text:= aText;
    OnChange:=tmpOnChange;
  end;
end;

procedure TMainForm.Log(aText: string);
begin
  aLogText:= aText;
  if TimerLog.Enabled then TimerLog.Enabled:= false;
  TimerLog.Enabled:=true;
end;

procedure TMainForm.edDecChange(Sender: TObject);
begin
     Convert(Sender, ctToHex);
end;

procedure TMainForm.XMLPropStorage1RestoreProperties(Sender: TObject);
begin
  aStayOnTop:= mStayOnTop.Checked;

  if mNormal.Checked then
     CustomResize(mNormal)
  else
     CustomResize(mMini);

  StayOnTop();
end;

end.

