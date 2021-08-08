unit FmAboutU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLIntf, FileInfo;

type

  { TFmAbout }

  TFmAbout = class(TForm)
    lLicense: TLabel;
    lSite: TLabel;
    lContact: TLabel;
    lVersion: TLabel;
    lAuthor: TLabel;
    lProgName: TLabel;
    pAbout: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure lContactClick(Sender: TObject);
    procedure lSiteClick(Sender: TObject);

  private
    function GetVersion: string;

  public

  end;

var
  FmAbout: TFmAbout;

implementation

{$R *.lfm}

procedure TFmAbout.FormCreate(Sender: TObject);
begin
  lVersion.Caption:= 'v.'+GetVersion;
end;

procedure TFmAbout.lContactClick(Sender: TObject);
begin
  OpenURL('https://t.me/degtyarev_a');
end;

procedure TFmAbout.lSiteClick(Sender: TObject);
begin
  OpenURL('http://repo.alexandr-degtyarev.ru');
end;

{ TFmAbout }
function TFmAbout.GetVersion: string;
var
  version: string;
  Info: TVersionInfo;
begin
  Info := TVersionInfo.Create;
  Info.Load(HINSTANCE);
  //[0] = Major version, [1] = Minor ver, [2] = Revision, [3] = Build Number
  version := IntToStr(Info.FixedInfo.FileVersion[0]) + '.' + IntToStr(
    Info.FixedInfo.FileVersion[1]) + '.' + IntToStr(Info.FixedInfo.FileVersion[2]) +
    '.' + IntToStr(Info.FixedInfo.FileVersion[3]);
  Result := version;
  Info.Free;
end;

end.

