unit OutPut_HTML_DLG;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw, ExtCtrls, Menus, Buttons, ComCtrls;

type
  TOutPutHTML_DLG = class(TForm)
    Info: TPanel;
    Browser: TWebBrowser;
    Save: TSaveDialog;
    Panel1: TPanel;
    sbSalvarComo_1: TSpeedButton;
    ProgressBar: TProgressBar;
    sbREFRESH: TSpeedButton;
    sbSELECTALL: TSpeedButton;
    sbCOPY: TSpeedButton;
    sbPRINT: TSpeedButton;
    sbSTOP: TSpeedButton;
    sbPROPERTIES: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SalvarComo_Click(Sender: TObject);
    procedure BrowserDownloadBegin(Sender: TObject);
    procedure BrowserDownloadComplete(Sender: TObject);
    procedure BrowserProgressChange(Sender: TObject; Progress,
      ProgressMax: Integer);
    procedure sbREFRESHClick(Sender: TObject);
    procedure sbSELECTALLClick(Sender: TObject);
    procedure sbCOPYClick(Sender: TObject);
    procedure sbPRINTClick(Sender: TObject);
    procedure sbSTOPClick(Sender: TObject);
    procedure sbPROPERTIESClick(Sender: TObject);
  private
    FFileName: String;
  public
    Procedure Open(const URL: String);
  end;

implementation
uses MessageManager,
     FileUtilsMessages;

{$R *.DFM}

{ TOutPutHTML_DLG }

procedure TOutPutHTML_DLG.Open(const URL: String);
var Flags: OLEVariant;
begin
  FFileName := URL;
  Flags := 0;
  Info.Caption := ' ' + URL;
  Browser.Navigate(WideString(URL), Flags, Flags, Flags, Flags);
end;

procedure TOutPutHTML_DLG.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TOutPutHTML_DLG.SalvarComo_Click(Sender: TObject);
var OldName: String;
begin
  if Save.Execute then
     begin
     OldName := FFileName;
     FFileName := Save.FileName;
     CopyFile(pChar(OldName), pChar(FFileName), False);
     GetMessageManager.SendMessage(FILE_CREATE, [@FFileName]);
     Info.Caption := ' ' + FFileName;
     end;
end;

procedure TOutPutHTML_DLG.BrowserDownloadBegin(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Info.Caption := ' Lendo documento ...';
end;

procedure TOutPutHTML_DLG.BrowserDownloadComplete(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Info.Caption := ' ' + FFileName;
end;

procedure TOutPutHTML_DLG.BrowserProgressChange(Sender: TObject; Progress, ProgressMax: Integer);
begin
  ProgressBar.Max := ProgressMax;
  ProgressBar.Position := Progress;
end;

procedure TOutPutHTML_DLG.sbREFRESHClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_REFRESH, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TOutPutHTML_DLG.sbSELECTALLClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_SELECTALL, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TOutPutHTML_DLG.sbCOPYClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_COPY, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TOutPutHTML_DLG.sbPRINTClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TOutPutHTML_DLG.sbSTOPClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_STOP, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TOutPutHTML_DLG.sbPROPERTIESClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_PROPERTIES, OLECMDEXECOPT_DONTPROMPTUSER);
end;

end.
