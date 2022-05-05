unit Frame_HTML_IE;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Book_Interfaces, StdCtrls, OleCtrls, SHDocVw, ComCtrls, ExtCtrls;

type
  Tfr_HTML_IE = class(TFrame, IBookPage, IPage)
    WB: TWebBrowser;
    Info: TPanel;
    ProgressBar: TProgressBar;
    procedure WBDownloadBegin(Sender: TObject);
    procedure WBDownloadComplete(Sender: TObject);
    procedure WBProgressChange(Sender: TObject; Progress,
      ProgressMax: Integer);
  private
    FFileName: string;
    function getControl(): TWinControl;
    procedure setReadOnly(b: boolean);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
  public
    { Public declarations }
  end;

  TFactory = class(TInterfacedObject, IPageFactory)
    function CreatePage(aOwner: TComponent): IBookPage;
  end;

implementation
uses Book_Classes;

{$R *.dfm}

{ TTextDocument }

function Tfr_HTML_IE.getControl(): TWinControl;
begin
  result := self;
end;

procedure Tfr_HTML_IE.LoadFromFile(const Filename: string);
var Flags: OLEVariant;
begin
  WB.Navigate(WideString(Filename), Flags, Flags, Flags, Flags);
  FFileName := Filename;
end;

procedure Tfr_HTML_IE.SaveToFile(const Filename: string);
begin

end;

procedure Tfr_HTML_IE.setReadOnly(b: boolean);
begin

end;

{ TTextPageFactory }

function TFactory.CreatePage(aOwner: TComponent): IBookPage;
begin
  result := Tfr_HTML_IE.Create(aOwner);
end;

procedure Tfr_HTML_IE.WBDownloadBegin(Sender: TObject);
begin
  ProgressBar.Visible := true;
  ProgressBar.Position := 0;
  Info.Caption := ' Lendo documento ...';
end;

procedure Tfr_HTML_IE.WBDownloadComplete(Sender: TObject);
begin
  ProgressBar.Visible := False;
  ProgressBar.Position := 0;
  Info.Caption := ' ' + FFileName;
end;

procedure Tfr_HTML_IE.WBProgressChange(Sender: TObject; Progress, ProgressMax: Integer);
begin
  ProgressBar.Max := ProgressMax;
  ProgressBar.Position := Progress;
end;

initialization
  getPagesFactory.RegisterPage('WebBrowser', TFactory.Create());

end.
