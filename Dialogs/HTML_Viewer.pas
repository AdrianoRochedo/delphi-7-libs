unit HTML_Viewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw, ExtCtrls, Menus, Buttons, ComCtrls, RAEditor, MSHTML,
  StdCtrls, EmbeddedWB;

type
  THTML_Viewer = class(TForm)
    Book: TPageControl;
    Tab1: TTabSheet;
    Save: TSaveDialog;
    Panel1: TPanel;
    sbSalvarComo_1: TSpeedButton;
    sbREFRESH: TSpeedButton;
    sbSELECTALL: TSpeedButton;
    sbCOPY: TSpeedButton;
    sbPRINT: TSpeedButton;
    sbSTOP: TSpeedButton;
    sbPROPERTIES: TSpeedButton;
    Info: TPanel;
    ProgressBar: TProgressBar;
    Browser: TEmbeddedWB;
    Tab2: TTabSheet;
    Editor: TRAEditor;
    lab_Arq: TLabel;
    cbArq: TComboBox;
    sbOpen: TSpeedButton;
    OpenDLG: TOpenDialog;
    sbFind: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SalvarComo_Click(Sender: TObject);
    procedure BrowserDownloadBegin(Sender: TObject);
    procedure BrowserDownloadComplete(Sender: TObject);
    procedure BrowserProgressChange(Sender: TObject; Progress, ProgressMax: Integer);
    procedure sbREFRESHClick(Sender: TObject);
    procedure sbSELECTALLClick(Sender: TObject);
    procedure sbCOPYClick(Sender: TObject);
    procedure sbPRINTClick(Sender: TObject);
    procedure sbSTOPClick(Sender: TObject);
    procedure sbPROPERTIESClick(Sender: TObject);
    procedure BookChange(Sender: TObject);
    procedure cbArqSelect(Sender: TObject);
    procedure sbOpenClick(Sender: TObject);
    procedure cbArqKeyPress(Sender: TObject; var Key: Char);
    procedure sbFindClick(Sender: TObject);
  private
    FFileName: String;
    FLastPath: String;
    procedure ShowURL(const URL: String);
    function getViewFileNames(): Boolean;
    procedure setViewFileNames(const Value: Boolean);
  public
    constructor Create(const URL: string; FormStyle: TFormStyle);

    // Abre um arquivo HTML
    Procedure Open(const URL: String);

    // Mostra os nomes dos arquivos
    property ViewFileNames: Boolean read getViewFileNames write setViewFileNames;

    // Nome do arquivo
    property FileName : String read FFileName;
  end;

implementation
uses SysUtilsEx, JclStrings, JclFileUtils;

{$R *.DFM}

{ THTML_Viewer }

procedure THTML_Viewer.Open(const URL: String);
begin
  if FileExists(URL) then
     begin
     FFileName := URL;
     Info.Caption := ' ' + URL;

     cbArq.Text := URL;
     if cbArq.Items.IndexOf(URL) = -1 then
        begin
        cbArq.Items.Insert(0, URL);
        if cbArq.Items.Count > 20 then cbArq.Items.Delete(cbArq.Items.Count-1);
        end;

     Editor.Lines.LoadFromFile(URL);
     ShowURL(URL);
     end;
end;

procedure THTML_Viewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure THTML_Viewer.SalvarComo_Click(Sender: TObject);

  function HRefToFileName(const HRef: String): String;
  begin
    Result := System.Copy(HRef, 9, Length(HRef)-8);
    Result := ChangeChar(Result, '/', '\');
  end;

  procedure CopyImages(const oldPath, newPath: String);
  var doc: IHTMLDocument2;
      ec : IHTMLElementCollection;
      img: IHTMLImgElement;
      i: Integer;
      origem, destino, path: String;
  begin
    doc := Browser.Document as IHTMLDocument2;
    ec  := doc.images;
    for i := 0 to ec.length-1 do
      begin
      img := ec.item(i, '') as IHTMLImgElement;
      origem := HRefToFileName(img.href);

      if i = 0 then
         begin
         path := newPath + ChangeFileExt(ExtractFileName(FFilename), '') + '_files\';
         CreateDir(path);
         end;

      destino := path + img.nameProp;
      CopyFile(pChar(origem), pChar(destino), False);
      end;
  end;

  procedure AjustImagesLinks(SL: TStrings; const FileName: String);
  var s, s2: String;
      i, k, k2: Integer;
      Achou: Boolean;
  begin
    for i := 0 to SL.Count-1 do
      begin
      Achou := False;
      s := SL[i];
      k := StrSearch('<img', s);
      while k > 0 do
        begin
        Achou := True;
        k := StrSearch('src', s, k+1);
        if k > 0 then
           begin
           k  := StrSearch('"', s, k+1);
           k2 := StrSearch('"', s, k+1);
           s2 := ExtractFileName(ChangeChar(SubString(s, k, k2), '/', '\'));
           s2 := ChangeFileExt(ExtractFileName(FileName), '') + '_files\' + s2;
           Delete(s, k+1, k2-k-1); // remove o conteúdo entre " "
           Insert(s2, s, k+1);
           inc(k, length(s2) + 1);
           end;
        k := StrSearch('<img', s, k+1);
        end;
      if Achou then SL[i] := s;
      end;
  end;

  procedure CopyFile(const Source, Destiny: string);
  var SL: TStrings;
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(Source);
      AjustImagesLinks(SL, FFilename);
      SL.SaveToFile(Destiny);
    finally
      SL.Free;
    end;
  end;

var OldName: String;
begin
  Save.InitialDir := FLastPath;
  if Save.Execute then
     begin
     FLastPath := ExtractFilePath(Save.FileName);
     case Save.FilterIndex of
       2, 3: begin
             OldName := FFilename;
             FFilename := Save.FileName;
             CopyImages(ExtractFilePath(OldName), ExtractFilePath(FFilename));
             CopyFile(OldName, FFilename);
             end;

       1: Editor.Lines.SaveToFile(Save.FileName);
       end;

     FFileName := Save.FileName;
     Info.Caption := ' ' + FFileName;
     end;
end;

procedure THTML_Viewer.BrowserDownloadBegin(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Info.Caption := ' Lendo documento ...';
end;

procedure THTML_Viewer.BrowserDownloadComplete(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Info.Caption := ' ' + FFileName;
end;

procedure THTML_Viewer.BrowserProgressChange(Sender: TObject; Progress, ProgressMax: Integer);
begin
  ProgressBar.Max := ProgressMax;
  ProgressBar.Position := Progress;
end;

procedure THTML_Viewer.sbREFRESHClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_REFRESH, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure THTML_Viewer.sbSELECTALLClick(Sender: TObject);
begin
  Browser.SelectAll();
end;

procedure THTML_Viewer.sbCOPYClick(Sender: TObject);
begin
  Browser.Copy();
end;

procedure THTML_Viewer.sbPRINTClick(Sender: TObject);
begin
  Browser.Print();
end;

procedure THTML_Viewer.sbSTOPClick(Sender: TObject);
begin
  Browser.Stop();
end;

procedure THTML_Viewer.sbPROPERTIESClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_PROPERTIES, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure THTML_Viewer.BookChange(Sender: TObject);
begin
  if (Book.ActivePageIndex = 0) and Editor.Modified then
     if MessageDLG('Código modificado. Deseja recarregar o arquivo ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
        Editor.Lines.SaveToFile(FFileName);
        Editor.Modified := False;
        ShowURL(FFileName);
        end;
end;

procedure THTML_Viewer.ShowURL(const URL: String);
var Flags: OLEVariant;
begin
  Browser.Navigate(WideString(URL), Flags, Flags, Flags, Flags);
end;

constructor THTML_Viewer.Create(const URL: string; FormStyle: TFormStyle);
begin
  inherited Create(nil);
  self.FormStyle := FormStyle;
  Open(URL);
end;

procedure THTML_Viewer.cbArqSelect(Sender: TObject);
begin
  if cbArq.Text <> FFileName then
     Open(cbArq.Text);
end;

procedure THTML_Viewer.sbOpenClick(Sender: TObject);
begin
  OpenDLG.InitialDir := FLastPath;
  if OpenDLG.Execute() then
     begin
     FLastPath := ExtractFilePath(openDLG.FileName);
     open(openDLG.FileName);
     end;
end;

procedure THTML_Viewer.cbArqKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
     Open(cbArq.Text);
end;

function THTML_Viewer.getViewFileNames: Boolean;
begin
  Result := cbArq.Visible;
end;

procedure THTML_Viewer.setViewFileNames(const Value: Boolean);
begin
  cbArq.Visible := Value;
  lab_Arq.Visible := Value;
end;

procedure THTML_Viewer.sbFindClick(Sender: TObject);
begin
  Browser.Find();
end;

end.
