unit BXML_Viewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw, ExtCtrls, Menus, Buttons, ComCtrls, RAEditor, MSHTML,
  StdCtrls, EmbeddedWB;

type
  TBXML_Viewer = class(TForm)
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
    FPluginsPath: String;
    FHTMLFile: String;
    FiniName: String;
    FTempFiles: TStrings;
    FLastPath: String;
    FSetupFile: String;
    procedure ConvertToHTMLAndShow(const URL: String);
    function getViewFileNames(): Boolean;
    procedure setViewFileNames(const Value: Boolean);
    destructor Destroy; override;
  public
    // SetupFile se refere ao arquivo de configuração da aplicação
    constructor Create(const URL, PluginsPath, SetupFile: string; FormChild: Boolean = False);

    // Abre um arquivo BXML
    Procedure Open(const URL: String);

    // Mostra os nomes dos arquivos
    property ViewFileNames: Boolean read getViewFileNames write setViewFileNames;

    property HTMLFile    : String read FHTMLFile;
    property FileName    : String read FFileName;
    property PluginsPath : String read FPluginsPath write FPluginsPath;
  end;

implementation
uses IniFiles, BXML_Conversor, SysUtilsEx, JclStrings, JclFileUtils;

{$R *.DFM}

{ TBXML_View }

procedure TBXML_Viewer.Open(const URL: String);
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
     ConvertToHTMLAndShow(URL);
     end;
end;

procedure TBXML_Viewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TBXML_Viewer.SalvarComo_Click(Sender: TObject);

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
         path := newPath + ChangeFileExt(ExtractFileName(FHTMLFile), '') + '_files\';
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
      AjustImagesLinks(SL, FHTMLFile);
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
             OldName := FHTMLFile;
             FHTMLFile := Save.FileName;
             CopyImages(ExtractFilePath(OldName), ExtractFilePath(FHTMLFile));
             CopyFile(OldName, FHTMLFile);
             end;

       1: Editor.Lines.SaveToFile(Save.FileName);
       end;

     FFileName := Save.FileName;
     Info.Caption := ' ' + FFileName;
     end;
end;

procedure TBXML_Viewer.BrowserDownloadBegin(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Info.Caption := ' Lendo documento ...';
end;

procedure TBXML_Viewer.BrowserDownloadComplete(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Info.Caption := ' ' + FFileName;
end;

procedure TBXML_Viewer.BrowserProgressChange(Sender: TObject; Progress, ProgressMax: Integer);
begin
  ProgressBar.Max := ProgressMax;
  ProgressBar.Position := Progress;
end;

procedure TBXML_Viewer.sbREFRESHClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_REFRESH, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TBXML_Viewer.sbSELECTALLClick(Sender: TObject);
begin
  Browser.SelectAll();
end;

procedure TBXML_Viewer.sbCOPYClick(Sender: TObject);
begin
  //Browser.ExecWB(OLECMDID_COPY, OLECMDEXECOPT_DONTPROMPTUSER);
  Browser.Copy();
end;

procedure TBXML_Viewer.sbPRINTClick(Sender: TObject);
begin
  //Browser.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DONTPROMPTUSER);
  Browser.Print();
end;

procedure TBXML_Viewer.sbSTOPClick(Sender: TObject);
begin
  //Browser.ExecWB(OLECMDID_STOP, OLECMDEXECOPT_DONTPROMPTUSER);
  Browser.Stop();
end;

procedure TBXML_Viewer.sbPROPERTIESClick(Sender: TObject);
begin
  Browser.ExecWB(OLECMDID_PROPERTIES, OLECMDEXECOPT_DONTPROMPTUSER);
end;

procedure TBXML_Viewer.BookChange(Sender: TObject);
begin
  if (Book.ActivePageIndex = 0) and Editor.Modified then
     if MessageDLG('Código modificado. Deseja recarregar o arquivo ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
        Editor.Lines.SaveToFile(FFileName);
        Editor.Modified := False;
        ConvertToHTMLAndShow(FFileName);
        end;
end;

procedure TBXML_Viewer.ConvertToHTMLAndShow(const URL: String);
var x: TBXML_MultiConversor;
    Flags: OLEVariant;
begin
  x := TBXML_MultiConversor.Create(FPluginsPath, URL);
  if x.Transform then
     begin
     Flags := 0;
     FHTMLFile := x.OutputFile;
     Browser.Navigate(WideString(FHTMLFile), Flags, Flags, Flags, Flags);
     FTempFiles.Add(FHTMLFile);
     end;
  x.Free;
end;

constructor TBXML_Viewer.Create(const URL, PluginsPath, SetupFile: string;
                                FormChild: Boolean = False);
var ini: TIniFile;
    i: Integer;
    s: String;
begin
  inherited Create(nil);

  if FormChild then FormStyle := fsMDIChild;
  FPluginsPath := PluginsPath;
  FSetupFile := SetupFile;
  FTempFiles := TStringList.Create;

  if FileExists(FSetupFile) then
     begin
     ini := TIniFile.Create(FIniName);
     FLastPath := ini.ReadString('psEditor', 'LastPath', '');
     for i := 1 to ini.ReadInteger('psEditor', 'Arquivos', 0) do
       cbArq.Items.Add(ini.ReadString('psEditor', 'A' + intToStr(i), ''));
     ini.Free;
     end;

  Open(URL);
end;

procedure TBXML_Viewer.cbArqSelect(Sender: TObject);
begin
  if cbArq.Text <> FFileName then
     Open(cbArq.Text);
end;

procedure TBXML_Viewer.sbOpenClick(Sender: TObject);
var s: String;
    Flags: OLEVariant;
begin
  OpenDLG.InitialDir := FLastPath;
  if OpenDLG.Execute then
     begin
     FLastPath := ExtractFilePath(openDLG.FileName);
     s := ExtractFileExt(openDLG.FileName);

     if CompareText(s, '.bxml') = 0 then
        open(openDLG.FileName) else

     if (CompareText(s, '.htm') = 0) or (CompareText(s, '.html') = 0) then
        begin
        Browser.Navigate(WideString(openDLG.FileName), Flags, Flags, Flags, Flags);
        Info.Caption := openDLG.FileName;
        end;
     end;
end;

procedure TBXML_Viewer.cbArqKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
     Open(cbArq.Text);
end;

destructor TBXML_Viewer.Destroy;
var ini: TIniFile;
    i: Integer;
    s: String;
begin
  // Apaga os arquivos e diretórios temporários criados
  for i := 0 to FTempFiles.Count-1 do
    begin
    DeleteFile(FTempFiles[i]);
    JclFileUtils.DelTree(ChangeFileExt(FTempFiles[i], '') + '_files');
    end;
  FTempFiles.Free;

  if FileExists(FSetupFile) then
     begin
     ini := TIniFile.Create(FSetupFile);
     ini.WriteString('psEditor', 'LastPath', FLastPath);
     ini.EraseSection('psEditor');
     ini.WriteInteger('psEditor', 'Arquivos', cbArq.Items.Count);
     for i := 0 to cbArq.Items.Count-1 do
       ini.WriteString('psEditor', 'A' + intToStr(i+1), cbArq.Items[i]);
     ini.Free;
     end;

  inherited;
end;

function TBXML_Viewer.getViewFileNames: Boolean;
begin
  Result := cbArq.Visible;
end;

procedure TBXML_Viewer.setViewFileNames(const Value: Boolean);
begin
  cbArq.Visible := Value;
  lab_Arq.Visible := Value;
end;

procedure TBXML_Viewer.sbFindClick(Sender: TObject);
begin
  Browser.Find();
end;

end.
