unit SQL_Viewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RAEditor, RAHLEditor, Buttons, ExtCtrls, DialogUtils, ComCtrls,
  SysUtilsEx;

type
  Tfo_SQL_Viewer = class(TForm)
    EditorSQL: TRAHLEditor;
    Panel1: TPanel;
    sbExec: TSpeedButton;
    sbAbrir: TSpeedButton;
    sbSalvar: TSpeedButton;
    sbSalvarComo: TSpeedButton;
    sbCopy: TSpeedButton;
    sbCut: TSpeedButton;
    sbPaste: TSpeedButton;
    sbEdit: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    StatusBar: TStatusBar;
    procedure sbExecClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbAbrirClick(Sender: TObject);
    procedure sbSalvarClick(Sender: TObject);
    procedure sbSalvarComoClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure sbCutClick(Sender: TObject);
    procedure sbPasteClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure EditorSQLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorSQLKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorSQLChange(Sender: TObject);
  private
    FSQL: ISQL;
  public
    constructor Create(SQL: ISQL);
  end;

implementation
//uses gbd_fo_SQL_Editor,
uses Application_Class;

{$R *.dfm}

constructor Tfo_SQL_Viewer.Create(SQL: ISQL);
begin
  inherited Create(nil);
  FSQL := SQL;

  FSQL.setLocked(true);

  if FSQL.getFilename() <> '' then
     begin
     EditorSQL.Lines.LoadFromFile(FSQL.getFilename());
     Caption := ' ' + FSQL.getFilename();
     end
  else
     begin
     EditorSQL.Lines.Text := FSQL.getSQL();
     Caption := ' SQL: ' + FSQL.getTitle();
     end;

  Show();
end;

procedure Tfo_SQL_Viewer.sbExecClick(Sender: TObject);

  procedure setText(const s: string);
  begin
    StatusBar.Panels.Items[2].Text := ' ' + s;
  end;

var Error: string;
    rowsAffected: integer;
begin
  Error := FSQL.Execute(EditorSQL.Lines.Text, rowsAffected);
  if Error <> '' then
     begin
     Dialogs.MessageDLG(Error, mtError, [mbOk], 0);
     setText('');
     end
  else
     if rowsAffected = -1 then
        setText('Comando executado com sucesso')
     else
        setText(' Ok: ' + toString(rowsAffected) + ' registros afetados');
end;

procedure Tfo_SQL_Viewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FSQL.setLocked(false);
end;

procedure Tfo_SQL_Viewer.sbAbrirClick(Sender: TObject);
var Name: string;
begin
  if DialogUtils.SelectFile(Name, Applic.LastDir, 'Comandos SQL|*.sql') then
     begin
     Applic.LastDir := ExtractFilePath(Name);
     EditorSQL.Lines.LoadFromFile(Name);
     FSQL.setFilename(Name);
     Caption := ' ' + Name;
     end;
end;

procedure Tfo_SQL_Viewer.sbSalvarClick(Sender: TObject);
begin
  if FileExists(FSQL.getFilename()) then
     begin
     EditorSQL.Lines.SaveToFile(FSQL.getFilename());
     FSQL.setSQL(EditorSQL.Lines.Text);
     end
  else
     sbSalvarComoClick(Sender);
end;

procedure Tfo_SQL_Viewer.sbSalvarComoClick(Sender: TObject);
var s: string;
begin
  if DialogUtils.ChooseFilename(s, Applic.LastDir, 'sql', 'Comandos SQL|*.sql') then
     begin
     EditorSQL.Lines.SaveToFile(s);
     FSQL.setFilename(s);
     FSQL.setSQL(EditorSQL.Lines.Text);
     Applic.LastDir := ExtractFilePath(s);
     end;
end;

procedure Tfo_SQL_Viewer.sbCopyClick(Sender: TObject);
begin
  EditorSQL.ClipBoardCopy();
end;

procedure Tfo_SQL_Viewer.sbCutClick(Sender: TObject);
begin
  EditorSQL.ClipBoardCut();
end;

procedure Tfo_SQL_Viewer.sbPasteClick(Sender: TObject);
begin
  EditorSQL.ClipBoardPaste();
end;

procedure Tfo_SQL_Viewer.sbEditClick(Sender: TObject);
begin
(*
  with TfoSQLEditor.Create(FSQL.Database) do
    if ShowModal() = mrOk then
       begin
       self.EditorSQL.Lines.Text := SQLCommand;
       Free();
       end;
*)
end;

procedure Tfo_SQL_Viewer.EditorSQLMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Inutil1: Word;
    Inutil2: TShiftState;
begin
  EditorSQLKeyUp(Nil, Inutil1, Inutil2);
end;

procedure Tfo_SQL_Viewer.EditorSQLKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  StatusBar.Panels[0].Text := ' Lin: ' + IntToStr(EditorSQL.CaretY + 1);
  StatusBar.Panels[1].Text := ' Col: ' + IntToStr(EditorSQL.CaretX + 1);
end;

procedure Tfo_SQL_Viewer.EditorSQLChange(Sender: TObject);
begin
  StatusBar.Panels.Items[2].Text := '';
end;

end.
