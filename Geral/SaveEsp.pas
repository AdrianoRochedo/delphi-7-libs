unit Saveesp;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, FileCtrl, Dialogs;

type
  TSalvaEspDLG = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Dir: TDirectoryListBox;
    Drive: TDriveComboBox;
    Nome: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Palavras: TMemo;
    SalvaPalavras: TBitBtn;
    RecuperaPalavras: TBitBtn;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    procedure OKBtnClick(Sender: TObject);
    procedure SalvaPalavrasClick(Sender: TObject);
    procedure RecuperaPalavrasClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SalvaEspDLG: TSalvaEspDLG;

implementation

{$R *.DFM}

Function ImprimeLinha(Const Linha: String): Boolean;
Var i : Integer;
Begin
  Result := True;
  For i := 0 to Palavras.Lines.Count-1 do
    If Pos(Palavras.Lines[i], Linha) <> 0 Then Begin
       Result := False;
       Break;
    End;
End;

procedure TSalvaEspDLG.OKBtnClick(Sender: TObject);
Var F : TextFile;
    i : Longint;
begin
  If Not FileExists(Dir.Directory + Nome.Text) Then
  Begin
    AssignFile(F, Dir.Directory + Nome.Text);

    Rewrite(F);
    For i := 1 to ActiveDoc.LineCount do
    begin
      If ImprimeLinha Then
         Write(F, ActiveDoc.Line[i]);
    end;
    CloseFile(F);

  End Else
    Raise Exception.Create('Arquivo já existe! Troque o Nome');
end;

procedure TSalvaEspDLG.SalvaPalavrasClick(Sender: TObject);
begin
  if SaveFileDialog.Execute then
     Palavras.SaveToFile(SaveFileDialog.FileName);
end;

procedure TSalvaEspDLG.RecuperaPalavrasClick(Sender: TObject);
begin
  if OpenFileDialog.Execute then
     Palavras.SaveToFile(SaveFileDialog.FileName);

end;

end.
