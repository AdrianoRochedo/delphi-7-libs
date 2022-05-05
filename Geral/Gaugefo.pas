unit Gaugefo;

(*
  AUTOR ............................... Adriano Rochedo
  DATA ................................ 09/06/1998

  ÚLTIMA ALTERAÇÃO .................... 24/08/1998

  COMENTÁRIOS:
    Esta unidade é responsável pelo gerenciamento de operações longas.
    O objeto criado aqui: Progress, age como um medidor de progresso,
    além disso, ele oferesse tratamento para multi-tarefa.
    O usuário que utilizá-lo, não precisará realizar a chamada de
    ProcessMessages, pois isto é feito automaticamente.

    OBS: DLG_Progress sempre devará ser criado localmente por causa da
         pilha de ativação da rotina.

    EX:

      Procedure TMyForm.MyProcedure(Sender: TObject);
      var i: longint;
          gProgress : TDLG_Progress;
      begin
        gProgress := CreateProgress(-1, -1, 1400000, 'Mensagem');
          for i := 1 to 1400000 do gProgress.Value := i; {<-- Operação demorada}
        gProgress.Free;
      end;

*)

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls;

type
  TDLG_Progress = class(TForm)
    G: TProgressBar;
    btnCancelar: TButton;
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private
    Movendo      : Boolean;
    DX,DY        : Integer;
    FPercent     : Integer;
    FSaveCaption : String;
    Procedure SetValue(Const Value: Longint);
  public
    Cancelar : boolean;
    Max      : Longint;
    Msg      : String[80];
    Property Value: Longint Write SetValue;
  end;

  Function CreateProgress(x, y: Integer;
                          Max: Longint;
                          Const S: String;
                          Center: Boolean = True): TDLG_Progress;

implementation

{$R *.DFM}

Function CreateProgress(x, y: Integer; Max: Longint; Const S: String; Center: Boolean = True): TDLG_Progress;
Var F: TForm;
Begin
  Result := TDLG_Progress.Create(Application);
  Result.Caption := S;
  Result.FSaveCaption := Application.MainForm.Caption;
  if Center then
     begin
     Result.Position := poMainFormCenter;
     Result.Show;
     end
  else
     If (x > -1) and (y > -1) Then
        Begin
        Result.Left := x;
        Result.Top  := y;
        Result.Show;
        End
     else
        Result.Visible := False;

  Result.Max  := Max;
  Result.Msg  := S;
End;

Procedure TDLG_Progress.SetValue(Const Value: Longint);
var P: Integer;
Begin
  P := Trunc(100 * Value / Max);
  If P <> FPercent Then
     Begin
     FPercent := P;
     If Visible Then G.Position := FPercent;
     If Msg <> '' Then
        Application.MainForm.Caption :=
           Format('%s: %s - (%d%%)',[Application.Title, Msg, FPercent]);
     End;
  Application.ProcessMessages;   
End;

procedure TDLG_Progress.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DX := X; DY := Y;
  Movendo := true;
end;

procedure TDLG_Progress.FormCreate(Sender: TObject);
begin
  G.Position := 0;
  Movendo := False;
end;

procedure TDLG_Progress.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var Atual: TPoint;
begin
  GetCursorPos(Atual);
  If Movendo Then
     Begin
     Left := Atual.X - DX;
     Top  := Atual.Y - DY;
     End;
end;

procedure TDLG_Progress.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Movendo := False;
end;

procedure TDLG_Progress.FormDestroy(Sender: TObject);
begin
  Application.MainForm.Caption := FSaveCaption;
end;

procedure TDLG_Progress.btnCancelarClick(Sender: TObject);
begin
  Cancelar := True;
end;

end.
