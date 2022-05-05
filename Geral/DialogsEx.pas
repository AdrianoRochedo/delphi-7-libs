unit DialogsEx;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

Const
  cError        : Integer = 1000;
  cConfirmation : Integer = 1001;
  cWarning      : Integer = 1002;
  cInformation  : Integer = 1003;
  cPrinter      : Integer = 2000;
  cMonitor      : Integer = 2001;

type
  TMessageDlg2 = class(TForm)
    Image1: TImage;
    Mensagem: TLabel;
    B0: TBitBtn;
    B1: TBitBtn;
    B2: TBitBtn;
    B3: TBitBtn;
    B4: TBitBtn;
    B5: TBitBtn;
    procedure B0Click(Sender: TObject);
  private
    {Declarações Privadas}
  public
    ButtonPress: Word;
  end;

  // Retorta a ordem do botão precionado (1..N)
  function MessageDlg2(
             const Msg : string;
             AType     : TMsgDlgType;
             BCaptions : Array of String;
             BGlyphs   : Array of Integer;
             HelpCtx   : Longint
  ): Byte;

implementation

{$R *.DFM}
{$IFDEF WIN32}
  {$R BitMaps32.RES}
{$ELSE}
  {$R BitMaps.RES}
{$ENDIF}


// Retorta a ordem do botão precionado (1..N)
function MessageDlg2(
           const Msg : string;
           AType     : TMsgDlgType;
           BCaptions : Array of String;
           BGlyphs   : Array of Integer;
           HelpCtx   : Longint) : Byte;

Var Form: TMessageDlg2;
    B   : TBitBtn;
    i   : Byte;
    H   : HBitmap;
    t   : integer;
Begin
  Form := TMessageDlg2.Create(Application);
  Try
    Form.Mensagem.Caption := Msg;

    Case AType of
      mtError        : H := LoadBitmap(HInstance, PChar(cError));
      mtConfirmation : H := LoadBitmap(HInstance, PChar(cConfirmation));
      mtWarning      : H := LoadBitmap(HInstance, PChar(cWarning));
      mtInformation  : H := LoadBitmap(HInstance, PChar(cInformation));
    End;

    If AType <> mtCustom Then Begin
       Form.Image1.Picture.BitMap.Handle := H;
       Form.Image1.Update;
    End;

    For i := 0 to High(BCaptions) do
    Begin
      B := TBitBtn(Form.FindComponent('B'+IntToStr(i)));
      B.Caption := BCaptions[i];
      B.Visible := True;
    End;

    If BGlyphs[0] <> 0 Then
      For i := 0 to High(BGlyphs) do
      Begin
        B := TBitBtn(Form.FindComponent('B'+IntToStr(i)));
        Try
          If BGlyphs[i] <> 0 Then
             B.Glyph.Handle := LoadBitmap(HInstance, PChar(BGlyphs[i]));
        Except
          {Nada}
        End;
      End;

    If High(BCaptions)+1 > 3 Then
       Form.Height := 170
    Else
       Form.Height := 137;

    If High(BCaptions)+1 < 3 Then
       Form.Width := 310
    Else
       Form.Width := 446;

    t := Form.Mensagem.Left + Form.Mensagem.Width + 20;
    if Form.Width < t then Form.Width := t;

    Form.ShowModal();
    Result := Form.ButtonPress;
  Finally
    If AType <> mtCustom Then DeleteObject(H);
    Form.Close;
    Form.Release;
    Application.ProcessMessages;
  End;
End;

procedure TMessageDlg2.B0Click(Sender: TObject);
begin
  ButtonPress := TBitBtn(Sender).Tag + 1;
  Close;
end;

end.
