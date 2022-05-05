unit MessagesForm;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons,
{$IFDEF WIN32}
  gridx32;
{$ELSE}
  Gridsx;
{$ENDIF}

type
  TErrorType = (etError, etInformation, etWarning);

  TfoMessages = class(TForm)
    P: TPanel;
    i: TImage;
    Save: TSaveDialog;
    G: TdrStringAlignGrid;
    btnLimpar: TSpeedButton;
    btnSalvar: TSpeedButton;
    btnImprimir: TSpeedButton;
    PrintDialog1: TPrintDialog;
    cbTopo: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbTopoClick(Sender: TObject);
  private
    function TextHeight(const s: String; Canvas: TCanvas): Integer;
  public
    constructor Create();

    Procedure Add(ErrorType: TErrorType; Const Msg: String);
    procedure SaveToFile(Const Name: String; ToPrinter: Boolean = False);
    procedure Print();
    procedure Clear();
  end;

implementation
uses Math, Printers;

{$R *.DFM}
{$IFDEF WIN32}
  {$R ErrosBMP32.res}
{$ELSE}
  {$R errosBMP.res}
{$ENDIF}

procedure TfoMessages.FormResize(Sender: TObject);
begin
  G.ColWidths[1] := Self.Width - G.ColWidths[0] - 15;
end;

Procedure TfoMessages.Add(ErrorType: TErrorType; Const Msg: String);
Begin
  {Seleciona Recurso}
  Case ErrorType of
    etError:       i.picture.Bitmap.Handle := LoadBitmap(HInstance, 'Error_ID');
    etWarning:     i.picture.Bitmap.Handle := LoadBitmap(HInstance, 'Warning2_ID');
    etInformation: i.picture.Bitmap.Handle := LoadBitmap(HInstance, 'Information_ID');
    End;
  if g.Cells[1,0] <> '' then G.RowCount := G.RowCount + 1;

  g.RowHeights[G.RowCount - 1] := Max(i.picture.Bitmap.Height + 2,
                                      TextHeight(Msg, g.Canvas));

  G.ColWidths[0] := i.picture.Bitmap.Width;
  g.Color := i.picture.Bitmap.Canvas.Pixels[0,0];

  g.AddBitmap(0, G.RowCount - 1, i.picture.Graphic);
  G.Cells[1, G.RowCount - 1] := Msg;
  FormResize(nil);
End;

procedure TfoMessages.FormShow(Sender: TObject);
begin
  G.SetFocus;
end;

procedure TfoMessages.GDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var TS, y, i, ii, ia, TL: Integer; s: String;
begin
  if acol = 1 then
     begin
     TL := g.Canvas.TextHeight('X');
     s := g.Cells[1, aRow];
     g.Canvas.Brush.color := g.Color;
     g.Canvas.FillRect(Rect);
     y := Rect.Top + 4; ii := 1; TS := Length(s);
     for i := 1 to TS do
       if (s[i] = #13) or (i = TS) Then
          begin
          ia := i;
          if i = TS then inc(ia);
          g.Canvas.TextOut(Rect.Left + 4, y, Copy(s, ii, ia-ii));
          ii := ia + 1;
          inc(y, TL + 4);
          end;
     end;
end;

function TfoMessages.TextHeight(const s: String; Canvas: TCanvas): Integer;
var i, ii: Integer;
begin
  ii := 1;
  for i := 1 to Length(s) do
    if s[i] = #13 then inc(ii);
  Result := ii * Canvas.TextHeight(s) + ii * 4 + 4;
end;

procedure TfoMessages.btnSalvarClick(Sender: TObject);
begin
  If Save.Execute Then
     SaveToFile(Save.FileName);
end;

procedure TfoMessages.SaveToFile(Const Name: String; ToPrinter: Boolean = False);
var SL: TStrings;
    i, ii, ia, TS, j : Integer;
    s : String;
    PrintText: System.Text;
begin
  SL := TStringList.Create;

  for j := 0 to G.RowCount - 1 do
    begin
    s := G.Cells[1, j];
    TS := Length(s);
    ii := 1;
    for i := 1 to TS do
      if (s[i] = #13) or (i = TS) Then
         begin
         ia := i;
         if i = TS then inc(ia);
         SL.ADD(Copy(s, ii, ia-ii));
         ii := ia + 1;
         end;
    SL.ADD('');
    end;

  if toPrinter then
     if PrintDialog1.Execute then
        begin
        AssignPrn(PrintText);
        try
          Rewrite(PrintText);
          Printer.Canvas.Font := g.Font;
          for i := 0 to SL.Count - 1 do
            Writeln(PrintText, SL[i]);
        finally
           System.Close(PrintText);
          end;
        end
     else //nada
  else
     SL.SaveToFile(Name);
end;

procedure TfoMessages.Clear;
var i: Integer;
Begin
  if g = nil then exit;
  
  for i := 0 to g.RowCount - 1 do
    begin
    TBitMap(g.Objects[0, i]).Free;
    g.Objects[0, i] := nil;
    end;

  g.Cells[1,0] := '';
  g.Cells[0,0] := '';
  g.RowCount := 1;
  g.Canvas.FillRect(Rect(0,0,g.Width, g.Height));
End;

procedure TfoMessages.Print;
begin
  SaveToFile('', True);
end;

procedure TfoMessages.btnImprimirClick(Sender: TObject);
begin
  Print;
end;

procedure TfoMessages.btnLimparClick(Sender: TObject);
begin
  Clear;
end;

procedure TfoMessages.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TfoMessages.cbTopoClick(Sender: TObject);
begin
  if cbTopo.Checked then
     FormStyle := fsStayOnTop
  else
     FormStyle := fsNormal
end;

constructor TfoMessages.Create();
begin
  inherited Create(nil);
end;

end.
