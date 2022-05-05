unit Equacoes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, drEdit, Frame_Planilha, psCore, ComCtrls, ExtCtrls,
  drGraficos;

type
  TForm1 = class(TForm)
    edEq: TEdit;
    Label1: TLabel;
    edPars: TEdit;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    edDurIni: TdrEdit;
    Label3: TLabel;
    edDurFim: TdrEdit;
    Label4: TLabel;
    edDurInc: TdrEdit;
    Label5: TLabel;
    Label6: TLabel;
    mTr: TMemo;
    btnCalcular: TButton;
    PageControl1: TPageControl;
    TabPlanilha: TTabSheet;
    TabGrafico: TTabSheet;
    P: TFramePlanilha;
    procedure btnCalcularClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    G: TgrGrafico;
    procedure DeclararVariaveis(Script: TPascalScript);
    procedure Calcular(Script: TPascalScript);
    procedure InicializaPlanilha;
    procedure Plotar;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses Lib_Math, SysUtilsEx, psBase, Series;

{$R *.dfm}

procedure TForm1.InicializaPlanilha;
var i, k, incr: Integer;
begin
  for k := 0 to mTr.Lines.Count-1 do
    begin
    p.Tab.SetActiveCell(1, k+2);
    p.Tab.SetFont('arial', 10, True, False, False, False, clBlack, False, False);
    p.Tab.Text := mTr.Lines[k];
    end;

  i := 3;
  k := edDurIni.AsInteger;
  incr := edDurInc.AsInteger;
  while k <= edDurFim.AsInteger do
    begin
    p.Tab.SetActiveCell(i, 1);
    p.Tab.SetFont('arial', 10, True, False, False, False, clBlack, False, False);
    p.Tab.Text := IntToStr(k);
    inc(k, incr);
    inc(i);
    end;
end;

procedure TForm1.Calcular(Script: TPascalScript);
var i, k, ini, fim, incr, anos, Row, Col: Integer;
    r: Real;
    Tr, Duracao, Result: TVariable;
begin
  ini  := edDurIni.AsInteger;
  fim  := edDurFim.AsInteger;
  incr := edDurInc.AsInteger;

  Tr      := Script.Variables.VarByName('Tr');
  Duracao := Script.Variables.VarByName('Duracao');
  Result  := Script.Variables.VarByName('Result');

  // permuta os anos
  Col := 2;
  for k := 0 to mTr.Lines.Count-1 do
    begin
    Anos := StrToIntDef(mTr.Lines[k], 0);
    Tr.Value := Anos;

    // permuta a duração
    Row := 3;
    i := ini;
    while i <= fim do
      begin
      Duracao.Value := i;
      Script.Execute;
      p.Tab.NumberRC[Row, Col] := Result.Value;
      inc(i, incr);
      inc(Row);
      end;

    inc(Col);
    end;
end;

procedure TForm1.DeclararVariaveis(Script: TPascalScript);
var SL: TStrings;
    i: Integer;
    r: Real;
    V: TVariable;
begin
  // Declara a variáve Result
  V := TVariable.Create('Result', pvtReal, 0, nil, True);
  Script.Variables.AddVar(V);

  // Declara a variáve Duracao
  V := TVariable.Create('Duracao', pvtInteger, 0, nil, True);
  Script.Variables.AddVar(V);

  // Declara a variáve Tr
  V := TVariable.Create('Tr', pvtInteger, 0, nil, True);
  Script.Variables.AddVar(V);

  // Declara os Parâmetros --> p1 p2 p3 ...
  SL := nil;
  StringToStrings(edPars.Text, SL, [' ']);
  for i := 0 to SL.Count-1 do
    begin
    r := StrToFloatDef(AllTrim(SL[i]));
    V := TVariable.Create('p'+IntToStr(i+1), pvtReal, r, nil, True);
    Script.Variables.AddVar(V);
    end;
  SL.Free;
end;
{
procedure TForm1.Plotar;
var Duracao, col, row, incr: Integer;
    Intensidade: Real;
    s: TLineSeries;
begin
  G.SeriesList.Clear;

  for Col := 0 to mTr.Lines.Count-1 do
    begin
    s := TLineSeries.Create(G);
    s.ParentChart := G;
    s.Title :=  'Tr = ' + mTr.Lines[Col];

    Row := 3;
    Duracao := edDurIni.AsInteger;
    incr := edDurInc.AsInteger;
    while Duracao <= edDurFim.AsInteger do
      begin
      Intensidade := p.Tab.NumberRC[Row, Col + 2];
      s.AddXY(Duracao, Intensidade);
      inc(Duracao, incr);
      inc(Row);
      end;

    end;
end;
}
procedure TForm1.Plotar;
var Duracao, col, row, incr: Integer;
    Intensidade: Real;
    s: TLineSeries;
begin
  G.Grafico.SeriesList.Clear;

  for Col := 0 to mTr.Lines.Count-1 do
    begin
    s := TLineSeries.Create(G);
    s.ParentChart := G.Grafico;
    s.Title :=  'Tr = ' + mTr.Lines[Col];

    Row := 3;
    Duracao := edDurIni.AsInteger;
    incr := edDurInc.AsInteger;
    while Duracao <= edDurFim.AsInteger do
      begin
      Intensidade := p.Tab.NumberRC[Row, Col + 2];
      s.AddXY(Duracao, Intensidade);
      inc(Duracao, incr);
      inc(Row);
      end;

    end;
end;

procedure TForm1.btnCalcularClick(Sender: TObject);
var Script: TPascalScript;
begin
  InicializaPlanilha;

  Screen.Cursor := crHourGlass;
  Script := TPascalScript.Create;
  try
    Script.Include(Lib_Math.API);
    Script.Text.Add('begin Result := ' + edEq.Text + ' end.');
    DeclararVariaveis(Script);
    if Script.Compile then
       begin
       Calcular(Script);
       Plotar;
       end
    else
       ShowMessage(Script.Errors.Text);
  finally
    Screen.Cursor := crDefault;
    Script.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  G := TgrGrafico.Create;
  G.Align := alClient;
  G.Parent := TabGrafico;
  G.BorderStyle := bsNone;
  G.Grafico.View3D := False;
  G.Grafico.LeftAxis.Logarithmic := True;
  G.Grafico.LeftAxis.Title.Caption := 'Intensidade';
  G.Grafico.BottomAxis.Title.Caption := 'Duração';
  G.Show;
end;

end.
