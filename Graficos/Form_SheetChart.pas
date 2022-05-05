unit Form_SheetChart;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Form_Chart, Menus, StdCtrls, Buttons, ExtCtrls, TeeProcs, TeEngine, Chart,
  Frame_Planilha, vcf1;

type
  TfoSheetChart = class(TfoChart)
    Splitter1: TSplitter;
    FP: TFramePlanilha;
    L_Valor: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FChartClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FPTabClick(Sender: TObject; nRow, nCol: Integer);
  private
    procedure DoSeriesChange(Serie: TChartSeries); override;  
  public
    procedure RefreshSheet();
  end;

implementation

{$R *.DFM}

procedure TfoSheetChart.FormShow(Sender: TObject);
begin
  inherited;
  RefreshSheet;
end;

procedure TfoSheetChart.RefreshSheet;
var i, k, ii: Integer;
begin
  ii := 1;
  FP.Tab.ClearRange(1, 1, FP.Tab.MaxRow, FP.Tab.MaxCol, F1ClearAll);

  FP.Tab.MaxCol := Series.Count + 1;
  FP.Tab.MaxRow := Series[0].Count + 1;

  // Cabeçalho das linhas
  FP.Tab.ShowRowHeading := False;
  FP.Tab.SetActiveCell(1, ii);
  FP.Tab.TextRC[1, ii] := 'Int.';
  FP.Tab.SetFont('arial', 10, true, false, false, false, clBlack, false, false);
  for i := 1 to Series[0].Count do
    FP.Tab.TextRC[i+1, ii] := IntToStr(i);

  for i := 0 to Series.Count -1 do
    if Series[i].Active then
       begin
       inc(ii);
       FP.Tab.SetActiveCell(1, ii);
       FP.Tab.TextRC[1, ii] := Series[i].Title;
       FP.Tab.SetFont('arial', 10, true, false, false, false, clBlack, false, false);
       for k := 0 to Series[i].Count-1 do
         begin
         FP.Tab.SetActiveCell(k+2, ii);
         FP.Tab.NumberFormat := '#' + DecimalSeparator + '##';
         FP.Tab.Number := Series[i].YValue[k];
         end;
       end;
end;

procedure TfoSheetChart.FChartClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i, ii: Integer;
begin
  inherited;
  if Series <> nil then
     begin
     ii := 1;
     for i := 0 to Self.Series.Count-1 do
       begin
       if Self.Series[i].Active then inc(ii);
       if Self.Series[i] = Series then Break;
       end;
     if ii > 0 then
        begin
        FP.Tab.SetFocus;
        FP.Tab.SetActiveCell(ValueIndex+2, ii);
        FP.Tab.ShowActiveCell;
        FPTabClick(nil, ValueIndex+2, ii);
        end;
     end;
end;

procedure TfoSheetChart.DoSeriesChange(Serie: TChartSeries);
begin
  inherited;
  RefreshSheet;
end;

procedure TfoSheetChart.FPTabClick(Sender: TObject; nRow, nCol: Integer);
begin
  inherited;
  if (nRow > 0) and (nCol > 0) then
     L_Valor.Caption := 'Valor Selecionado: ' + FP.Tab.TextRC[nRow, nCol] + '  ' +
                        'Delta T: ' + FP.Tab.TextRC[nRow, 1];
end;

end.
