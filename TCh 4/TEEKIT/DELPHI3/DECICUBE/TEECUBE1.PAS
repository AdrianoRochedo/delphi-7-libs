unit teecube1;

interface

{ This unit is an example of drill-down using the TDecisionGraph
  component.
}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, mxgrid, mxDB, Db, DBTables, mxtables, mxstore, mxpivsrc, ExtCtrls,
  TeEngine, Series, TeeProcs, Chart, mxgraph, StdCtrls, Buttons;

type
  TDCDemoForm = class(TForm)
    Panel1: TPanel;
    DecisionPivot1: TDecisionPivot;
    DecisionCube1: TDecisionCube;
    DecisionQuery1: TDecisionQuery;
    DecisionSource1: TDecisionSource;
    DecisionGrid1: TDecisionGrid;
    DecisionGraph1: TDecisionGraph;
    Series1: TBarSeries;
    Series2: TBarSeries;
    Series4: TBarSeries;
    Panel2: TPanel;
    ButtonBack: TBitBtn;
    Series13: TBarSeries;
    Series3: TBarSeries;
    Series5: TBarSeries;
    procedure DecisionGraph1ClickSeries(Sender: TCustomChart;
      Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonBackClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DecisionSource1SummaryChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NewDim,tmpDim,tmpValueIndex:Integer;
    Mode:TDimGroup;
    Procedure SetGraphTitle;
  end;

var
  DCDemoForm: TDCDemoForm;

implementation

{$R *.DFM}

{ Change the Chart Title to show the "drilled" value }
Procedure TDCDemoForm.SetGraphTitle;
begin
  With DecisionGraph1.Title.Text do
  begin
    Clear;
    Add( DecisionSource1.GetSummaryName(DecisionCube1.CurrentSummary) );
    if DecisionSource1.GetState(tmpDim)=dmDrilled then
    begin
       Add(' of '+DecisionSource1.GetDimensionName(tmpDim)+': '+
            DecisionSource1.GetMemberAsString(tmpDim,tmpValueIndex) );
       if NewDim<>-1 then
          Add(' by '+DecisionSource1.GetDimensionName(DecisionSource1.GetActiveDim(Mode,NewDim,false)));
    end
    else
       Add(' by '+DecisionSource1.GetDimensionName(tmpDim));
  end;
end;


{ Perform drill-down when the mouse clicks a point... }
procedure TDCDemoForm.DecisionGraph1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var ColDim,RowDim:  Integer;
    tmpState:TRowState;
begin
  if (Series=Series13) then
  begin

    { get the active dimension of the Series... }
    RowDim := DecisionSource1.GetActiveDim(dgRow,0,true);
    ColDim := DecisionSource1.GetActiveDim(dgCol,0,true);
    if (RowDim<>-1) then
    begin
      tmpDim:=RowDim;
      Mode:=dgRow;
    end
    else
    if (ColDim<>-1) then
    begin
      tmpDim:=ColDim;
      Mode:=dgCol;
    end
    else Exit;

    { drill into that dimension point... }
    tmpValueIndex:=ValueIndex;
    DecisionSource1.DrillDimIndex( Mode, DecisionSource1.GetIndex(tmpDim,false), tmpValueIndex,false);

    { open another dimension to split the drilled point into several points... }
    tmpState:=DecisionSource1.GetRowState(tmpDim);
    if rcNextClosed in tmpState then NewDim:=tmpDim+1 else
    if rcPrevClosed in tmpState then NewDim:=tmpDim-1 else NewDim:=-1;
    if NewDim<>-1 then
       DecisionSource1.ToggleDimIndex(Mode,NewDim,false);

    { refresh Chart Title... }
    SetGraphTitle;

    { enable the user to "go back" to the previous state... }
    ButtonBack.Enabled:=True;
  end;
end;

procedure TDCDemoForm.ButtonBackClick(Sender: TObject);
begin
  { restore the drilled dimension and close the other dimension... }
  DecisionSource1.ToggleDimIndex(Mode,tmpDim,false);
  DecisionSource1.ToggleDimIndex(Mode,NewDim,false);
  NewDim:=-1;

  { refresh the Chart title... }
  SetGraphTitle;

  { disable the user to go back... }
  ButtonBack.Enabled:=False;
end;

procedure TDCDemoForm.FormShow(Sender: TObject);
begin
  NewDim:=0;
  Mode:=dgNone;
  tmpDim:=0;
  SetGraphTitle;
end;

procedure TDCDemoForm.DecisionSource1SummaryChange(Sender: TObject);
begin
  SetGraphTitle;
end;

end.
