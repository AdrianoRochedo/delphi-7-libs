{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiPage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Chart, ComCtrls;

type
  TFormTeePage = class(TForm)
    L17: TLabel;
    SEPointsPerPage: TEdit;
    CBScaleLast: TCheckBox;
    L18: TLabel;
    LabelPages: TLabel;
    BFirstPage: TBitBtn;
    ButtonPrevious: TBitBtn;
    ButtonNext: TBitBtn;
    BLastPage: TBitBtn;
    UDPointsPerPage: TUpDown;
    procedure SEPointsPerPageChange(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure CBScaleLastClick(Sender: TObject);
    procedure BLastPageClick(Sender: TObject);
    procedure BFirstPageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    procedure TheChartPageChange(Sender: TObject);
  public
    { Public declarations }
    TheChart : TCustomChart;
    Constructor CreateChart(Owner:TComponent; AChart:TCustomChart);
  end;

implementation

{$R *.DFM}

Constructor TFormTeePage.CreateChart(Owner:TComponent; AChart:TCustomChart);
begin
  inherited Create(Owner);
  TheChart:=AChart;
end;

procedure TFormTeePage.TheChartPageChange(Sender: TObject);
begin
  { show the current page number and the total number of pages }
  { (like a report) }
  LabelPages.Caption:=IntToStr(TheChart.Page)+'/'+IntToStr(TheChart.NumPages);
  { enable or disable buttons }
  ButtonPrevious.Enabled:=TheChart.Page > 1;
  ButtonNext.Enabled:=TheChart.Page < TheChart.NumPages;
  BFirstPage.Enabled:=ButtonPrevious.Enabled;
  BLastPage.Enabled:=ButtonNext.Enabled;
end;

procedure TFormTeePage.SEPointsPerPageChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    With TheChart do
    if SEPointsPerPage.Text='' then MaxPointsPerPage:=0
                               else MaxPointsPerPage:=UDPointsPerPage.Position;
    TheChartPageChange(TheChart); { <-- repaint page / number of pages }
  end;
end;

procedure TFormTeePage.ButtonPreviousClick(Sender: TObject);
begin
  TheChart.PreviousPage;  { <-- goto next chart page }
  TheChartPageChange(Self);
end;

procedure TFormTeePage.CBScaleLastClick(Sender: TObject);
begin
  TheChart.ScaleLastPage:=CBScaleLast.Checked;
end;

procedure TFormTeePage.BLastPageClick(Sender: TObject);
begin
  TheChart.Page:=TheChart.NumPages;  { <-- goto Last chart page }
  TheChartPageChange(Self);
end;

procedure TFormTeePage.BFirstPageClick(Sender: TObject);
begin
  TheChart.Page:=1;  { <-- goto first chart page }
  TheChartPageChange(Self);
end;

procedure TFormTeePage.FormShow(Sender: TObject);
begin
  With TheChart do
  begin
    UDPointsPerPage.Position :=MaxPointsPerPage;
    CBScaleLast.Checked      :=ScaleLastPage;
    TheChartPageChange(Self);
  end;
  CreatingForm:=False;
end;

procedure TFormTeePage.ButtonNextClick(Sender: TObject);
begin
  TheChart.NextPage;  { <-- goto next chart page }
  TheChartPageChange(Self);
end;

procedure TFormTeePage.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

end.
