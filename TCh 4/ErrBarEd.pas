{**********************************************}
{   TErrorBarSeries Component Editor Dialog    }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit ErrBarEd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,Chart,Series, ErrorBar
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TErrorSeriesEditor = class(TForm)
    SEBarwidth: TEdit;
    Label1: TLabel;
    BPen: TButton;
    RGWidthUnit: TRadioGroup;
    UDBarWidth: TUpDown;
    RGStyle: TRadioGroup;
    CBColorEach: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure SEBarwidthChange(Sender: TObject);
    procedure BPenClick(Sender: TObject);
    procedure RGWidthUnitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGStyleClick(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
  public
    { Public declarations }
    ErrorSeries:TCustomErrorSeries;
  end;

implementation

{$R *.DFM}
Uses PenDlg,BrushDlg,BarEdit,TeeConst
     {$IFDEF D1}
     ,IEdit16
     {$ELSE}
     ,IEdiSeri
     {$ENDIF};

procedure TErrorSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  ErrorSeries:=TCustomErrorSeries(Tag);
  With ErrorSeries do
  begin
    UDBarWidth.Position:=ErrorWidth;
    if ErrorWidthUnits=ewuPercent then RGWidthUnit.ItemIndex:=0
                                  else RGWidthUnit.ItemIndex:=1;
    RGStyle.Visible:=ErrorSeries is TErrorSeries;
    if RGStyle.Visible then RGStyle.ItemIndex:=Ord(ErrorStyle);
    CBColorEach.Checked:=ColorEachPoint;
  end;
  if ErrorSeries is TErrorBarSeries then { trick }
     {$IFDEF D1}
     (GetOwnerForm(Parent) as TChartEditForm).InsertSeriesForm( TBarSeriesEditor,
                                                        1,TeeMsg_GalleryBar,
                                                        ErrorSeries);
     {$ELSE}
     (Parent.Owner as TFormTeeSeries).InsertSeriesForm( TBarSeriesEditor,
                                                        1,TeeMsg_GalleryBar,
                                                        ErrorSeries);
     {$ENDIF}
  CreatingForm:=False;
end;

procedure TErrorSeriesEditor.SEBarwidthChange(Sender: TObject);
begin
  if not CreatingForm then ErrorSeries.ErrorWidth:=UDBarWidth.Position;
end;

procedure TErrorSeriesEditor.BPenClick(Sender: TObject);
begin
  EditChartPen(Self,ErrorSeries.ErrorPen);
  With ErrorSeries do SeriesColor:=ErrorPen.Color;
end;

procedure TErrorSeriesEditor.RGWidthUnitClick(Sender: TObject);
begin
  ErrorSeries.ErrorWidthUnits:=TErrorWidthUnits(RGWidthUnit.ItemIndex);
end;

procedure TErrorSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

procedure TErrorSeriesEditor.RGStyleClick(Sender: TObject);
begin
  if not CreatingForm then
     ErrorSeries.ErrorStyle:=TErrorSeriesStyle(RGStyle.ItemIndex);
end;

procedure TErrorSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  ErrorSeries.ColorEachPoint:=CBColorEach.Checked;
end;

initialization
  RegisterClass(TErrorSeriesEditor);
end.
