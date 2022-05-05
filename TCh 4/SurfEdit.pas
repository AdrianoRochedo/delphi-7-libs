{**********************************************}
{   TSurfaceSeries Editor Dialog               }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit SurfEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, TeeSurfa;

type
  TSurfaceSeriesEditor = class(TForm)
    Button2: TButton;
    Button3: TButton;
    RadioGroup1: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    Surface      : TSurfaceSeries;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
Uses PenDlg, BrushDlg, TeeProcs, TeeGriEd;

procedure TSurfaceSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Surface:=TSurfaceSeries(Tag);
  With Surface do
  begin
    if WireFrame then RadioGroup1.ItemIndex:=1 else
    if DotFrame then RadioGroup1.ItemIndex:=2 else
                     RadioGroup1.ItemIndex:=0;
  end;
  TeeInsertGrid3DForm(Parent,Surface);
  CreatingForm:=False;
end;

procedure TSurfaceSeriesEditor.Button2Click(Sender: TObject);
begin
  EditChartPen(Self,Surface.Pen);
end;

procedure TSurfaceSeriesEditor.Button3Click(Sender: TObject);
begin
  EditChartBrush(Self,Surface.Brush);
end;

procedure TSurfaceSeriesEditor.RadioGroup1Click(Sender: TObject);
begin
  if not CreatingForm then
  Case RadioGroup1.ItemIndex of
    0: { solid }
       begin
         Surface.DotFrame:=False;
         Surface.WireFrame:=False;
       end;
    1: Surface.WireFrame:=True;
    2: Surface.DotFrame:=True;
  end;
end;

procedure TSurfaceSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

initialization
  RegisterClass(TSurfaceSeriesEditor);
end.
