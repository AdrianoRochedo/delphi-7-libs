{**********************************************}
{   TSurfaceSeries Editor Dialog               }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit ContEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, TeeSurfa
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TContourSeriesEditor = class(TForm)
    Button2: TButton;
    Label4: TLabel;
    SENumLevels: TEdit;
    CBColorEach: TCheckBox;
    Label2: TLabel;
    SEYPos: TEdit;
    CBYPosLevel: TCheckBox;
    UDYPos: TUpDown;
    UDNumLevels: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SENumLevelsChange(Sender: TObject);
    procedure CBYPosLevelClick(Sender: TObject);
    procedure SEYPosChange(Sender: TObject);
    procedure CBColorEachClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    Contour      : TContourSeries;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
Uses PenDlg, TeeGriEd;


procedure TContourSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Contour:=TContourSeries(Tag);
  With Contour do
  begin
    CBColorEach.Checked :=ColorEachPoint;
    CBYPosLevel.Checked :=YPositionLevel;
    SEYPos.Enabled      :=not YPositionLevel;
    UDYPos.Position     :=Round(YPosition);
    UDNumLevels.Position:=NumLevels;
  end;
  TeeInsertGrid3DForm(Parent,Contour);
  CreatingForm:=False;
end;

procedure TContourSeriesEditor.Button2Click(Sender: TObject);
begin
  EditChartPen(Self,Contour.Pen);
end;

procedure TContourSeriesEditor.SENumLevelsChange(Sender: TObject);
begin
  if not CreatingForm then Contour.NumLevels:=UDNumLevels.Position;
end;

procedure TContourSeriesEditor.CBYPosLevelClick(Sender: TObject);
begin
  Contour.YPositionLevel:=CBYPosLevel.Checked;
  SEYPos.Enabled:=not CBYPosLevel.Checked;
end;

procedure TContourSeriesEditor.SEYPosChange(Sender: TObject);
begin
  if not CreatingForm then Contour.YPosition:=UDYPos.Position;
end;

procedure TContourSeriesEditor.CBColorEachClick(Sender: TObject);
begin
  Contour.ColorEachPoint:=CBColorEach.Checked;
end;

procedure TContourSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

initialization
  RegisterClass(TContourSeriesEditor);
end.
