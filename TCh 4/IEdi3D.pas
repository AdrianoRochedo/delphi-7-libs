{**********************************************}
{   TeeChart Pro 3D editor options             }
{   Copyright (c) 1998 by David Berneda        }
{**********************************************}
{$I teedefs.inc}
unit IEdi3D;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Chart, ComCtrls;

type
  TFormTee3D = class(TForm)
    GB3D: TGroupBox;
    L13: TLabel;
    L4: TLabel;
    L35: TLabel;
    L36: TLabel;
    CBView3d: TCheckBox;
    SE3d: TEdit;
    CBOrthogonal: TCheckBox;
    SBZoom: TScrollBar;
    LZoom: TLabel;
    SBRotation: TScrollBar;
    SBElevation: TScrollBar;
    LRotation: TLabel;
    LElevation: TLabel;
    Label1: TLabel;
    SBHOffset: TScrollBar;
    LHOffset: TLabel;
    Label3: TLabel;
    SBVOffset: TScrollBar;
    LVOffset: TLabel;
    UD3D: TUpDown;
    CBZoomText: TCheckBox;
    Label2: TLabel;
    SBPerspec: TScrollBar;
    LPerspec: TLabel;
    procedure CBOrthogonalClick(Sender: TObject);
    procedure SBZoomChange(Sender: TObject);
    procedure SBRotationChange(Sender: TObject);
    procedure SBElevationChange(Sender: TObject);
    procedure CBView3dClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBHOffsetChange(Sender: TObject);
    procedure SBVOffsetChange(Sender: TObject);
    procedure SE3dChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBZoomTextClick(Sender: TObject);
    procedure SBPerspecChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TheChart      : TCustomChart;
    AllowRotation : Boolean;
    CreatingForm  : Boolean;
    Constructor CreateChart(AOwner:TComponent; AChart:TCustomChart);
    Procedure CheckRotation;
  end;

implementation

{$R *.DFM}
uses TeCanvas;

Constructor TFormTee3D.CreateChart(AOwner:TComponent; AChart:TCustomChart);
begin
  inherited Create(AOwner);
  TheChart:=AChart;
end;

Procedure TFormTee3D.CheckRotation;
begin
  With TheChart do
  begin
    CBOrthogonal.Enabled:=View3D and AllowRotation;
    SBRotation.Enabled  :=CBOrthogonal.Enabled and (not View3DOptions.Orthogonal);
  end;
end;

procedure TFormTee3D.CBOrthogonalClick(Sender: TObject);
begin
  With TheChart.View3DOptions do
  begin
    Orthogonal:=CBOrthogonal.Checked;
    SBRotation.Enabled:=(not Orthogonal) and AllowRotation;
    SBElevation.Enabled:=not Orthogonal;
    SBPerspec.Enabled:=not Orthogonal;
  end;
end;

procedure TFormTee3D.SBZoomChange(Sender: TObject);
begin
  TheChart.View3DOptions.Zoom:=SBZoom.Position;
  LZoom.Caption:=IntToStr(SBZoom.Position)+'%';
end;

procedure TFormTee3D.SBRotationChange(Sender: TObject);
begin
  TheChart.View3DOptions.Rotation:=SBRotation.Position;
  LRotation.Caption:=IntToStr(SBRotation.Position);
end;

procedure TFormTee3D.SBElevationChange(Sender: TObject);
begin
  TheChart.View3DOptions.Elevation:=SBElevation.Position;
  LElevation.Caption:=IntToStr(SBElevation.Position);
end;

procedure TFormTee3D.CBView3dClick(Sender: TObject);
var tmp:Boolean;
begin
  With TheChart do
  Begin
    View3D              :=CBView3D.Checked;
    SE3D.Enabled        :=View3D;
    CBOrthogonal.Enabled:=View3D and AllowRotation;
    tmp:=View3D and (not View3DOptions.Orthogonal);
    SBRotation.Enabled:=tmp and AllowRotation;
    SBElevation.Enabled:=tmp;
    SBPerspec.Enabled:=tmp;
    SBHOffset.Enabled:=View3D;
    SBVOffset.Enabled:=View3D;
    SBZoom.Enabled:=View3D;
    CBZoomText.Enabled:=View3D;
  end;
end;

procedure TFormTee3D.FormShow(Sender: TObject);
var tmp:Integer;
begin
  With TheChart do
  begin
    CBView3D.Checked       :=View3D;
    SE3D.Enabled           :=View3D;
    UD3D.Position          :=Chart3DPercent;

    if Canvas.SupportsFullRotation then tmp:=1
                                   else tmp:=270;
    SBRotation.Min         :=tmp;
    SBElevation.Min        :=tmp;

    CBOrthogonal.Enabled   :=View3D and AllowRotation;

    With View3DOptions do
    begin
      SBZoom.Position        :=Zoom;
      CBOrthogonal.Checked   :=Orthogonal;
      SBRotation.Position    :=Rotation;
      SBElevation.Position   :=Elevation;
      SBHOffset.Position     :=HorizOffset;
      SBVOffset.Position     :=VertOffset;
      SBRotation.Enabled     :=CBOrthogonal.Enabled and (not Orthogonal);
      CBZoomText.Checked     :=ZoomText;
      SBPerspec.Position     :=Perspective;
    end;
  end;
  CreatingForm:=False;
end;

procedure TFormTee3D.SBHOffsetChange(Sender: TObject);
begin
  TheChart.View3DOptions.HorizOffset:=SBHOffset.Position;
  LHOffset.Caption:=IntToStr(SBHOffset.Position);
end;

procedure TFormTee3D.SBVOffsetChange(Sender: TObject);
begin
  TheChart.View3DOptions.VertOffset:=SBVOffset.Position;
  LVOffset.Caption:=IntToStr(SBVOffset.Position);
end;

procedure TFormTee3D.SE3dChange(Sender: TObject);
begin
  if not CreatingForm then TheChart.Chart3DPercent:=UD3D.Position;
end;

procedure TFormTee3D.FormCreate(Sender: TObject);
begin
  CreatingForm := True;
end;

procedure TFormTee3D.CBZoomTextClick(Sender: TObject);
begin
  TheChart.View3DOptions.ZoomText:=CBZoomText.Checked;
end;

procedure TFormTee3D.SBPerspecChange(Sender: TObject);
begin
  TheChart.View3DOptions.Perspective:=SBPerspec.Position;
  LPerspec.Caption:=IntToStr(SBPerspec.Position);
end;

end.
