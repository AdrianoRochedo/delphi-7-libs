{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiWall;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Chart;

type
  TFormTeeWall = class(TForm)
    CBView3dWalls: TCheckBox;
    TabSubWalls: TTabControl;
    L33: TLabel;
    SHWallColor: TShape;
    BWallColor: TButton;
    BWallPen: TButton;
    BWallBrush: TButton;
    SEWallSize: TEdit;
    CBWallTransp: TCheckBox;
    UDWallSize: TUpDown;
    CBDark3D: TCheckBox;
    procedure TabSubWallsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBView3dWallsClick(Sender: TObject);
    procedure BWallPenClick(Sender: TObject);
    procedure BWallBrushClick(Sender: TObject);
    procedure BWallColorClick(Sender: TObject);
    procedure SEWallSizeChange(Sender: TObject);
    procedure SHWallColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBWallTranspClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBDark3DClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    procedure SetWallControls;
    Procedure RefreshShape;
  public
    { Public declarations }
    TheWall:TChartWall;
    Constructor CreateWall(Owner:TComponent; AWall:TChartWall);
  end;

implementation

{$R *.DFM}
Uses PenDlg,Teengine,TeeProcs,BrushDlg;

Constructor TFormTeeWall.CreateWall(Owner:TComponent; AWall:TChartWall);
begin
  inherited Create(Owner);
  TheWall:=AWall;
end;

procedure TFormTeeWall.TabSubWallsChange(Sender: TObject);
begin
  With TheWall.ParentChart do
  Case TabSubWalls.TabIndex of
    0: TheWall:=LeftWall;
    1: TheWall:=BottomWall;
  else TheWall:=BackWall;
  end;
  SetWallControls;
end;

procedure TFormTeeWall.FormShow(Sender: TObject);
begin
  With TheWall.ParentChart do
  if TheWall=LeftWall then TabSubWalls.TabIndex:=0 else
  if TheWall=BottomWall then TabSubWalls.TabIndex:=1 else
                             TabSubWalls.TabIndex:=2;
  SetWallControls;
  CreatingForm:=False;
end;

procedure TFormTeeWall.CBView3dWallsClick(Sender: TObject);
begin
  TheWall.ParentChart.View3dWalls:=CBView3dWalls.Checked;
end;

procedure TFormTeeWall.BWallPenClick(Sender: TObject);
begin
  EditChartPen(Self,TheWall.Pen);
end;

procedure TFormTeeWall.BWallBrushClick(Sender: TObject);
begin
  EditChartBrush(Self,TheWall.Brush);
end;

procedure TFormTeeWall.BWallColorClick(Sender: TObject);
begin
  With TheWall do Color:=EditColor(Self,Color);
  RefreshShape;
end;

procedure TFormTeeWall.SEWallSizeChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    TheWall.Size:=UDWallSize.Position;
    CBDark3D.Enabled:=TheWall.Size>0;
  end;
end;

Procedure TFormTeeWall.RefreshShape;
begin
  SHWallColor.Brush.Color:=TheWall.Color;
end;

procedure TFormTeeWall.SHWallColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BWallColorClick(Self);
end;

procedure TFormTeeWall.SetWallControls;
begin
  With TheWall do
  begin
    CBView3DWalls.Checked :=ParentChart.View3DWalls;
    UDWallSize.Position   :=Size;
    CBWallTransp.Checked  :=Brush.Style=bsClear;
    CBDark3D.Checked      :=Dark3D;
    CBDark3D.Enabled      :=Size>0;
  end;
  RefreshShape;
end;

procedure TFormTeeWall.CBWallTranspClick(Sender: TObject);
Const Styles:Array[Boolean] of TBrushStyle=(bsSolid,bsClear);
Var tmp:TBitmap;
begin
  With TheWall.Brush do
  begin
    if Bitmap<>nil then tmp:=Bitmap else tmp:=nil;
    Style:=Styles[TCheckBox(Sender).Checked];
    if tmp<>nil then Bitmap:=tmp;
    if Style=bsSolid then
       if TheWall.Color=clTeeColor then
       begin
         TheWall.Color:=clSilver;
         RefreshShape;
       end;
  end;
end;

procedure TFormTeeWall.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  SHWallColor.Cursor:=crTeeHand; 
  {$IFDEF D3}
  TabSubWalls.HotTrack:=True;
  {$ENDIF}
end;

procedure TFormTeeWall.CBDark3DClick(Sender: TObject);
begin
  TheWall.Dark3D:=CBDark3D.Checked;
end;

end.
