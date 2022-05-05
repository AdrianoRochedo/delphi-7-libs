{**********************************************}
{   TCandleSeries Component Editor Dialog      }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit CandlEdi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, Series, CandleCh
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TCandleEditor = class(TForm)
    RGStyle: TRadioGroup;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    SEWidth: TEdit;
    ShapeUp: TShape;
    ShapeDown: TShape;
    CBShowOpen: TCheckBox;
    CBShowClose: TCheckBox;
    Button1: TButton;
    UDWidth: TUpDown;
    CBDraw3D: TCheckBox;
    CBDark3D: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure RGStyleClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure SEWidthChange(Sender: TObject);
    procedure ShapeDownMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapeUpMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBShowOpenClick(Sender: TObject);
    procedure CBShowCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBDraw3DClick(Sender: TObject);
    procedure CBDark3DClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    Procedure RefreshControls;
  public
    { Public declarations }
    Candle:TCandleSeries;
  end;

implementation

{$R *.DFM}
uses PenDlg, TeeProcs;

Procedure TCandleEditor.RefreshControls;
begin
  CBShowOpen.Enabled :=Candle.CandleStyle=csCandleBar;
  CBShowClose.Enabled:=CBShowOpen.Enabled;
  CBDraw3D.Enabled:=not CBShowOpen.Enabled;
  CBDark3D.Enabled:=CBDraw3D.Enabled;
end;

procedure TCandleEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Candle:=TCandleSeries(Tag);
  With Candle do
  begin
    if CandleStyle=csCandleStick then RGStyle.ItemIndex:=0
                                 else RGStyle.ItemIndex:=1;
    UDWidth.Position      :=CandleWidth;
    ShapeUp.Brush.Color   :=UpCloseColor;
    ShapeDown.Brush.Color :=DownCloseColor;
    CBShowOpen.Checked    :=ShowOpenTick;
    CBShowClose.Checked   :=ShowCloseTick;
    CBDraw3D.Checked      :=Draw3D;
    CBDark3D.Checked      :=Dark3D;
    RefreshControls;
  end;
  ShapeUp.Cursor:=crTeeHand;
  ShapeDown.Cursor:=crTeeHand;
  CreatingForm:=False;
end;

procedure TCandleEditor.BitBtn1Click(Sender: TObject);
begin
  With Candle do
  begin
    UpCloseColor:=EditColor(Self,UpCloseColor);
    ShapeUp.Brush.Color:=UpCloseColor;
  end;
end;

procedure TCandleEditor.RGStyleClick(Sender: TObject);
begin
  with Candle do
  if RGStyle.ItemIndex=0 then CandleStyle:=csCandleStick
                         else CandleStyle:=csCandleBar;
  RefreshControls;
end;

procedure TCandleEditor.BitBtn2Click(Sender: TObject);
begin
  With Candle do
  begin
    DownCloseColor:=EditColor(Self,DownCloseColor);
    ShapeDown.Brush.Color:=DownCloseColor;
  end;
end;

procedure TCandleEditor.SEWidthChange(Sender: TObject);
begin
  if not CreatingForm then Candle.CandleWidth:=UDWidth.Position;
end;

procedure TCandleEditor.ShapeDownMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BitBtn2Click(Self);
end;

procedure TCandleEditor.ShapeUpMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BitBtn1Click(Self);
end;

procedure TCandleEditor.CBShowOpenClick(Sender: TObject);
begin
  Candle.ShowOpenTick:=CBShowOpen.Checked;
end;

procedure TCandleEditor.CBShowCloseClick(Sender: TObject);
begin
  Candle.ShowCloseTick:=CBShowClose.Checked;
end;

procedure TCandleEditor.Button1Click(Sender: TObject);
begin
  EditPen(Self,Candle.Pen);
end;

procedure TCandleEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

procedure TCandleEditor.CBDraw3DClick(Sender: TObject);
begin
  Candle.Draw3D:=CBDraw3D.Checked;
end;

procedure TCandleEditor.CBDark3DClick(Sender: TObject);
begin
  Candle.Dark3D:=CBDark3D.Checked;
end;

initialization
  RegisterClass(TCandleEditor);
end.
