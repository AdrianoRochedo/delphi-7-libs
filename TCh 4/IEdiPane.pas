{**********************************************}
{  TCustomChart (or derived) Editor Dialog     }
{  Copyright (c) 1996-98 by David Berneda      }
{**********************************************}
{$I teedefs.inc}
unit IEdiPane;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Chart, ComCtrls;

type
  TFormTeePanel = class(TForm)
    RGBevelIn: TRadioGroup;
    RGBevelOut: TRadioGroup;
    BPanelColor: TButton;
    CBPanelBorder: TCheckBox;
    GB4: TGroupBox;
    Shape1: TShape;
    Shape2: TShape;
    CBGradVisible: TCheckBox;
    BGradientStart: TButton;
    BGradientEnd: TButton;
    GB6: TGroupBox;
    RGBitmap: TRadioGroup;
    BBrowseImage: TButton;
    CBImageInside: TCheckBox;
    SEPanelBor: TEdit;
    L19: TLabel;
    L2: TLabel;
    SEPanelWi: TEdit;
    UDPanelWi: TUpDown;
    UDPanelBor: TUpDown;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure RGBevelInClick(Sender: TObject);
    procedure RGBevelOutClick(Sender: TObject);
    procedure CBPanelBorderClick(Sender: TObject);
    procedure SEPanelWiChange(Sender: TObject);
    procedure BGradientStartClick(Sender: TObject);
    procedure BGradientEndClick(Sender: TObject);
    procedure CBGradVisibleClick(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure RGBitmapClick(Sender: TObject);
    procedure BBrowseImageClick(Sender: TObject);
    procedure CBImageInsideClick(Sender: TObject);
    procedure SEPanelBorChange(Sender: TObject);
    procedure BPanelColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    CreatingForm : Boolean;
    procedure RepaintGradientColors;
    procedure CheckGradientVisible;
    procedure EnableImageControls;
  public
    { Public declarations }
    TheChart:TCustomChart;
    Constructor CreateChart(Owner:TComponent; AChart:TCustomChart);
  end;

procedure TeeLoadClearImage(AOwner:TComponent; AImage:TPicture);

implementation

{$R *.DFM}
Uses TeCanvas,PenDlg,TeeProcs,TeeConst
     {$IFDEF D3}
     ,ExtDlgs
     {$ENDIF}
     ;

Constructor TFormTeePanel.CreateChart(Owner:TComponent; AChart:TCustomChart);
begin
  inherited Create(Owner);
  TheChart:=AChart;
end;

procedure TFormTeePanel.RGBevelInClick(Sender: TObject);
begin
  if TheChart.BevelInner<>TPanelBevel(RGBevelIn.ItemIndex) then
     TheChart.BevelInner:=TPanelBevel(RGBevelIn.ItemIndex);
end;

procedure TFormTeePanel.RGBevelOutClick(Sender: TObject);
begin
  if TheChart.BevelOuter<>TPanelBevel(RGBevelOut.ItemIndex) then
     TheChart.BevelOuter:=TPanelBevel(RGBevelOut.ItemIndex);
end;

procedure TFormTeePanel.CBPanelBorderClick(Sender: TObject);
Const Styles:Array[Boolean] of TBorderStyle=(bsNone,bsSingle);
begin
  TheChart.BorderStyle:=Styles[CBPanelBorder.Checked]
end;

procedure TFormTeePanel.SEPanelWiChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart do
  if BevelWidth<>UDPanelWi.Position then BevelWidth:=UDPanelWi.Position;
end;

procedure TFormTeePanel.BGradientStartClick(Sender: TObject);
begin
  With TheChart.Gradient do StartColor:=EditColor(Self,StartColor);
  RepaintGradientColors;
end;

procedure TFormTeePanel.BGradientEndClick(Sender: TObject);
begin
  With TheChart.Gradient do EndColor:=EditColor(Self,EndColor);
  RepaintGradientColors;
end;

procedure TFormTeePanel.CheckGradientVisible;
Begin
  ComboBox1.ItemIndex:=Ord(TheChart.Gradient.Direction);
  EnableControls(TheChart.Gradient.Visible,[ ComboBox1,
                                             BGradientStart,
                                             BGradientEnd,
                                             Shape1,
                                             Shape2]);
  RepaintGradientColors;
end;

procedure TFormTeePanel.CBGradVisibleClick(Sender: TObject);
begin
  TheChart.Gradient.Visible:=CBGradVisible.Checked;
  CheckGradientVisible;
end;

procedure TFormTeePanel.Shape1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BGradientStartClick(Self);
end;

procedure TFormTeePanel.Shape2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BGradientEndClick(Self);
end;

procedure TFormTeePanel.FormShow(Sender: TObject);
begin
  With TheChart do
  begin
    RGBevelIn.ItemIndex    :=Ord(BevelInner);
    RGBevelOut.ItemIndex   :=Ord(BevelOuter);
    CBPanelBorder.Checked  :=BorderStyle=bsSingle;
    UDPanelWi.Position     :=BevelWidth;
    UDPanelBor.Position    :=BorderWidth;
    CBGradVisible.Checked  :=Gradient.Visible;
    CheckGradientVisible;
    RGBitmap.ItemIndex     :=Ord(BackImageMode);
    CBImageInside.Checked  :=BackImageInside;
    EnableImageControls;
  end;
  CreatingForm:=False;
end;

procedure TFormTeePanel.RGBitmapClick(Sender: TObject);
begin
  TheChart.BackImageMode:=TTeeBackImageMode(RGBitmap.ItemIndex);
end;

procedure TFormTeePanel.EnableImageControls;
begin
  RGBitmap.Enabled:=(TheChart.BackImage.Graphic<>nil);
  CBImageInside.Enabled:=RGBitmap.Enabled;
  if TheChart.BackImage.Graphic<>nil then
     BBrowseImage.Caption:=TeeMsg_ClearImage
  else
     BBrowseImage.Caption:=TeeMsg_BrowseImage;
end;

procedure TeeLoadClearImage(AOwner:TComponent; AImage:TPicture);
begin
  if AImage.Graphic<>nil then AImage.Assign(nil)
  else
  {$IFDEF D3}
  With TOpenPictureDialog.Create(AOwner) do
  {$ELSE}
  With TOpenDialog.Create(AOwner) do
  {$ENDIF}
  try
    Filter:=GraphicFilter(TGraphic);
    {$IFNDEF D3}
    DefaultExt:=TeeGetImageExtension(0);
    Options:=Options+[ofHideReadOnly];
    {$ENDIF}
    if Execute then AImage.LoadFromFile(FileName);
  finally
    Free;
  end;
end;

procedure TFormTeePanel.BBrowseImageClick(Sender: TObject);
begin
  TeeLoadClearImage(Self,TheChart.BackImage);
  EnableImageControls;
end;

procedure TFormTeePanel.CBImageInsideClick(Sender: TObject);
begin
  TheChart.BackImageInside:=CBImageInside.Checked;
end;

procedure TFormTeePanel.RepaintGradientColors;

  Procedure SetShape(AShape:TShape; AColor:TColor);
  begin
    With AShape do
    Begin
      Pen.Style:=psSolid;
      Pen.Color:=clBlack;
      Brush.Style:=bsSolid;
      Brush.Color:=AColor;
    end;
  end;

Begin
  SetShape(Shape1,TheChart.Gradient.StartColor);
  SetShape(Shape2,TheChart.Gradient.EndColor);
end;

procedure TFormTeePanel.SEPanelBorChange(Sender: TObject);
begin
  if not CreatingForm then
  With TheChart do
  if BorderWidth<>UDPanelBor.Position then BorderWidth:=UDPanelBor.Position;
end;

procedure TFormTeePanel.BPanelColorClick(Sender: TObject);
begin
  With TheChart do
  begin
    Color:=EditColor(Self,Color);
    Repaint;
  end;
end;

procedure TFormTeePanel.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  Shape1.Cursor:=crTeeHand;
  Shape2.Cursor:=crTeeHand;
end;

procedure TFormTeePanel.ComboBox1Change(Sender: TObject);
begin
  TheChart.Gradient.Direction:=TGradientDirection(ComboBox1.ItemIndex);
end;

end.
