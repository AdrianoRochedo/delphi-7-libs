{****************************************}
{    TPieSeries Editor Dialog            }
{ Copyright (c) 1996-98 by David Berneda }
{****************************************}
{$I teedefs.inc}
unit PieEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, Series
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TPieSeriesEditor = class(TForm)
    CBCircled: TCheckBox;
    CB3D: TCheckBox;
    SERotation: TEdit;
    Label1: TLabel;
    BPiePen: TButton;
    CBPatterns: TCheckBox;
    BBackPieColor: TBitBtn;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    SEXRadius: TEdit;
    CBAutoXR: TCheckBox;
    Label3: TLabel;
    SEYRadius: TEdit;
    CBAutoYR: TCheckBox;
    CBDark3d: TCheckBox;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    CBOther: TComboBox;
    Label6: TLabel;
    EOtherValue: TEdit;
    Label7: TLabel;
    EOtherLabel: TEdit;
    SEExpBig: TEdit;
    UDYRadius: TUpDown;
    UDXRadius: TUpDown;
    UDExpBig: TUpDown;
    UDRotation: TUpDown;
    Label8: TLabel;
    CBOrder: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure CBCircledClick(Sender: TObject);
    procedure CB3DClick(Sender: TObject);
    procedure SERotationChange(Sender: TObject);
    procedure BPiePenClick(Sender: TObject);
    procedure CBDark3dClick(Sender: TObject);
    procedure SEXRadiusChange(Sender: TObject);
    procedure SEYRadiusChange(Sender: TObject);
    procedure CBPatternsClick(Sender: TObject);
    procedure BBackPieColorClick(Sender: TObject);
    procedure CBAutoXRClick(Sender: TObject);
    procedure CBAutoYRClick(Sender: TObject);
    procedure CBOtherChange(Sender: TObject);
    procedure EOtherValueChange(Sender: TObject);
    procedure EOtherLabelChange(Sender: TObject);
    procedure SEExpBigChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBOrderChange(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    Pie:TPieSeries;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
Uses Teengine, PenDlg;

procedure TPieSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Pie:=TPieSeries(Tag);
  With Pie do
  begin
    CBCircled.Checked:=Circled;
    if (ParentChart<>nil) then CB3D.Checked:=ParentChart.View3d
                          else CB3D.Enabled:=False;
    UDExpBig.Position     :=ExplodeBiggest;
    UDRotation.Position   :=RotationAngle;
    CBDark3D.Checked      :=Dark3D;
    CBPatterns.Checked    :=UsePatterns;
    BBackPieColor.Enabled :=UsePatterns;
    UDXRadius.Position    :=CustomXRadius;
    UDYRadius.Position    :=CustomYRadius;
    With OtherSlice do
    begin
      EOtherLabel.Text:=Text;
      EOtherValue.Text:=FloatToStr(Value);
      CBOther.ItemIndex:=Ord(Style);
      EnableControls(Style<>poNone,[EOtherLabel,EOtherValue]);
    end;
    Case PieValues.Order of
      loNone:       CBOrder.ItemIndex:=0;
      loAscending:  CBOrder.ItemIndex:=1;
      loDescending: CBOrder.ItemIndex:=2;
    end;
  end;
  CreatingForm:=False;
end;

procedure TPieSeriesEditor.CBCircledClick(Sender: TObject);
begin
  Pie.Circled:=CBCircled.Checked;
end;

procedure TPieSeriesEditor.CB3DClick(Sender: TObject);
begin
  if (Pie.ParentChart<>nil) then
  Begin
    Pie.ParentChart.View3d:=CB3d.Checked;
  end;
end;

procedure TPieSeriesEditor.SERotationChange(Sender: TObject);
begin
  if not CreatingForm then Pie.RotationAngle:=UDRotation.Position;
end;

procedure TPieSeriesEditor.BPiePenClick(Sender: TObject);
begin
  EditChartPen(Self,Pie.PiePen);
end;

procedure TPieSeriesEditor.CBDark3dClick(Sender: TObject);
begin
  if not CreatingForm then Pie.Dark3D:=CBDark3D.Checked;
end;

procedure TPieSeriesEditor.SEXRadiusChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    Pie.CustomXRadius:=UDXRadius.Position;
    CBAutoXR.Checked:=UDXRadius.Position=0;
  end;
end;

procedure TPieSeriesEditor.SEYRadiusChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    Pie.CustomYRadius:=UDYRadius.Position;
    CBAutoYR.Checked:=UDYRadius.Position=0;
  end;
end;

procedure TPieSeriesEditor.CBPatternsClick(Sender: TObject);
begin
  Pie.UsePatterns:=CBPatterns.Checked;
  BBackPieColor.Enabled:=Pie.UsePatterns;
end;

procedure TPieSeriesEditor.BBackPieColorClick(Sender: TObject);
begin
  Pie.CircleBackColor:=EditColor(Self,Pie.CircleBackColor);
end;

procedure TPieSeriesEditor.CBAutoXRClick(Sender: TObject);
begin
  if CBAutoXR.Checked then UDXRadius.Position:=0;
end;

procedure TPieSeriesEditor.CBAutoYRClick(Sender: TObject);
begin
  if CBAutoYR.Checked then UDYRadius.Position:=0;
end;

procedure TPieSeriesEditor.CBOtherChange(Sender: TObject);
begin
  With Pie.OtherSlice do
  begin
    Style:=TPieOtherStyle(CBOther.ItemIndex);
    EnableControls(Style<>poNone,[EOtherLabel,EOtherValue]);
  end;
end;

procedure TPieSeriesEditor.EOtherValueChange(Sender: TObject);
begin
  if EOtherValue.Text<>'' then
     Pie.OtherSlice.Value:=StrToFloat(EOtherValue.Text);
end;

procedure TPieSeriesEditor.EOtherLabelChange(Sender: TObject);
begin
  Pie.OtherSlice.Text:=EOtherLabel.Text;
end;

procedure TPieSeriesEditor.SEExpBigChange(Sender: TObject);
begin
  if not CreatingForm then Pie.ExplodeBiggest:=UDExpBig.Position;
end;

procedure TPieSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

procedure TPieSeriesEditor.CBOrderChange(Sender: TObject);
begin
  if CBOrder.ItemIndex<>0 then
  With Pie,PieValues do
  begin
    Case CBOrder.ItemIndex of
      1: Order:=loAscending;
      2: Order:=loDescending;
    end;
    Sort;
    Repaint
  end;
end;

initialization
  RegisterClass(TPieSeriesEditor);
end.
 
