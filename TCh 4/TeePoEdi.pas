{**********************************************}
{   TSeriesPointer Component Editor Dialog     }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeePoEdi;

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
  TSeriesPointerEditor = class(TForm)
    GPPoint: TGroupBox;
    CBDrawPoint: TCheckBox;
    CB3dPoint: TCheckBox;
    CBInflate: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    SEPointHorizSize: TEdit;
    SEPointVertSize: TEdit;
    CBPointStyle: TComboBox;
    Label3: TLabel;
    BPoinPenCol: TBitBtn;
    UDPointHorizSize: TUpDown;
    UDPointVertSize: TUpDown;
    CBPoDark: TCheckBox;
    GroupBox1: TGroupBox;
    BPointFillColor: TBitBtn;
    CBDefBrushColor: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CBDrawPointClick(Sender: TObject);
    procedure CB3dPointClick(Sender: TObject);
    procedure SEPointHorizSizeChange(Sender: TObject);
    procedure BPointFillColorClick(Sender: TObject);
    procedure CBPointStyleChange(Sender: TObject);
    procedure BPoinPenColClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SEPointVertSizeChange(Sender: TObject);
    procedure CBInflateClick(Sender: TObject);
    procedure CBPoDarkClick(Sender: TObject);
    procedure CBDefBrushColorClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    procedure SetPointerVisible(Value:Boolean);
    Procedure Enable3DPoint;
  public
    { Public declarations }
    ThePointer:TSeriesPointer;
  end;

{ Adds a new sub-tab Form into the Series tab at EditChart dialog }
Procedure TeeInsertPointerForm(AParent:TControl; APointer:TSeriesPointer);

implementation

{$R *.DFM}
uses PenDlg,Teengine,TeeConst
     {$IFDEF D1}
     ,IEdit16
     {$ELSE}
     ,IEdiSeri
     {$ENDIF};

Procedure TeeInsertPointerForm(AParent:TControl; APointer:TSeriesPointer);

begin
  {$IFDEF D1}
  (GetOwnerForm(AParent) as TChartEditForm).InsertSeriesForm( TSeriesPointerEditor,
                                                      1,TeeMsg_GalleryPoint,
                                                      APointer);
  {$ELSE}
  (AParent.Owner as TFormTeeSeries).InsertSeriesForm( TSeriesPointerEditor,
                                                      1,TeeMsg_GalleryPoint,
                                                      APointer);
  {$ENDIF}
end;

Procedure TSeriesPointerEditor.Enable3DPoint;
begin
  With ThePointer do
  CB3DPoint.Enabled:= (ParentSeries.ParentChart.View3D) and
                      ( (Style=psRectangle) or
                        (Style=psTriangle) or
                        (Style=psDownTriangle) );
  CBPoDark.Enabled:=CB3DPoint.Enabled;
end;

procedure TSeriesPointerEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  if TPersistent(Tag) is TSeriesPointer then
     ThePointer:=TSeriesPointer(Tag)
  else
     ThePointer:=TCustomSeries(Tag).Pointer;
  With ThePointer do
  Begin
    SetPointerVisible(Visible);
    CBDrawPoint.Checked:=Visible;
    CB3DPoint.Checked:=Draw3D;
    Enable3DPoint;
    UDPointHorizSize.Position:=HorizSize;
    UDPointVertSize.Position :=VertSize;
    CBPointStyle.ItemIndex:=Ord(Style);
    CBInflate.Checked:=InflateMargins;
    CBPoDark.Checked:=Dark3D;
    CBDefBrushColor.Checked:=Brush.Color=clTeeColor;

    if not AllowChangeSize then
       ShowControls(False,[ Label1,Label2,SEPointHorizSize,
                            SEPointVertSize,UDPointHorizSize,UDPointVertSize]);
  end;
  CreatingForm:=False;
end;

procedure TSeriesPointerEditor.SetPointerVisible(Value:Boolean);
begin
  ThePointer.Visible:=Value;
  if Value then Enable3DPoint
  else
  begin
    CB3DPoint.Enabled:=False;
    CBPoDark.Enabled:=False;
  end;
  EnableControls(Value,[ CBInflate,SEPointHorizSize,SEPointVertSize,
                         UDPointHorizSize,UDPointVertSize,BPointFillColor,
                         CBPointStyle,BPoinPenCol,CBDefBrushColor] );
end;

procedure TSeriesPointerEditor.CBDrawPointClick(Sender: TObject);
begin
  SetPointerVisible(CBDrawPoint.Checked);
end;

procedure TSeriesPointerEditor.CB3dPointClick(Sender: TObject);
begin
  ThePointer.Draw3D:=CB3DPoint.Checked;
end;

procedure TSeriesPointerEditor.SEPointHorizSizeChange(Sender: TObject);
begin
  if not CreatingForm then ThePointer.HorizSize:=UDPointHorizSize.Position;
end;

procedure TSeriesPointerEditor.BPointFillColorClick(Sender: TObject);
var tmp:TColor;
begin
  with ThePointer.Brush do
  begin
    if Color=clTeeColor then tmp:=ThePointer.ParentSeries.SeriesColor
                        else tmp:=Color;
    Color:=EditColor(Self,tmp);
    CBDefBrushColor.Checked:=Color=clTeeColor;
  end;
end;

procedure TSeriesPointerEditor.CBPointStyleChange(Sender: TObject);
begin
  ThePointer.Style:=TSeriesPointerStyle(CBPointStyle.ItemIndex);
  Enable3DPoint;
end;

procedure TSeriesPointerEditor.BPoinPenColClick(Sender: TObject);
begin
  EditChartPen(Self,ThePointer.Pen);
end;

procedure TSeriesPointerEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  ThePointer:=nil;
end;

procedure TSeriesPointerEditor.SEPointVertSizeChange(Sender: TObject);
begin
  if not CreatingForm then ThePointer.VertSize:=UDPointVertSize.Position;
end;

procedure TSeriesPointerEditor.CBInflateClick(Sender: TObject);
begin
  ThePointer.InflateMargins:=CBInflate.Checked;
end;

procedure TSeriesPointerEditor.CBPoDarkClick(Sender: TObject);
begin
  ThePointer.Dark3D:=CBPoDark.Checked;
end;

procedure TSeriesPointerEditor.CBDefBrushColorClick(Sender: TObject);
begin
  if CBDefBrushColor.Checked then ThePointer.Brush.Color:=clTeeColor;
end;

initialization
  RegisterClass(TSeriesPointerEditor);
end.
