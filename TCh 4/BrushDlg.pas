{****************************************}
{    TeeChart. TBrushDialog              }
{ Copyright (c) 1996-98 by David Berneda }
{****************************************}
{$I teedefs.inc}
unit BrushDlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TBrushDialog = class(TForm)
    RGStyle: TRadioGroup;
    Button2: TButton;
    GroupBox1: TGroupBox;
    BColor: TButton;
    SHColor: TShape;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure RGStyleClick(Sender: TObject);
    procedure BColorClick(Sender: TObject);
    procedure SHColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    BackupBrush:TBrush;
    procedure RedrawShape;
  public
    { Public declarations }
    TheBrush:TBrush;
  end;

Procedure EditChartBrush( AOwner:TComponent;
                          ChartBrush:TBrush);

Function InternalEditFont(AOwner:TComponent; AFont:TFont):TFont;

implementation

{$R *.DFM}
Uses PenDlg, TeeProcs;

Function InternalEditFont(AOwner:TComponent; AFont:TFont):TFont;
Begin
  With TFontDialog.Create(AOwner) do
  try
    Font.Assign(AFont);
    if Execute then AFont.Assign(Font);
  finally
    Free;
  end;
  result:=AFont;
end;

Procedure EditChartBrush( AOwner:TComponent;
                          ChartBrush:TBrush);
Begin
  With TBrushDialog.Create(AOwner) do
  try
    TheBrush:=ChartBrush;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TBrushDialog.RedrawShape;
begin
  BColor.Enabled:=TheBrush.Style<>bsClear;
  SHColor.Brush.Color:=TheBrush.Color;
  SHColor.Visible:=BColor.Enabled;
end;

procedure TBrushDialog.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  BackupBrush:=TBrush.Create;
  BackupBrush.Assign(TheBrush);
  RGStyle.ItemIndex:=Ord(TheBrush.Style);
  SHColor.Cursor:=crTeeHand;
  RedrawShape;
end;

procedure TBrushDialog.RGStyleClick(Sender: TObject);
begin
  TheBrush.Style:=TBrushStyle(RGStyle.ItemIndex);
  RedrawShape;
end;

procedure TBrushDialog.BColorClick(Sender: TObject);
begin
  With TheBrush do Color:=EditColor(Self,Color);
  RedrawShape;
end;

procedure TBrushDialog.SHColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BColorClick(Self);
end;

procedure TBrushDialog.Button3Click(Sender: TObject);
begin
  TheBrush.Assign(BackupBrush);
  ModalResult:=mrCancel;
end;

procedure TBrushDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  BackupBrush.Free;
end;

end.
