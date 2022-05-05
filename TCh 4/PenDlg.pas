{**********************************************}
{   TPenDialog                                 }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit PenDlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, TeCanvas
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TPenDialog = class(TForm)
    CBVisible: TCheckBox;
    SEWidth: TEdit;
    LWidth: TLabel;
    RGStyle: TRadioGroup;
    Button1: TButton;
    GroupBox1: TGroupBox;
    SHColor: TShape;
    BColor: TButton;
    Button3: TButton;
    UDWidth: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure RGStyleClick(Sender: TObject);
    procedure SEWidthChange(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure BColorClick(Sender: TObject);
    procedure SHColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    BackupPen:TChartPen;
    Procedure RefreshShape;
  public
    { Public declarations }
    ThePen:TPen;
  end;

Function EditColor(AOwner:TComponent; AColor:TColor):TColor;
Procedure EditChartPen(AOwner:TComponent; ChartPen:TChartPen);
Procedure EditPen(AOwner:TComponent; APen:TPen);

{ Show the Open Picture dialog to ask the user for a picture file }
procedure TeeLoadClearImage(AOwner:TComponent; AImage:TPicture);

{ Show / Hide controls in array }
Procedure ShowControls(Show:Boolean; Const AControls:Array of TControl);
{ Enable / Disable controls in array }
Procedure EnableControls(Show:Boolean; Const AControls:Array of TControl);

{ Asks the user a question and returns Yes or No }
Function TeeYesNo(Const Message:String):Boolean;

implementation

{$R *.DFM}
Uses TeeConst,TeeProcs
     {$IFDEF D3}
     ,ExtDlgs
     {$ENDIF}
     ;

Function EditColor(AOwner:TComponent; AColor:TColor):TColor;
Begin
  With TColorDialog.Create(AOwner) do
  try
    Color:=AColor;
    if Execute then AColor:=Color;
  finally
    Free;
  end;
  result:=AColor;
end;

Procedure EditChartPen(AOwner:TComponent; ChartPen:TChartPen);
Begin
  With TPenDialog.Create(AOwner) do
  try
    ThePen:=ChartPen;
    ShowModal;
  finally
    Free;
  end;
end;

Procedure EditPen(AOwner:TComponent; APen:TPen);
Begin
  With TPenDialog.Create(AOwner) do
  try
    ThePen:=TChartPen(APen);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TPenDialog.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  BackupPen:=TChartPen.Create(nil);
  BackupPen.Assign(ThePen);
  if ThePen is TChartPen then
  begin
    CBVisible.Checked:=TChartPen(ThePen).Visible;
    BackupPen.Visible:=CBVisible.Checked;
    {$IFNDEF D1}
    if (Win32Platform=VER_PLATFORM_WIN32_NT) then
       RGStyle.Items.Add(TeeMsg_SmallDotsPen);
    if TChartPen(ThePen).SmallDots then
       RGStyle.ItemIndex:=RGStyle.Items.Count-1
    else
    {$ENDIF}
       RGStyle.ItemIndex:=Ord(ThePen.Style);
  end
  else
  begin
    CBVisible.Visible:=False;
    RGStyle.ItemIndex:=Ord(ThePen.Style);
  end;
  UDWidth.Position:=ThePen.Width;
  RGStyle.Enabled:=ThePen.Width=1;
  SHColor.Cursor:=crTeeHand;
  RefreshShape;
  CreatingForm:=False;
end;

procedure TPenDialog.RGStyleClick(Sender: TObject);
begin
  {$IFNDEF D1}
  if (ThePen is TChartPen) and
     (Win32Platform=VER_PLATFORM_WIN32_NT) and
     (RGStyle.ItemIndex=RGStyle.Items.Count-1) then
         TChartPen(ThePen).SmallDots:=True
  else
  {$ENDIF}
  begin
    ThePen.Style:=TPenStyle(RGStyle.ItemIndex);
    if ThePen is TChartPen then
       TChartPen(ThePen).SmallDots:=False;
  end;
end;

procedure TPenDialog.SEWidthChange(Sender: TObject);
begin
  if not CreatingForm then
  begin
    ThePen.Width:=UDWidth.Position;
    RGStyle.Enabled:=ThePen.Width=1;
  end;
end;

procedure TPenDialog.CBVisibleClick(Sender: TObject);
begin
  if not CreatingForm then TChartPen(ThePen).Visible:=CBVisible.Checked;
end;

procedure TPenDialog.BColorClick(Sender: TObject);
begin
  With ThePen do Color:=EditColor(Self,Color);
  RefreshShape;
end;

Procedure TPenDialog.RefreshShape;
begin
  BColor.Enabled:=SHColor.Visible;
  SHColor.Brush.Color:=ThePen.Color;
end;

procedure TPenDialog.SHColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BColorClick(Self);
end;

procedure TPenDialog.Button3Click(Sender: TObject);
begin
  ThePen.Assign(BackupPen);
  if ThePen is TChartPen then
  begin
    TChartPen(ThePen).Visible:=BackupPen.Visible;
    {$IFNDEF D1}
    if Assigned(ThePen.OnChange) then 
    {$ENDIF}
      ThePen.OnChange(Self);
  end;
  ModalResult:=mrCancel;
end;

procedure TPenDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BackupPen.Free;
end;

procedure TPenDialog.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
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

Procedure ShowControls(Show:Boolean; Const AControls:Array of TControl);
var t:Integer;
begin
  for t:=Low(AControls) to High(AControls) do AControls[t].Visible:=Show;
end;

Procedure EnableControls(Show:Boolean; Const AControls:Array of TControl);
var t:Integer;
begin
  for t:=Low(AControls) to High(AControls) do AControls[t].Enabled:=Show;
end;

Function TeeYesNo(Const Message:String):Boolean;
Begin
  Screen.Cursor:=crDefault;
  result:=MessageDlg(Message,mtConfirmation,[mbYes,mbNo],0)=mrYes;
End;

end.
