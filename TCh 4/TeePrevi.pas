{*******************************************}
{    TeeChart & TeeTree PrintPreview Form   }
{   Copyright (c) 1996-98 by David Berneda  }
{*******************************************}
{$I teedefs.inc}
unit TeePrevi;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, TeeProcs
  {$IFDEF D1}
  , Spin, TeeUpDow
  {$ELSE}
  , ComCtrls
  {$ENDIF}
  ;

{$IFDEF TEEOCX}
Const TeeMsg_OCXNoPrinter= 'There is no default printer.'+#13+
                           'Use Windows Control Panel to add a Printer';
{$ENDIF}

type
  TOnChangeMarginsEvent=Procedure (Sender:TObject; DisableProportional:Boolean; Const NewMargins:TRect) of object;

  TeePreviewZones=( teePrev_None,
                    teePrev_Left,
                    teePrev_Top,
                    teePrev_Right,
                    teePrev_Bottom,
                    teePrev_Image,
                    teePrev_LeftTop,
                    teePrev_RightTop,
                    teePrev_LeftBottom,
                    teePrev_RightBottom );

  TTeePreviewPage=class(TGraphicControl)
  private
    FAllowResize     : Boolean;
    FAllowMove       : Boolean;
    FAsBitmap        : Boolean;
    FImage           : TCustomTeePanel;
    FDragImage       : Boolean;
    FOnChangeMargins : TOnChangeMarginsEvent;
    FOldShowImage    : Boolean;
    FPaperColor      : TColor;
    FShowImage       : Boolean;
    FShowMargins     : Boolean;

    { internal }
    FDragged         : TeePreviewZones;
    OldX             : Integer;
    OldY             : Integer;
    OldRect          : TRect;
    ImageRect        : TRect;
    PaperRect        : TRect;
    Procedure SetShowMargins(Value:Boolean);
    Procedure SetImage(Value:TCustomTeePanel);
    Function GetPrintingBitmap:TBitmap;
  protected
    Procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Function CalcImagePrintMargins:TRect;
    Procedure DrawPaper(ACanvas:TCanvas);
    Procedure DrawBack(ACanvas:TCanvas);
    Procedure DrawImage(ACanvas:TCanvas);
    Procedure DrawMargins(ACanvas:TCanvas);
    Function WhereIsCursor(x,y:Integer):TeePreviewZones;
    Procedure Print;
  published
    property AllowResize:Boolean read FAllowResize write FAllowResize default True;
    property AllowMove:Boolean read FAllowMove write FAllowMove default True;
    property AsBitmap:Boolean read FAsBitmap write FAsBitmap;
    property DragImage:Boolean read FDragImage write FDragImage default False;
    property PaperColor:TColor read FPaperColor write FPaperColor;
    property ShowImage:Boolean read FShowImage write FShowImage default True;
    property ShowMargins:Boolean read FShowMargins write SetShowMargins default True;
    property OnChangeMargins:TOnChangeMarginsEvent read FOnChangeMargins
                                                   write FOnChangeMargins;
    property Image:TCustomTeePanel read FImage write SetImage;
  end;

  TChartPreview = class(TForm)
    Panel1: TPanel;
    Printers: TComboBox;
    Label1: TLabel;
    BSetupPrinter: TBitBtn;
    Panel2: TPanel;
    Orientation: TRadioGroup;
    GBMargins: TGroupBox;
    SETopMa: TEdit;
    SELeftMa: TEdit;
    SEBotMa: TEdit;
    SERightMa: TEdit;
    PrinterSetupDialog1: TPrinterSetupDialog;
    ShowMargins: TCheckBox;
    BReset: TButton;
    ChangeDetailGroup: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    BClose: TButton;
    Resolution: TScrollBar;
    BPrint: TButton;
    UDLeftMa: TUpDown;
    UDTopMa: TUpDown;
    UDRightMa: TUpDown;
    UDBotMa: TUpDown;
    CBProp: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure BSetupPrinterClick(Sender: TObject);
    procedure PrintersChange(Sender: TObject);
    procedure OrientationClick(Sender: TObject);
    procedure SETopMaChange(Sender: TObject);
    procedure SERightMaChange(Sender: TObject);
    procedure SEBotMaChange(Sender: TObject);
    procedure SELeftMaChange(Sender: TObject);
    procedure ShowMarginsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BResetClick(Sender: TObject);
    procedure ResolutionChange(Sender: TObject);
    procedure BPrintClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure CBPropClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm    : Boolean;
    ChangingMargins : Boolean;
    ChangingProp    : Boolean;
    Procedure ResetMargins;
  protected
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  public
    { Public declarations }
    PreviewPage     : TTeePreviewPage;
    OldMargins      : TRect;
    Procedure RefreshPage;
    Procedure RecalcControls;
    Procedure PreviewPageChangeMargins(Sender:TObject; DisableProportional:Boolean; Const NewMargins:TRect);
    procedure ChangeMargin(UpDown:TUpDown; Var APos:Integer; OtherSide:Integer);
  end;

Procedure ChartPreview(AOwner:TComponent; TeePanel:TCustomTeePanel);

implementation

{$R *.DFM}
Uses Printers, TeCanvas;

Const TeePreviewCursors:Array[0..9] of TCursor=
           ( crDefault, { none }
             crHSplit,
             crVSplit,
             crHSplit,
             crVSplit,
             crTeeHand,
             crSizeNWSE,
             crSizeNESW,
             crSizeNESW,
             crSizeNWSE );

Procedure ChartPreview(AOwner:TComponent; TeePanel:TCustomTeePanel);
Begin
  {$IFDEF TEEOCX}
  if (Printer.Printers.Count=0) or
     (Printer.PrinterIndex=-1) then ShowMessage(TeeMsg_OCXNoPrinter)
  else
  {$ENDIF}
  with TChartPreview.Create(AOwner) do
  try
    PreviewPage.Image:=TeePanel;
    ShowModal;
  finally
    Free;
    TeePanel.Repaint;
  end;
End;

{ TeePreviewPage }
Constructor TTeePreviewPage.Create(AOwner:TComponent);
Begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle+[csOpaque];
  FImage:=nil;
  FDragImage:=False;
  FShowMargins:=True;
  FShowImage:=True;
  FOldShowImage:=True;
  FDragged:=teePrev_None;
  FAsBitmap:=False;  { <-- as metafile by default }
  FPaperColor:=clWhite;
  FAllowResize:=True;
  FAllowMove:=True;
End;

Procedure TTeePreviewPage.Print;
var tmpBitmap:TBitmap;
Begin
  if Assigned(FImage) then
  Begin
    Screen.Cursor:=crHourGlass;
    try
      if FAsBitmap then
      begin
        Printer.Title:=FImage.Name;
        Printer.BeginDoc;
        try
          tmpBitmap:=GetPrintingBitmap;
          try
            Printer.Canvas.StretchDraw(FImage.ChartPrintRect,tmpBitmap);
          finally
            tmpBitmap.Free;
          end;
          Printer.EndDoc;
        except
        on Exception do
        begin
          Printer.Abort;
          if Printer.Printing then Printer.EndDoc;
          Raise;
        end;
        end;
      end
      else FImage.Print;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

Procedure TTeePreviewPage.DrawPaper(ACanvas:TCanvas);
Begin
  With ACanvas do
  Begin
    Pen.Style:=psSolid;
    Pen.Color:=clBlack;
    Brush.Color:=FPaperColor;
    Pen.Width:=1;
    With PaperRect do Rectangle(Left,Top,Right,Bottom);
    Brush.Color:=clBlack;
    With PaperRect do FillRect(Rect(Left+5,Bottom,Right+5,Bottom+4));
    With PaperRect do FillRect(Rect(Right,Top+4,Right+5,Bottom+4));
  end;
end;

Procedure TTeePreviewPage.SetShowMargins(Value:Boolean);
Begin
  FShowMargins:=Value;
  Invalidate;
End;

Procedure TTeePreviewPage.SetImage(Value:TCustomTeePanel);
Begin
  FImage:=Value;
  Invalidate;
End;

Procedure TTeePreviewPage.DrawBack(ACanvas:TCanvas);
Begin
  with ACanvas do
  Begin
    Pen.Style:=psClear;
    Brush.Style:=bsSolid;
    Brush.Color:=Self.Color;
    With ClientRect do Rectangle(Left,Top,Right,Bottom);
  end;
end;

Function TTeePreviewPage.CalcImagePrintMargins:TRect;
var PaperWidth  : Longint;
    PaperHeight : Longint;
begin
  With FImage.PrintMargins do
  begin
    RectSize(PaperRect,PaperWidth,PaperHeight);
    result.Left  :=PaperRect.Left  +MulDiv(Left  ,PaperWidth,100);
    result.Right :=PaperRect.Right -MulDiv(Right ,PaperWidth,100);
    result.Top   :=PaperRect.Top   +MulDiv(Top   ,PaperHeight,100);
    result.Bottom:=PaperRect.Bottom-MulDiv(Bottom,PaperHeight,100);
  end;
end;

Procedure TTeePreviewPage.Paint;
Var PrinterWidth  : Longint;
    PrinterHeight : Longint;

  Procedure CalcPaperRectangles;
  Var R         : TRect;
      tmpWidth  : Longint;
      tmpHeight : Longint;
  begin
    PrinterWidth := Printer.PageWidth;
    PrinterHeight:= Printer.PageHeight;

    R:=Rect(0,0,PrinterWidth,PrinterHeight);
    if PrinterWidth<PrinterHeight then
    Begin
      tmpHeight:=ClientHeight-Round(10.0*ClientHeight/100.0);
      if PrinterHeight>0 then
        tmpWidth:=MulDiv(tmpHeight,PrinterWidth,PrinterHeight)
      else
        tmpWidth:=ClientWidth;
      PaperRect.Left:=(ClientWidth-tmpWidth) div 2;
      PaperRect.Right:=PaperRect.Left+tmpWidth;
      PaperRect.Top:=Round(5.0*ClientHeight/100.0);
      PaperRect.Bottom:=PaperRect.Top+tmpHeight;
    end
    else
    Begin
      tmpWidth:=ClientWidth-Round(10.0*ClientWidth/100.0);
      if PrinterWidth>0 then
        tmpHeight:=MulDiv(tmpWidth,PrinterHeight,PrinterWidth)
      else
        tmpHeight:=ClientHeight;

      PaperRect.Top:=(ClientHeight-tmpHeight) div 2;
      PaperRect.Bottom:=PaperRect.Top+tmpHeight;
      PaperRect.Left:=Round(5.0*ClientWidth/100.0);
      PaperRect.Right:=PaperRect.Left+tmpWidth;
    end;
  end;

var tmp:TBitmap;
Begin
  inherited Paint;
  CalcPaperRectangles;
  ImageRect:=CalcImagePrintMargins;
  tmp:=TBitmap.Create;
  with tmp do
  try
    Width:=ClientRect.Right-ClientRect.Left;
    Height:=ClientRect.Bottom-ClientRect.Top;
    DrawBack(Canvas);
    DrawPaper(Canvas);
    if FShowMargins then DrawMargins(Canvas);
    DrawImage(Canvas);
    Self.Canvas.Draw(0,0,tmp);
  finally
    Free;
  end;
end;

Procedure TTeePreviewPage.DrawMargins(ACanvas:TCanvas);
Begin
  With ACanvas do
  Begin
    if (FPaperColor=clSilver) or (FPaperColor=clGray) then
       Pen.Color:=clWhite
    else
       Pen.Color:=clSilver;
    Pen.Style:=psDot;
    Pen.Width:=1;
    Brush.Style:=bsClear;
    Brush.Color:=FPaperColor;
    Pen.Mode:=pmNotXor;
    SetBKMode(Handle,Transparent);
    With ImageRect do
    Begin
      MoveTo(Left-1,PaperRect.Top);
      LineTo(Left-1,PaperRect.Bottom);

      MoveTo(Right+1,PaperRect.Top);
      LineTo(Right+1,PaperRect.Bottom);

      MoveTo(PaperRect.Left,Top-1);
      LineTo(PaperRect.Right,Top-1);

      MoveTo(PaperRect.Left,Bottom+1);
      LineTo(PaperRect.Right,Bottom+1);
    end;
    SetBKMode(Handle,Opaque);
    Pen.Style:=psSolid;
    Pen.Mode:=pmCopy;
  end;
end;

Function TTeePreviewPage.GetPrintingBitmap:TBitmap;
var tmpR      : TRect;
    WinWidth  : Longint;
    WinHeight : Longint;
    tmpW      : Longint;
    tmpH      : Longint;
    tmpWidth  : Longint;
    tmpHeight : Longint;
begin
  FImage.Printing:=True;
  tmpR:=ImageRect;
  With FImage.GetRectangle do
  begin
    tmpWidth:=Right-Left;
    tmpHeight:=Bottom-Top;
  end;
  FImage.CalcMetaBounds(tmpR,Rect(0,0,tmpWidth,tmpHeight),WinWidth,WinHeight,tmpW,tmpH);
  result:=FImage.TeeCreateBitmap(FPaperColor,Rect(0,0,WinWidth,WinHeight));
  FImage.Printing:=False;
end;

Procedure TTeePreviewPage.DrawImage(ACanvas:TCanvas);

    Procedure DrawAsBitmap;
    var tmpBitmap : TBitmap;
    begin
      tmpBitmap:=GetPrintingBitmap;
      try
        ACanvas.StretchDraw(ImageRect,tmpBitmap);
      finally
        tmpBitmap.Free;
      end;
    end;

    Procedure DrawAsMetafile;
    var tmpR      : TRect;
        tmpMeta   : TMetafile;
        WinWidth  : Longint;
        WinHeight : Longint;
        tmpW      : Longint;
        tmpH      : Longint;
    begin
      tmpR:=ImageRect;
      FImage.CalcMetaBounds(tmpR,FImage.GetRectangle,WinWidth,WinHeight,tmpW,tmpH);
      tmpMeta:=FImage.TeeCreateMetafile(True,Rect(0,0,WinWidth,WinHeight){tmpR});
      try
        ACanvas.StretchDraw(ImageRect,tmpMeta);
      finally
        tmpMeta.Free;
      end;
    end;

Begin
  FImage.Printing:=True;
  if FImage.CanClip then ClipCanvas(ACanvas,ImageRect)
                    else ClipCanvas(ACanvas,PaperRect);
  if FShowImage then if AsBitmap then DrawAsBitmap else DrawAsMetafile;
  UnClipCanvas(ACanvas);
  FImage.Printing:=False;
end;

Function TTeePreviewPage.WhereIsCursor(x,y:Integer):TeePreviewZones;
Const MinPixels=5;
var xLeft   : Longint;
    xRight  : Longint;
    yTop    : Longint;
    yBottom : Longint;
Begin
  With ImageRect do
  begin
    xLeft  :=Abs(x-Left);
    XRight :=Abs(x-Right);
    yTop   :=Abs(y-Top);
    yBottom:=Abs(y-Bottom);
    if (xLeft<MinPixels)  and (yTop<MinPixels)    then result:=teePrev_LeftTop else
    if (xLeft<MinPixels)  and (yBottom<MinPixels) then result:=teePrev_LeftBottom else
    if (xRight<MinPixels) and (yTop<MinPixels)    then result:=teePrev_RightTop else
    if (xRight<MinPixels) and (yBottom<MinPixels) then result:=teePrev_RightBottom else
    if xLeft<MinPixels   then result:=teePrev_Left else
    if xRight<MinPixels  then result:=teePrev_Right else
    if yTop<MinPixels    then result:=teePrev_Top else
    if yBottom<MinPixels then result:=teePrev_Bottom else
    if PtInRect(ImageRect,Point(x,y)) then
    begin
      if FAllowMove then
      begin
        result:=teePrev_Image;
        exit;
      end else result:=teePrev_None;
    end
    else result:=teePrev_None;
    if (result<>teePrev_None) and (not FAllowResize) then result:=teePrev_None;
  end;
End;

Procedure TTeePreviewPage.MouseMove(Shift: TShiftState; X, Y: Integer);
var tmpR        : TRect;
    PaperWidth  : Longint;
    PaperHeight : Longint;
begin
  inherited MouseMove(Shift,X,Y);
  if PtInRect(PaperRect,Point(x,y)) then
  Begin
    if FDragged=teePrev_None then
    begin
      Cursor:=TeePreviewCursors[Ord(WhereIsCursor(x,y))];
      Exit;
    end
    else
    begin
      if not FDragImage then DrawMargins(Canvas);
      Case FDragged of
        { sides }
        teePrev_Left   : if (x>=PaperRect.Left) and (x<ImageRect.Right) then ImageRect.Left:=x;
        teePrev_Top    : if (y>=PaperRect.Top) and (y<ImageRect.Bottom) then ImageRect.Top:=y;
        teePrev_Right  : if (x<=PaperRect.Right) and (x>ImageRect.Left) then ImageRect.Right:=x;
        teePrev_Bottom : if (y<=PaperRect.Bottom) and (y>ImageRect.Top) then ImageRect.Bottom:=y;
        teePrev_Image  : Begin
                           tmpR.Left  :=MaxLong(PaperRect.Left,OldRect.Left+(x-OldX));
                           tmpR.Top   :=MaxLong(PaperRect.Top,OldRect.Top+(y-OldY));
                           tmpR.Right :=MinLong(PaperRect.Right,tmpR.Left+(OldRect.Right-OldRect.Left));
                           tmpR.Bottom:=MinLong(PaperRect.Bottom,tmpR.Top+(OldRect.Bottom-OldRect.Top));
                           if PtInRect(PaperRect,tmpR.TopLeft) and
                              PtInRect(PaperRect,tmpR.BottomRight) then
                                ImageRect:=tmpR;
                         End;
        { corners }
       teePrev_LeftTop : if (x>=PaperRect.Left) and (x<ImageRect.Right) and
                            (y>=PaperRect.Top) and (y<ImageRect.Bottom) then
                         Begin
                           ImageRect.Left:=x;
                           ImageRect.Top:=y;
                         end;
    teePrev_LeftBottom : if (x>=PaperRect.Left) and (x<ImageRect.Right) and
                            (y<=PaperRect.Bottom) and (y>ImageRect.Top) then
                         Begin
                           ImageRect.Left:=x;
                           ImageRect.Bottom:=y;
                         end;
      teePrev_RightTop : if (x<=PaperRect.Right) and (x>ImageRect.Left) and
                            (y>=PaperRect.Top) and (y<ImageRect.Bottom) then
                         Begin
                           ImageRect.Right:=x;
                           ImageRect.Top:=y;
                         end;
   teePrev_RightBottom : if (x<=PaperRect.Right) and (x>ImageRect.Left) and
                            (y<=PaperRect.Bottom) and (y>ImageRect.Top) then
                         Begin
                           ImageRect.Right:=x;
                           ImageRect.Bottom:=y;
                         end;
      end;
      RectSize(PaperRect,PaperWidth,PaperHeight);
      With FImage.PrintMargins do
      Begin
        Left  :=MulDiv((ImageRect.Left-PaperRect.Left),100,PaperWidth);
        Right :=MulDiv((PaperRect.Right-ImageRect.Right),100,PaperWidth);
        Top   :=MulDiv((ImageRect.Top-PaperRect.Top),100,PaperHeight);
        Bottom:=MulDiv((PaperRect.Bottom-ImageRect.Bottom),100,PaperHeight);
      end;
      if Assigned(FOnChangeMargins) then FOnChangeMargins(Self,True,FImage.PrintMargins);
    end;
  end;
end;

procedure TTeePreviewPage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
  FDragged:=teePrev_None;
  Invalidate;
end;

procedure TTeePreviewPage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  FDragged:=WhereIsCursor(x,y);
  if FDragged=teePrev_Image then
  Begin
    OldX:=x;
    OldY:=y;
    OldRect:=ImageRect;
  end;
end;

Procedure TChartPreview.ResetMargins;
begin
  With PreviewPage do
  begin
    if Image.PrintProportional then
    begin
      OldMargins:=Image.CalcProportionalMargins;
      Image.PrintMargins:=OldMargins;
    end;
    RecalcControls;
    PreviewPageChangeMargins(Self,False,Image.PrintMargins);
  end;
end;

{ Form }
procedure TChartPreview.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  Printers.Items:=Printer.Printers;
  Printers.ItemIndex:=Printer.PrinterIndex;
  {$IFNDEF TEEOCX}
  Printer.Orientation:=poLandscape;
  {$ENDIF}
  With PreviewPage do
  begin
    CBProp.Checked:=Image.PrintProportional;
    Resolution.Position:=Image.PrintResolution;
    OldMargins:=Image.PrintMargins;
    ResetMargins;
  end;
  CreatingForm:=False;
end;

procedure TChartPreview.BSetupPrinterClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  Printers.Items:=Printer.Printers;
  Printers.ItemIndex:=Printer.PrinterIndex;
  RecalcControls;
end;

procedure TChartPreview.PrintersChange(Sender: TObject);
begin
  Printer.PrinterIndex:=Printers.ItemIndex;
  RecalcControls;
  OrientationClick(Self);
end;

procedure TChartPreview.OrientationClick(Sender: TObject);
begin
  Printer.Orientation:=TPrinterOrientation(Orientation.ItemIndex);
  ResetMargins;
  PreviewPage.Invalidate;
end;

procedure TChartPreview.ChangeMargin(UpDown:TUpDown; Var APos:Integer; OtherSide:Integer);
begin
  if not CreatingForm then
  begin
    if UpDown.Position+OtherSide<100 then
    begin
      APos:=UpDown.Position;
      RefreshPage;
    end
    else UpDown.Position:=APos;
  end;
end;

procedure TChartPreview.SETopMaChange(Sender: TObject);
begin
  with PreviewPage.Image.PrintMargins do ChangeMargin(UDTopMa,Top,Bottom);
end;

procedure TChartPreview.SERightMaChange(Sender: TObject);
begin
  with PreviewPage.Image.PrintMargins do ChangeMargin(UDRightMa,Right,Left);
end;

procedure TChartPreview.SEBotMaChange(Sender: TObject);
begin
  with PreviewPage.Image.PrintMargins do ChangeMargin(UDBotMa,Bottom,Top);
end;

procedure TChartPreview.SELeftMaChange(Sender: TObject);
begin
  with PreviewPage.Image.PrintMargins do ChangeMargin(UDLeftMa,Left,Right);
end;

procedure TChartPreview.WMEraseBkgnd(var Message: TWmEraseBkgnd);
Begin
  if TeeEraseBack then Inherited;
  Message.Result:=1;
End;

Procedure TChartPreview.RefreshPage;
Begin
  if not ChangingMargins then
  With PreviewPage do
  Begin
    Invalidate;
    BReset.Enabled:=not EqualRect(Image.PrintMargins,OldMargins);
  end;
end;

procedure TChartPreview.ShowMarginsClick(Sender: TObject);
begin
  PreviewPage.ShowMargins:=ShowMargins.Checked;
end;

procedure TChartPreview.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
  ChangingMargins:=True;
  ChangingProp:=False;
  PreviewPage:=TTeePreviewPage.Create(Self);
  With PreviewPage do
  begin
    Parent:=Self;
    Align:=alClient;
    OnChangeMargins:=PreviewPageChangeMargins;
  end;
end;

procedure TChartPreview.BResetClick(Sender: TObject);
begin
  With PreviewPage do
  Begin
    Image.PrintMargins:=OldMargins;
    PreviewPageChangeMargins(Self,False,Image.PrintMargins);
  end;
end;

Procedure TChartPreview.PreviewPageChangeMargins(Sender:TObject; DisableProportional:Boolean; Const NewMargins:TRect);
Begin
  ChangingMargins:=True;
  try
    UDLeftMa.Position :=NewMargins.Left;
    UDRightMa.Position:=NewMargins.Right;
    UDTopMa.Position  :=NewMargins.Top;
    UDBotMa.Position  :=NewMargins.Bottom;
    if DisableProportional then
    begin
      PreviewPage.Image.PrintProportional:=False;
      ChangingProp:=True;
      CBProp.Checked:=False;
      ChangingProp:=False;
    end;
  finally
    ChangingMargins:=False;
    if PreviewPage.DragImage or (not DisableProportional) then
       RefreshPage
    else
       PreviewPage.DrawMargins(PreviewPage.Canvas);
  end;
end;

Procedure TChartPreview.RecalcControls;
Begin
  Orientation.ItemIndex:=Ord(Printer.Orientation);
End;

procedure TChartPreview.ResolutionChange(Sender: TObject);
begin
  With PreviewPage do
  Begin
    Image.PrintResolution:=Resolution.Position;
    Invalidate;
  end;
  RecalcControls;
end;

procedure TChartPreview.BPrintClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    PreviewPage.Print;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TChartPreview.FormDestroy(Sender: TObject);
begin
  PreviewPage.Free;
end;

procedure TChartPreview.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TChartPreview.CBPropClick(Sender: TObject);
begin
  if not ChangingProp then
  begin
    PreviewPage.Image.PrintProportional:=CBProp.Checked;
    ResetMargins;
  end;
end;

end.
