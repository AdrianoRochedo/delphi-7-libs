{*****************************************}
{   TeeChart-Pro 4.0                      }
{   Copyright (c) 1995-98 David Berneda   }
{     TChartEditor                        }
{     TChartPreviewer                     }
{*****************************************}
{$I teedefs.inc}
unit TeeEdit;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Chart,
  {$IFDEF D1}
  IEdit16
  {$ELSE}
  IEditCha
  {$ENDIF}
  ;

type
  TCustomChartEditor=class(TComponent)
  private
    { Private declarations }
    FChart   : TCustomChart;
    FTitle   : String;
    FOnClose : TNotifyEvent;
    FOnShow  : TNotifyEvent;
    procedure SetChart(const Value: TCustomChart);
  protected
    { Protected declarations }
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
  public
    { Public declarations }
    Procedure Execute; virtual; abstract;
    property Title:String read FTitle write FTitle;
  published
    { Published declarations }
    property Chart:TCustomChart read FChart write SetChart;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TChartEditor = class(TCustomChartEditor)
  private
    { Private declarations }
    FAutoRepaint : Boolean;
    FDefaultTab  : TChartEditorTab;
    FHideTabs    : TChartEditorHiddenTabs;
    FOptions     : TChartEditorOptions;
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
    Procedure Execute; override;
  published
    { Published declarations }
    property AutoRepaint:Boolean read FAutoRepaint write FAutoRepaint default True;
    property DefaultTab:TChartEditorTab read FDefaultTab write FDefaultTab default cetMain;
    property HideTabs:TChartEditorHiddenTabs read FHideTabs write FHideTabs;
    property Options:TChartEditorOptions read FOptions write FOptions
                                         default eoAll;
    property Title;
  end;

  TChartPreviewOption=( cpoChangePrinter,
                        cpoSetupPrinter,
                        cpoResizeChart,
                        cpoMoveChart,
                        cpoChangeDetail,
                        cpoChangePaperOrientation,
                        cpoChangeMargins,
                        cpoProportional,
                        cpoDragChart,
                        cpoPrintPanel,
                        cpoAsBitmap );

  TChartPreviewOptions=set of TChartPreviewOption;

Const DefaultChartPreviewOptions=[ cpoChangePrinter,
                                   cpoSetupPrinter,
                                   cpoResizeChart,
                                   cpoMoveChart,
                                   cpoChangeDetail,
                                   cpoChangePaperOrientation,
                                   cpoChangeMargins,
                                   cpoProportional ];


type
  TChartPreviewer = class(TCustomChartEditor)
  private
    FOptions    : TChartPreviewOptions;
    FPaperColor : TColor;
    FWindowState: TWindowState;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
    Procedure Execute; override;
  published
    { Published declarations }
    property Options:TChartPreviewOptions read FOptions write FOptions
             default DefaultChartPreviewOptions;
    property PaperColor:TColor read FPaperColor write FPaperColor default clWhite;
    property Title;
    property WindowState:TWindowState read FWindowState write FWindowState default wsNormal;
  end;

implementation

Uses TeeProCo,TeeProcs,EditChar,TeePrevi;

procedure TCustomChartEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
     if Assigned(FChart) and (AComponent=FChart) then
        FChart:=nil;
end;

procedure TCustomChartEditor.SetChart(const Value: TCustomChart);
begin
  FChart := Value;
  {$IFNDEF D1}
  if Assigned(FChart) then FChart.FreeNotification(Self);
  {$ENDIF}
end;

{ TChartEditor }
Constructor TChartEditor.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FOptions:=eoAll;
  FDefaultTab:=cetMain;
  FAutoRepaint:=True;
end;

procedure TChartEditor.Execute;
Var TheForm : TChartEditForm;
    OldAuto : Boolean;
begin
  if Assigned(FChart) then
  begin
    TheForm:=TChartEditForm.Create(nil);
    With TheForm do
    try
      Caption:=Self.FTitle;
      TheChart:=FChart;
      EditorOptions:=FOptions;
      {$IFDEF TEEHELPEDITOR}
      CheckHelpFile;
      {$ENDIF}
      TheActivePageIndex:=Ord(FDefaultTab);
      TheHiddenTabs:=FHideTabs;
      if Assigned(Self.FOnShow) then Self.FOnShow(TheForm);
      OldAuto:=True; 
      if not Self.FAutoRepaint then
      begin
        OldAuto:=FChart.AutoRepaint;
        FChart.AutoRepaint:=False;
      end;
      ShowModal;
      if not Self.FAutoRepaint then
      begin
        FChart.AutoRepaint:=OldAuto;
        FChart.Repaint;
      end;
    finally
      if Assigned(Self.FOnClose) then Self.FOnClose(TheForm);
      Free;
    end;
    FChart.CancelMouse:=True;
  end;
end;

{ TChartPreviewer }
Constructor TChartPreviewer.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FOptions:=DefaultChartPreviewOptions;
  FPaperColor:=clWhite;
  FWindowState:=wsNormal;
end;

procedure TChartPreviewer.Execute;
var OldPrintTeePanel:Boolean;
    TheForm:TChartPreview;
begin
  if Assigned(FChart) then
  begin
    TheForm:=TChartPreview.Create(nil);
    with TheForm do
    try
      if Self.FTitle<>'' then Caption:=Self.FTitle;
      PreviewPage.Image:=Self.FChart;
      WindowState:=Self.FWindowState;
      if not (cpoChangePrinter in Self.FOptions) then Printers.Enabled:=False;
      if not (cpoSetupPrinter in Self.FOptions) then BSetupPrinter.Enabled:=False;
      PreviewPage.AllowResize:=(cpoResizeChart in Self.FOptions);
      PreviewPage.AllowMove:=(cpoMoveChart in Self.FOptions);
      if not (cpoChangeDetail in Self.FOptions) then ChangeDetailGroup.Enabled:=False;
      if not (cpoChangePaperOrientation in Self.FOptions) then Orientation.Enabled:=False;
      if not (cpoChangeMargins in Self.FOptions) then
      begin
        GBMargins.Enabled:=False;
        BReset.Enabled:=False;
      end;
      PreviewPage.Image.PrintProportional:=(cpoProportional in Self.FOptions);
      PreviewPage.PaperColor:=Self.FPaperColor;
      PreviewPage.DragImage:=(cpoDragChart in Self.FOptions);
      OldPrintTeePanel:=PrintTeePanel;
      PrintTeePanel:=(cpoPrintPanel in Self.FOptions);
      PreviewPage.AsBitmap:=(cpoAsBitmap in Self.FOptions);
      if Assigned(Self.FOnShow) then Self.FOnShow(TheForm);
      ShowModal;
      PrintTeePanel:=OldPrintTeePanel;
    finally
      if Assigned(Self.FOnClose) then Self.FOnClose(TheForm);
      Free;
      Self.FChart.Repaint;
    end;
    Self.FChart.CancelMouse:=True;
  end;
end;

end.
