{**********************************************}
{   TImageBarSeries Component Editor Dialog    }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeeImaEd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Chart, Series, ImageBar
  {$IFDEF D1}
  , TeeUpDow, Spin
  {$ELSE}
  , ComCtrls
  ;{$ENDIF}

type
  TImageBarSeriesEditor = class(TForm)
    GroupBox1: TGroupBox;
    Image1: TImage;
    BBrowse: TButton;
    CBTiled: TCheckBox;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BBrowseClick(Sender: TObject);
    procedure CBTiledClick(Sender: TObject);
  private
    { Private declarations }
    CreatingForm:Boolean;
    procedure EnableImageControls;
  public
    { Public declarations }
    ImageBarSeries:TImageBarSeries;
  end;

implementation

{$R *.DFM}
Uses PenDlg,BrushDlg,TeeConst,BarEdit
     {$IFNDEF D1}
     ,IEdiSeri,IEdiPane
     {$ENDIF}
     {$IFDEF D3}
     ,ExtDlgs
     {$ENDIF}
     ;

procedure TImageBarSeriesEditor.FormShow(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
  ImageBarSeries:=TImageBarSeries(Tag);
  With ImageBarSeries do
  begin
    CBTiled.Checked:=ImageTiled;
    Image1.Picture.Assign(Image);
  end;
  {$IFNDEF D1}
  With (Parent.Owner as TFormTeeSeries).InsertSeriesForm( TBarSeriesEditor,
                                                          1,TeeMsg_GalleryBar,
                                                          ImageBarSeries) as
                                                          TBarSeriesEditor do
  begin
    LStyle.Visible:=False;
    CBBarStyle.Visible:=False;
  end;
  {$ENDIF}
  EnableImageControls;
  CreatingForm:=False;
end;

procedure TImageBarSeriesEditor.EnableImageControls;
begin
  CBTiled.Enabled:=(ImageBarSeries.Image.Graphic<>nil);
  if CBTiled.Enabled then
     BBrowse.Caption:=TeeMsg_ClearImage
  else
     BBrowse.Caption:=TeeMsg_BrowseImage;
  Image1.Picture.Assign(ImageBarSeries.Image);
end;

procedure TImageBarSeriesEditor.FormCreate(Sender: TObject);
begin
  CreatingForm:=True;
end;

procedure TImageBarSeriesEditor.BBrowseClick(Sender: TObject);
begin
  TeeLoadClearImage(Self,ImageBarSeries.Image);
  EnableImageControls;
end;

procedure TImageBarSeriesEditor.CBTiledClick(Sender: TObject);
begin
  ImageBarSeries.ImageTiled:=CBTiled.Checked;
end;

initialization
  RegisterClass(TImageBarSeriesEditor);
end.
