{****************************************}
{       TeeChart Charting Library        }
{          For Delphi 1, 2 & 3           }
{           & C++ Builder 1              }
{ Copyright (c) 1996-97 by David Berneda }
{          All Rights Reserved           }
{****************************************}
unit TeeJPEG;

{ This unit shows an example of EXPORTING a TeeChart component to
  a graphic FILE.

  Bitmap and Metafile image formats are supported in Delphi 1,2 and 3.
  JPEG format is supported in Delphi 3.0 only.

  INSTALLATION:

  BEFORE running this project, you should do the following in Delphi 3.0:

  Component --> Install Component --> Unit file Name:

  \Delphi 3.0\Lib\JPEG.DCU

  and recompile and install the selected PACKAGE.

}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, TeEngine, Series, TeeProcs, Chart, Spin,
  ComCtrls;

type
  TForm1 = class(TForm)
    EditFilePath: TEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Image1: TImage;
    ButtonEdit: TBitBtn;
    Series1: TBarSeries;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    CheckBoxGrayScale: TCheckBox;
    CheckBoxProgressiveEncoding: TCheckBox;
    Label1: TLabel;
    CheckBoxProgressiveDisplay: TCheckBox;
    CheckBoxSmoothing: TCheckBox;
    RadioPixelFormat: TRadioGroup;
    RadioPerformance: TRadioGroup;
    RadioScale: TRadioGroup;
    TrackQuality: TTrackBar;
    Series2: TLineSeries;
    Label3: TLabel;
    LabelFileSize: TLabel;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    TabSheet3: TTabSheet;
    Image2: TImage;
    Image3: TImage;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxGrayScaleClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshBMPImage;
    procedure RefreshJPEGImage;
    procedure RefreshMetaImage;
    Function ImageFileName:String;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
uses JPEG,               { <-- This unit comes with Delphi 3.0 }
     EditChar;           { <-- TeeChart unit necessary to show the Chart Editor }

{ This function returns a FILE SIZE in BYTES }
Function SizeFile(Const FileName:String):Integer;
var f:File of byte;
begin
  AssignFile(f,FileName);
  Reset(f);
  result:=FileSize(f);
  CloseFile(f);
end;

{ This function creates a TJPEGImage and draws a TeeChart on it }
Function GetChartJPEG(AChart:TCustomChart; Params:TJPEGDefaults):TJPEGImage;
var tmpBitmap:TBitmap;
begin
  result:=TJPEGImage.Create;   { <-- create a TJPEGImage }
  tmpBitmap:=TBitmap.Create;   { <-- create a temporary TBitmap }
  try
    tmpBitmap.Width :=AChart.Width;   { <-- set the bitmap dimensions }
    tmpBitmap.Height:=AChart.Height;
    { draw the Chart on the temporary Bitmap... }
    AChart.Draw(tmpBitmap.Canvas,Rect(0,0,tmpBitmap.Width,tmpBitmap.Height));
    { set the desired JPEG options... }
    With result do
    begin
      GrayScale            :=Params.GrayScale;
      ProgressiveEncoding  :=Params.ProgressiveEncoding;
      CompressionQuality   :=Params.CompressionQuality;
      PixelFormat          :=Params.PixelFormat;
      ProgressiveDisplay   :=Params.ProgressiveDisplay;
      Performance          :=Params.Performance;
      Scale                :=Params.Scale;
      Smoothing            :=Params.Smoothing;
      { Copy the temporary Bitmap onto the JPEG image... }
      Assign(tmpBitmap);
    end;
  finally
    tmpBitmap.Free;  { <-- free the temporary Bitmap }
  end;
end;

{ Create a Bitmap, draw the Chart on it and Save the Bitmap }
procedure TForm1.RefreshBMPImage;
begin
  With TBitmap.Create do
  try
    Width:=Chart1.Width;   { <-- set the bitmap dimensions }
    Height:=Chart1.Height;
    Chart1.Draw(Canvas,Rect(0,0,Width,Height)); { <-- draw the Chart }
    SaveToFile(ImageFileName);  { <-- save the bitmap to disk }
  finally
    Free;
  end;
end;

{ This function creates a Metafile with a Chart image on it and
  Saves the Metafile to disk }
procedure TForm1.RefreshMetaImage;
var tmpMeta:TMetafile;
begin
  { Create the Metafile with the desired dimensions }
  With Chart1 do tmpMeta:=TeeCreateMetafile(True,Rect(0,0,Width,Height));
  try
    tmpMeta.SaveToFile(ImageFileName);   { <-- save the metafile to disk }
    Image2.Picture.LoadFromFile(ImageFileName);   { <-- load the metafile }
  finally
    tmpMeta.Free;  { <-- free the temporary Metafile }
  end;
end;

{ This function creates a JPEG Image, sets the desired JPEG
  parameters, draws a Chart on it, Saves the JPEG on disk and
  Loads the JPEG from disk to show it on this Form. }
procedure TForm1.RefreshJPEGImage;
Var Params:TJPEGDefaults;
begin
  Screen.Cursor:=crHourGlass;
  try
    { Get the JPEG params }
    With Params do
    begin
      GrayScale:=CheckBoxGrayScale.Checked;
      ProgressiveEncoding:=CheckBoxProgressiveEncoding.Checked;
      CompressionQuality:=TrackQuality.Position;
      if RadioPixelFormat.ItemIndex=0 then PixelFormat:=jf24bit
                                      else PixelFormat:=jf8bit;
      ProgressiveDisplay:=CheckBoxProgressiveDisplay.Checked;
      if RadioPerformance.ItemIndex=0 then Performance:=jpBestQuality
                                      else Performance:=jpBestSpeed;
      Case RadioScale.ItemIndex of
         0: Scale:=jsFullSize;
         1: Scale:=jsHalf;
         2: Scale:=jsQuarter;
         3: Scale:=jsEighth;
      end;
      Smoothing:=CheckBoxSmoothing.Checked;
    end;
    { Create the JPEG with the Chart image }
    With GetChartJPEG(Chart1,Params) do
    try
      SaveToFile(ImageFileName);    { <-- save the JPEG to disk }
      Image1.Picture.LoadFromFile(ImageFileName);  { <-- Load the JPEG }
    finally
      Free;  { <-- free the temporary JPEG object }
    end;
    { refresh the saved File Size }
    LabelFileSize.Caption:=IntToStr(SizeFile(ImageFileName));
  finally
    Screen.Cursor:=crDefault;
  end;
end;

{ Fill the TeeChart with some random values and refresh the JPEG image }
procedure TForm1.FormCreate(Sender: TObject);
begin
  Series1.FillSampleValues(8);
  Series2.FillSampleValues(28);
  PageControl1.ActivePage:=TabSheet1;
  PageControl1Change(Self);
end;

procedure TForm1.CheckBoxGrayScaleClick(Sender: TObject);
begin
  RefreshJPEGImage;
end;

procedure TForm1.ButtonEditClick(Sender: TObject);
begin
  EditChart(Self,Chart1);
  RefreshJPEGImage;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

{ This function returns a temporary file name to save the Chart
  image with the correct File extension }
Function TForm1.ImageFileName:String;
Var Extension:String;
begin
  With PageControl1 do
  begin
    if ActivePage=TabSheet1 then Extension:='JPG' else
    if ActivePage=TabSheet2 then Extension:='BMP' else
    if ActivePage=TabSheet3 then Extension:='EMF';
  end;
  result:=EditFilePath.Text+'teechart.'+Extension;
end;

{ Refresh the Chart image and controls }
procedure TForm1.PageControl1Change(Sender: TObject);
begin
  With PageControl1 do
  begin
    ButtonEdit.Visible:=ActivePage=TabSheet2;
    if ActivePage=TabSheet1 then RefreshJPEGImage else
    if ActivePage=TabSheet2 then RefreshBMPImage else
    if ActivePage=TabSheet3 then RefreshMetaImage;
    LabelFileSize.Caption:=IntToStr(SizeFile(ImageFileName));
    GroupBox1.Visible:=ActivePage=TabSheet1;
  end;
end;

end.
