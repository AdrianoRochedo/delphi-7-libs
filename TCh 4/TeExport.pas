{**********************************************}
{   TeeChart Export Dialog                     }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeExport;

interface

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TeeProcs
  {$IFDEF TEEJPEG}
  ,JPEG
  {$ENDIF}
  ;

type
  TTeeExportForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RGFormat: TRadioGroup;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGFormatClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ExportPanel:TCustomTeePanel;
  end;

Procedure LoadTeeFromFile( Var ATree:TCustomTeePanel; Const AName:String);
Procedure SaveTeeToFile(ATree:TCustomTeePanel; Const AName:String);

type TExportSaveProc=procedure(ExportPanel:TCustomTeePanel; Const AFileName:String);

Const TeeExportSave:TExportSaveProc=nil;

{$IFDEF TEEJPEG}
procedure TeeSaveToJPEGFile( APanel:TCustomTeePanel;
                             const FileName: WideString;
                             Gray: WordBool;
                             Performance: TJPEGPerformance;
                             Quality, AWidth, AHeight: Integer);
{$ENDIF}

implementation

{$R *.DFM}
{$IFDEF D1}
Uses TeeConst;
{$ELSE}
Uses TeeConst
     {$IFDEF D3}
     ,ExtDlgs
     {$ENDIF}
     ;
{$ENDIF}

Procedure SaveTeeToStream(ATree:TCustomTeePanel; AStream:TStream);
{var Writer : TWriter;}
begin
  AStream.WriteComponent(ATree);
{  Writer := TWriter.Create(AStream, 4096);
  With Writer do
  try
    RootAncestor:=nil;
    Ancestor:= nil;
    Root := ATree.Owner;
    WriteSignature;
    WriteComponent(ATree);
  finally
    Writer.Free;
  end;}
end;

Procedure SaveTeeToFile(ATree:TCustomTeePanel; Const AName:String);
Var tmp : TFileStream;
    OldVisible : Boolean;
begin
  tmp:=TFileStream.Create(AName,fmCreate);
  try
    OldVisible:=ATree.Visible;
    ATree.Visible:=True;
    try
      SaveTeeToStream(ATree,tmp);
    finally
      ATree.Visible:=OldVisible;
    end;
  finally
    tmp.Free;
  end;
end;

Procedure LoadTeeFromStream( Var ATree:TCustomTeePanel; AStream:TStream);
begin
  AStream.ReadComponent(ATree);
end;

Procedure LoadTeeFromFile( Var ATree:TCustomTeePanel; Const AName:String);
Var tmp:TFileStream;
begin
  tmp:=TFileStream.Create(AName,fmOpenRead);
  try
    LoadTeeFromStream(ATree,tmp);
  finally
    tmp.Free;
  end;
end;

{ Export Dialog }
procedure TTeeExportForm.Button1Click(Sender: TObject);
begin
  With ExportPanel do
  Case RGFormat.ItemIndex of
    1: CopyToClipboardMetafile(False);
    2: CopyToClipboardMetafile(True);
  else
    CopyToClipboardBitmap;
  end;
end;

{$IFDEF TEEJPEG}
Function TeeGetJPEGImageParams( APanel:TCustomTeePanel;
                                Params:TJPEGDefaults;
                                AWidth,AHeight:Integer):TJPEGImage;
var tmpBitmap : TBitmap;
    OldWidth,
    OldHeight : Integer;
begin
  result:=TJPEGImage.Create;   { <-- create a TJPEGImage }
  OldWidth:=APanel.Width;
  OldHeight:=APanel.Height;
  tmpBitmap:=TBitmap.Create;   { <-- create a temporary TBitmap }
  try
    tmpBitmap.Width :=AWidth;   { <-- set the bitmap dimensions }
    tmpBitmap.Height:=AHeight;
    { draw the Chart on the temporary Bitmap... }
    APanel.Draw(tmpBitmap.Canvas,Rect(0,0,tmpBitmap.Width,tmpBitmap.Height));
    APanel.Width:=OldWidth;
    APanel.Height:=OldHeight;
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

{ This function creates a JPEG Image, sets the desired JPEG
  parameters, draws a Chart (or TeeTree) on it, Saves the JPEG on disk and
  Loads the JPEG from disk to show it on this Form. }
Function TeeGetJPEGImage(APanel:TCustomTeePanel):TJPEGImage;
Var Params:TJPEGDefaults;
begin
  With Params do
  begin
    GrayScale:=False;
    ProgressiveEncoding:=False;
    CompressionQuality:=90;
    PixelFormat:=jf24bit;
    ProgressiveDisplay:=False;
    Performance:=jpBestQuality;
    Scale:=jsFullSize;
    Smoothing:=True;
  end;
  { Create the JPEG with the Chart image }
  result:=TeeGetJPEGImageParams(APanel,Params,APanel.Width,APanel.Height);
end;

procedure TeeSaveToJPEGFile( APanel:TCustomTeePanel;
                             const FileName: WideString;
                             Gray: WordBool;
                             Performance: TJPEGPerformance;
                             Quality, AWidth, AHeight: Integer);
var tmp:String;
    tmpWidth,tmpHeight:Integer;
    Params:TJPEGDefaults;
begin
  tmp:=FileName;
  if Pos('.',tmp)=0 then tmp:=tmp+'.jpg';
  { Get the JPEG params }
  Params.GrayScale:=Gray;
  Params.CompressionQuality:=Quality;
  Params.Performance:=Performance;
  if AWidth=0 then tmpWidth:=APanel.Width
              else tmpWidth:=AWidth;
  if AHeight=0 then tmpHeight:=APanel.Height
               else tmpHeight:=AHeight;
  { Create the JPEG with the Chart image }
  With TeeGetJPEGImageParams(APanel,Params,tmpWidth,tmpHeight) do
  try
    SaveToFile(tmp);    { <-- save the JPEG to disk }
  finally
    Free;  { <-- free the temporary JPEG object }
  end;
end;
{$ENDIF}

procedure TTeeExportForm.Button2Click(Sender: TObject);
begin
  With SaveDialog1 do
  begin
    DefaultExt:=TeeGetImageExtension(RGFormat.ItemIndex);
    FilterIndex:=1+Ord(RGFormat.ItemIndex);
    FileName:='';
    if Execute then
    With ExportPanel do
    Case FilterIndex-1 of
        0: SaveToBitmapFile(FileName);
        1: SaveToMetafile(FileName);
        2: SaveToMetafileEnh(FileName);
        3: if Assigned(TeeExportSave) then
              TeeExportSave(ExportPanel,FileName)
           else
              SaveTeeToFile(ExportPanel,FileName);
    {$IFDEF TEEJPEG}
        4: TeeSaveToJPEGFile(ExportPanel,FileName,False,jpBestQuality,95,0,0);
    {$ENDIF}
    end;
  end;
end;

procedure TTeeExportForm.FormCreate(Sender: TObject);
begin
  {$IFDEF TEEJPEG}
  Height:=Height+32;
  With RGFormat do
  begin
    Height:=Height+32;
    Items.Add(TeeMsg_AsJPEG);
  end;
  {$ENDIF}
end;

procedure TTeeExportForm.RGFormatClick(Sender: TObject);
begin
  Button1.Enabled:=RGFormat.ItemIndex<>3;
end;

end.

