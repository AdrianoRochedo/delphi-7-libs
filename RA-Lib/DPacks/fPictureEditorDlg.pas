{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit fPictureEditorDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtDlgs, ExtCtrls, StdCtrls, RARegAuto, ExptIntf, Clipbrd;

type
  TPictureEditorDlg = class(TForm)
    bClear: TButton;
    bSave: TButton;
    bLoad: TButton;
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    ImagePanel: TPanel;
    ImagePaintBox: TImage;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    bCopy: TButton;
    bPaste: TButton;
    RegAuto1: TRegAuto;
    procedure bLoadClick(Sender: TObject);
    procedure bClearClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bCopyClick(Sender: TObject);
    procedure bPasteClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    Picture: TObject;
    procedure UpdatePasteEnabled;
  end;

  function EditPictureProperty(APicture: TObject): Boolean;

implementation

uses RAUtils, packconst;

{$R *.dfm}

var
  CF_ICON : Integer = 0;

function EditPictureProperty(APicture: TObject): Boolean;
begin
  with TPictureEditorDlg.Create(Application) do
    try
      Picture := APicture;
      if Picture is TPicture then
      begin
        ImagePaintBox.Picture := Picture as TPicture;
      end
      else if Picture is TBitmap then
      begin
        ImagePaintBox.Picture.Bitmap := Picture as TBitmap;
        OpenDialog.Filter := 'Bitmaps (*.bmp)|*.bmp';
        OpenDialog.DefaultExt := '.bmp';
      end
      else if Picture is TIcon then
      begin
        ImagePaintBox.Picture.Icon := Picture as TIcon;
        OpenDialog.Filter := 'Icons (*.ico)|*.ico';
        OpenDialog.DefaultExt := '.ico';
      end;
      UpdatePasteEnabled;
      SaveDialog.Filter := OpenDialog.Filter;
      SaveDialog.DefaultExt := OpenDialog.DefaultExt;
      Result := ShowModal = mrOK;
      if Result then
        if Picture is TPicture then
          (Picture as TPicture).Assign(ImagePaintBox.Picture)
        else if Picture is TBitmap then
          (Picture as TBitmap).Assign(ImagePaintBox.Picture.Bitmap)
        else if Picture is TIcon then
          (Picture as TIcon).Assign(ImagePaintBox.Picture.Icon)
    finally
      Free;
    end;    { try/finally }
end;    { EditPictureProperty }

procedure TPictureEditorDlg.UpdatePasteEnabled;
begin
  if Picture is TPicture then
  begin
    bPaste.Enabled := ClipBoard.HasFormat(CF_BITMAP) or
      ClipBoard.HasFormat(CF_ICON);
  end
  else if Picture is TBitmap then
  begin
    bPaste.Enabled := ClipBoard.HasFormat(CF_BITMAP);
  end
  else if Picture is TIcon then
  begin
    bPaste.Enabled := ClipBoard.HasFormat(CF_ICON);
  end;
end;    {  }

procedure TPictureEditorDlg.bLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    if Picture is TPicture then
      ImagePaintBox.Picture.LoadFromFile(OpenDialog.FileName)
    else if Picture is TBitmap then
      ImagePaintBox.Picture.Bitmap.LoadFromFile(OpenDialog.FileName)
    else if Picture is TIcon then
      ImagePaintBox.Picture.Icon.LoadFromFile(OpenDialog.FileName)
end;

procedure TPictureEditorDlg.bSaveClick(Sender: TObject);
begin
  SaveDialog.FileName := OpenDialog.FileName;
  if SaveDialog.Execute then
    if Picture is TPicture then
      ImagePaintBox.Picture.SaveToFile(SaveDialog.FileName)
    else if Picture is TBitmap then
      ImagePaintBox.Picture.Bitmap.SaveToFile(SaveDialog.FileName)
    else if Picture is TIcon then
      ImagePaintBox.Picture.Icon.SaveToFile(SaveDialog.FileName);
  OpenDialog.FileName := SaveDialog.FileName;
end;

procedure TPictureEditorDlg.bClearClick(Sender: TObject);
begin
  if Picture is TPicture then
    ImagePaintBox.Picture := nil
  else if Picture is TBitmap then
    ImagePaintBox.Picture.Bitmap := nil
  else if Picture is TIcon then
    ImagePaintBox.Picture.Icon := nil;
end;

procedure TPictureEditorDlg.FormCreate(Sender: TObject);
begin
  bOK.Caption      := SOK    ;
  bCancel.Caption  := SCancel;
  bLoad.Caption    := SPELoad  ;
  bSave.Caption    := SPESave  ;
  bClear.Caption   := SPEClear ;
  bCopy.Caption    := SPECopy  ;
  bPaste.Caption   := SPEPaste ;
  if Assigned(ToolServices) then
    RegAuto1.RegPath := ToolServices.GetBaseRegistryKey +
      '\Property Editors\Picture Editor'
  else
    RegAuto1.UseReg := False;
end;

procedure TPictureEditorDlg.bCopyClick(Sender: TObject);
var
  S: TMemoryStream;
begin
  if Picture is TPicture then
    Clipboard.Assign(ImagePaintBox.Picture)
  else if Picture is TBitmap then
    Clipboard.Assign(ImagePaintBox.Picture.Bitmap)
  else if Picture is TIcon then
  begin
    S := TMemoryStream.Create;
    try
      ImagePaintBox.Picture.Icon.SaveToStream(S);
      MemStreamToClipBoard(S, CF_ICON);
    finally
      S.Free;
    end;
  end;
  UpdatePasteEnabled;  
end;

procedure TPictureEditorDlg.bPasteClick(Sender: TObject);
var
  S: TMemoryStream;
begin
  if Picture is TPicture then
    ImagePaintBox.Picture.Assign(Clipboard)
  else if Picture is TBitmap then
    ImagePaintBox.Picture.Bitmap.Assign(Clipboard)
  else if Picture is TIcon then
  begin
    S := TMemoryStream.Create;
    try
      ClipBoardToMemStream(S, CF_ICON);
      ImagePaintBox.Picture.Icon.LoadFromStream(S);
    finally
      S.Free;
    end;
  end;
end;


procedure TPictureEditorDlg.FormActivate(Sender: TObject);
begin
  UpdatePasteEnabled;
end;

initialization
  CF_ICON := RegisterClipboardFormat('Icon');
end.
