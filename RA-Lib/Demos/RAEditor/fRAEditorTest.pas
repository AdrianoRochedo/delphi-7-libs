unit fRAEditorTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RAEditor, IParser, StdCtrls, ExtCtrls, ComCtrls, RARegAuto, RAHLEditor
  {$IFDEF RA_D4H}, ImgList {$ENDIF RA_D4H};

type
  TEditor = class(TForm)
    GutterImages: TImageList;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    RAEditor: TRAHLEditor;
    TabSheet2: TTabSheet;
    RAEditor1: TRAHLEditor;
    TabSheet3: TTabSheet;
    RAEditor2: TRAHLEditor;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    RAEditor3: TRAHLEditor;
    RAEditor4: TRAEditor;
    Panel1: TPanel;
    Label1: TLabel;
    ilCompletions: TImageList;
    RegAuto1: TRegAuto;
    TabSheet6: TTabSheet;
    RAHLEditor1: TRAHLEditor;
    TabSheet7: TTabSheet;
    RAHLEditor2: TRAHLEditor;
    TabSheet8: TTabSheet;
    RAHLEditor3: TRAHLEditor;
    TabSheet9: TTabSheet;
    RAHLEditor4: TRAHLEditor;
    StatusBar1: TStatusBar;
    TabSheet10: TTabSheet;
    RAHLEditor5: TRAHLEditor;
    TabSheet11: TTabSheet;
    RAHLEditor6: TRAHLEditor;
    Panel2: TPanel;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RAEditorPaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
    procedure RAEditorCompletionDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
    procedure RAEditor3ReservedWord(Sender: TObject; Token: String;
      var Reserved: Boolean);
  private
    Parser : TIParser;
  end;

var
  Editor: TEditor;

implementation

uses RAUtils, RACnst;

{$R *.DFM}

procedure TEditor.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  Parser := TIParser.Create;
end;

procedure TEditor.FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

procedure TEditor.RAEditorPaintGutter(Sender: TObject; Canvas: TCanvas);
  procedure Draw(Y, ImageIndex : integer);
  var
    Ro : integer;
    R : TRect;
  begin
    if Y <> -1 then
      with Sender as TRAEditor do begin
        Ro := Y - TopRow;
        R := CalcCellRect(0, Ro);
        GutterImages.Draw(Canvas,
          GutterWidth -GutterRightMargin -GutterImages.Width{R.Left},
          R.Top + (CellRect.Height - GutterImages.Height) div 2 +1,
          ImageIndex);
      end;
  end;
var
  i  : integer;
begin
  for i := 0 to 9 do
    if (Sender as TRAEditor).Bookmarks[i].Valid then
      Draw((Sender as TRAEditor).Bookmarks[i].Y, i);
end;

procedure TEditor.PageControl1Change(Sender: TObject);
begin
  Parser.Style := TIParserStyle(not(PageControl1.ActivePage.PageIndex=0));
  RAEditor.Refresh;
  RAEditor1.Refresh;
  RAEditor2.Refresh;
  RAEditor3.Refresh;
end;

procedure TEditor.PageControl1Enter(Sender: TObject);
begin
  (PageControl1.ActivePage.Controls[0] as TWinControl).SetFocus;
end;

procedure TEditor.RAEditorCompletionDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	Offset, W : Integer;
  S : string;
  ImageIndex : integer;
begin
  Offset := 3;
  with Control as TListBox, (Control.Owner as TRAEditor).Completion do
  begin
    Canvas.FillRect(Rect);
    case Mode of
      cmIdentifers :
        begin
          ImageIndex := StrToInt(Trim(SubStr(Items[Index], 2, Separator))) - 1;
          ilCompletions.Draw(Canvas, Rect.Left + 2, Rect.Top, ImageIndex);
          Canvas.TextOut(Rect.Left + 3*Offset + ilCompletions.Width, Rect.Top +2, SubStr(Items[Index], 0, Separator));
          S := Trim(SubStr(Items[Index], 1, Separator));
          W := Canvas.TextWidth(S);
          Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2, S);
        end;
      cmTemplates :
        begin
          Canvas.TextOut(Rect.Left + Offset, Rect.Top +2, SubStr(Items[Index], 1, Separator));
          Canvas.Font.Style := [fsBold];
          S := SubStr(Items[Index], 0, Separator);
          W := Canvas.TextWidth(S);
          Canvas.TextOut(Rect.Right - 2*Offset - W, Rect.Top +2, S);
        end;
    end;
  end;  
end;

procedure TEditor.RegAuto1AfterLoad(Sender: TObject);
begin
  PageControl1.ActivePage := PageControl1.Pages[RegAuto1.ReadInteger(Name, 'PageIndex', PageControl1.ActivePage.PageIndex)];
  PageControl1Change(nil);
end;


procedure TEditor.RegAuto1AfterSave(Sender: TObject);
begin
  RegAuto1.WriteInteger(Name, 'PageIndex', PageControl1.ActivePage.PageIndex);
end;

procedure TEditor.RAEditor3ReservedWord(Sender: TObject; Token: String;
  var Reserved: Boolean);
begin
  Reserved := (Token = 'R') or (Token = '&') or (Token = 'A') or
    Cmp(Token, 'Library');
end;

end.
