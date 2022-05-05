unit fLineNumbersMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, RAEditor, RAHLEditor, StdCtrls;

type
  TLineNumbersMain = class(TForm)
    RAHLEditor1: TRAHLEditor;
    Panel1: TPanel;
    GutterFont: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RAHLEditor1PaintGutter(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LineNumbersMain: TLineNumbersMain;

implementation

uses RAUtils;

{$R *.DFM}

procedure TLineNumbersMain.FormCreate(Sender: TObject);
begin
  if FileExists(ExePath + '..\..\!README.TXT') then
    RAHLEditor1.Lines.LoadFromFile(ExePath + '..\..\!README.TXT')
  else
    RAHLEditor1.Lines.Add('          File "' + ExpandFileName(ExePath + '..\..\!README.TXT') + '" not found !');
end;

procedure TLineNumbersMain.RAHLEditor1PaintGutter(Sender: TObject;
  Canvas: TCanvas);
var
  i: Integer;
  Rect: TRect;
  oldFont: TFont;  
begin
  oldFont := TFont.Create;
  try                                            
    oldFont.Assign(Canvas.Font);
    Canvas.Font := GutterFont.Font;
    with RAHLEditor1 do
      for i := TopRow to TopRow + VisibleRowCount do
      begin
        Rect := Bounds(2, (i - TopRow) * CellRect.Height, GutterWidth - 2 - 5, CellRect.Height);
        DrawText(Canvas.Handle, PChar(IntToStr(i + 1)), -1, Rect, DT_RIGHT or DT_VCENTER or DT_SINGLELINE);
      end;
  finally
    Canvas.Font := oldFont;
    oldFont.Free;
  end;
end;

end.
