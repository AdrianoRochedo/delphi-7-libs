unit SpreadSheetBook_Form;

{ATENCAO:
  Este formulario nao devera ser criado manualmente.
  As instancias de TSpreadSheetBook fazem isto.}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpreadSheetBook, SpreadSheetBook_Frame, Menus;

type
  TSpreadSheetBookForm = class(TForm)
    Frame: TSpreadSheetBookFrame;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSpreadSheet: TSpreadSheetBook;
  public
    constructor Create(SpreadSheet : TSpreadSheetBook);
    property SpreadSheet : TSpreadSheetBook read FSpreadSheet write FSpreadSheet;
  end;

implementation

{$R *.dfm}

{ TSpreadSheetBookForm }

constructor TSpreadSheetBookForm.Create(SpreadSheet : TSpreadSheetBook);
begin
  inherited Create(nil);
  FSpreadSheet := SpreadSheet;
end;

procedure TSpreadSheetBookForm.FormDestroy(Sender: TObject);
begin
  if FSpreadSheet <> nil then
     begin
     FSpreadSheet.ParentForm := nil;
     FreeAndNil(FSpreadSheet);
     end;
end;

procedure TSpreadSheetBookForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Frame.Menu_Destruir.Checked then
     Action := caFree
  else
     Action := caHide;
end;

end.
