unit SpreadSheetBook;

interface
uses Forms,
     SysUtilsEx,
     BaseSpreadSheetBook,
     SheetWrapper;

type
  TSpreadSheetBook = class(TBaseSpreadSheetBook)
  private
    FForm: TForm;
    function getActiveSheet: TBaseSheet;  override;
    function getActiveSheetIndex: integer; override;
    function getSheetCount: integer; override;
    procedure setActiveSheetIndex(const Value: integer); override;
    function getCaption: String; override;
    procedure SetCaption(const Value: String); override;
  public
    constructor Create(); overload;
    constructor Create(const WinCaption, SheetCaption: string); overload;
    destructor Destroy(); override;

    // Geral
    procedure Show(FormStyle: TFormStyle = fsNormal); override;
    procedure ShowModal(); override;
    procedure BeginUpdate(); override;
    procedure EndUpdate(); override;

    // leitura e salvamento
    procedure LoadFromFile(const FileName: String); override;
    procedure SaveToFile  (const FileName: String); override;

    // Folhas
    procedure NewSheet(const aPageName: string = ''); override;

    // Titulo da janela
    property Caption: String read getCaption write setCaption;

    // Folhas
    property ActiveSheet      : TBaseSheet read getActiveSheet;
    property ActiveSheetIndex : integer read getActiveSheetIndex write setActiveSheetIndex;
    property SheetCount       : integer read getSheetCount;

    property ParentForm : TForm read FForm write FForm;
  end;

implementation
uses SpreadSheetBook_Form,
     MessageManager;

{ TSpreadSheetBook }

constructor TSpreadSheetBook.Create();
begin
  FForm := TSpreadSheetBookForm.Create(self);
end;

procedure TSpreadSheetBook.BeginUpdate();
begin
  TSpreadSheetBookForm(FForm).Frame.SS.BeginUpdate();
end;

constructor TSpreadSheetBook.Create(const WinCaption, SheetCaption: string);
begin
  FForm := TSpreadSheetBookForm.Create(self);
  FForm.Caption := WinCaption;
  if SheetCaption <> '' then
     TSpreadSheetBookForm(FForm).Frame.SS.ActiveSheet.Caption := SheetCaption;
end;

destructor TSpreadSheetBook.Destroy();
var MesID: integer;
begin
  // Caso o processo esteja utilizando o mecanismo de armazenamento de
  // objetos dinamicos.
  MesID := getMessageManager.GetMessageID('OL_RemoveObject');
  if MesID <> -1 then
     getMessageManager.SendMessage(MesID, [self]);

  if FForm <> nil then
     begin
     TSpreadSheetBookForm(FForm).SpreadSheet := nil;
     FForm.Free();
     end;

  inherited;
end;

procedure TSpreadSheetBook.EndUpdate;
begin
  TSpreadSheetBookForm(FForm).Frame.SS.EndUpdate();
end;

function TSpreadSheetBook.getActiveSheet: TBaseSheet;
begin
  result := TBaseSheet(TSpreadSheetBookForm(FForm).Frame.ActiveSheet);
end;

function TSpreadSheetBook.getActiveSheetIndex: integer;
begin
  result := TSpreadSheetBookForm(FForm).Frame.ActiveSheetIndex;
end;

function TSpreadSheetBook.getCaption: String;
begin
  result := FForm.Caption;
end;

function TSpreadSheetBook.getSheetCount: integer;
begin
  result := TSpreadSheetBookForm(FForm).Frame.SheetCount;
end;

procedure TSpreadSheetBook.LoadFromFile(const FileName: String);
begin
  TSpreadSheetBookForm(FForm).Frame.LoadFromFile(FileName);
end;

procedure TSpreadSheetBook.NewSheet(const aPageName: string);
begin
  TSpreadSheetBookForm(FForm).Frame.NewSheet(aPageName);
end;

procedure TSpreadSheetBook.SaveToFile(const FileName: String);
begin
  TSpreadSheetBookForm(FForm).Frame.SaveToFile(FileName);
end;

procedure TSpreadSheetBook.setActiveSheetIndex(const Value: integer);
begin
  TSpreadSheetBookForm(FForm).Frame.ActiveSheetIndex := Value;
end;

procedure TSpreadSheetBook.SetCaption(const Value: String);
begin
  FForm.Caption := Value;
end;

procedure TSpreadSheetBook.Show(FormStyle: TFormStyle = fsNormal);
begin
  setActiveSheetIndex(0);
  if FormStyle = Forms.fsMDIChild then
     begin
     FForm.FormStyle := FormStyle;
     FForm.SetBounds(100, 100, 400, 300);
     end
  else
     FForm.Show;
end;

procedure TSpreadSheetBook.ShowModal;
begin
  FForm.ShowModal();
end;

end.
