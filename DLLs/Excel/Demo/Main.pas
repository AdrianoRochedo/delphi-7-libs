unit Main;

interface

uses
  Classes, Forms, Controls, StdCtrls,
  Excel_dll,
  BaseSpreadSheetBook;

type
  TForm1 = class(TForm)
    btnLer: TButton;
    procedure btnLerClick(Sender: TObject);
  private
    P: TBaseSpreadSheetBook;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  cArquivo = 'F:\Documentos\Meus Arquivos\Excel\Precos - Supermercados.xls';

procedure TForm1.btnLerClick(Sender: TObject);
begin
  P := Excel_dll.CreateSpreadSheet();
  P.LoadFromFile(cArquivo);

  caption := P.ActiveSheet.GetText(1, 1);

  P.NewSheet('teste');
  P.ActiveSheet.Write(1, 1, 'teste');
  P.SaveToFile('F:\teste.xls');

  P.Free();
end;

end.
