program SpreadSheetDemo;

uses
  Forms,
  SysUtils,
  SysUtilsEx,
  SpreadSheetBook in '..\SpreadSheetBook.pas',
  SpreadSheetBook_Frame in '..\SpreadSheetBook_Frame.pas' {SpreadSheetBookFrame: TFrame};

{$R *.res}

var p: TSpreadSheetBook;
    i, j: integer;
begin
  SysUtils.DecimalSeparator := '.';

  p := TSpreadSheetBook.Create();

  p.ActiveSheet.ColWidth[1] := 150;
  p.ActiveSheet.Write(1, 1, 2.345);
  p.ActiveSheet.Write(2, 1, 23456);
  p.ActiveSheet.Write(3, 1, 'teste teste teste teste teste');
  try
    p.ActiveSheet.Write(4, 1, 9.30000019073486);
    p.ActiveSheet.Write(5, 1, '9.30000019073486');
  except
    showMessage('Erro');
  end;

  for i := 5 to 10 do
    for j := 2 to 6 do
      p.ActiveSheet.Write(i, j, Format('%d, %d', [i, j]));

  p.ShowModal();
  p.Free();
end.
