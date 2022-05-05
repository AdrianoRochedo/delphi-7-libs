library Excel;

uses
  ShareMem,
  SysUtils,
  Classes,
  SpreadSheetBook,
  BaseSpreadSheetBook in '..\..\SpreadSheet\BaseSpreadSheetBook.pas',
  SpreadSheetBook_Frame in '..\..\SpreadSheet\SpreadSheetBook_Frame.pas' {SpreadSheetBookFrame: TFrame};

{$R *.res}

  function CreateSpreadSheet(): TBaseSpreadSheetBook;
  begin
    Result := TSpreadSheetBook.Create();
  end;

exports
  CreateSpreadSheet;

begin

end.
