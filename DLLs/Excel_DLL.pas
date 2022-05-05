unit Excel_DLL;

interface
uses BaseSpreadSheetBook;

  function CreateSpreadSheet(): TBaseSpreadSheetBook;

implementation

  function CreateSpreadSheet(): TBaseSpreadSheetBook; external 'Excel.dll';

end.
