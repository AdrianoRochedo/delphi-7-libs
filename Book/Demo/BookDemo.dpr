program BookDemo;

uses
  Forms, Graphics,
  Book_Interfaces in '..\Book_Interfaces.pas',
  Book_Classes in '..\Book_Classes.pas',
  foBook in '..\foBook.pas' {Book},
  Frame_Memo in '..\..\Frames\Frame_Memo.pas' {fr_Memo: TFrame},
  Frame_RTF in '..\..\Frames\Frame_RTF.pas' {fr_RTF: TFrame},
  SpreadSheet_Frame in '..\..\SpreadSheet\SpreadSheet_Frame.pas' {SpreadSheetFrame: TFrame},
  SpreadSheetBook_Frame in '..\..\SpreadSheet\SpreadSheetBook_Frame.pas' {SpreadSheetBookFrame: TFrame};

{$R *.res}

var b: TBook;
begin
  b := TBook.Create(' Páginas Registradas');

  b.NewPage('memo'       , 'Memo'               , 'f:\teste.txt');
  b.NewPage('sheet'      , 'Folha de Planilha'                  );
  b.NewPage('sheet book' , 'Planilha'                           );
  b.NewPage('rtf'        , 'RTF 1'              , 'f:\teste.txt');
  b.NewPage('rtf'        , 'RTF 2'              , 'f:\teste.txt');

  b.ActivePageIndex := 0;
  b.TextPage.Write('teste na memo');

  b.ActivePageIndex := 4;
  b.TextPage.Write('teste no RTF ReadOnly');
  b.TextPage.setFontColor(clRED);
  b.TextPage.Write('Isto está em vermelho');
  b.TextPage.setFontColor(clLIME);
  b.TextPage.setFontSize(12);
  b.TextPage.Write('Isto está em verde 12');
  b.TextPage.setReadOnly(true);

  b.ShowModal();
  b.Free();
end.
