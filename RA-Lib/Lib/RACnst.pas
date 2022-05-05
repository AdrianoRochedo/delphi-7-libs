{***********************************************************
                R&A Library
       Copyright (C) 1996-2001 Andrei Prygounkov

       description : Language specific constant for English

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}
// Portugues (BR) translation : Nelson Luiz Dumbra (dumbranl@zaz.com.br)

{$INCLUDE RA.INC}

unit RACnst;

interface

const

 {TRADBTreeView}
  SDeleteNode             = 'Exclui %s ?';
  SDeleteNode2            = 'Exclui %s (com todos filhos) ?';
  SMasterFieldEmpty       = 'Propriedade "MasterField" deve ser preenchida';
  SDetailFieldEmpty       = 'Propriedade "DetailField" deve ser preenchida';
  SItemFieldEmpty         = 'Propriedade "ItemField" deve ser preenchida';
  SMasterDetailFieldError = '"MasterField" e "DetailField" devem ser do mesmo tipo';
  SMasterFieldError       = '"MasterField" deve ser do tipo inteiro';
  SDetailFieldError       = '"DetailField" deve ser do tipo inteiro';
  SItemFieldError         = '"ItemField" deve ser do tipo string, data ou inteiro';
  SIconFieldError         = '"IconField" deve ser do tipo inteiro';
  SMoveToModeError        = 'Inválido move modo para RADBTreeNode';
  SDataSetNotActive       = 'DataSet não está ativo';

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Você pode digitar propriedade especifica aqui';
  sRegAutoEditorTreeHint       = 'Propriedades Disponíveis';
  sRegAutoEditorListHint       = 'Propriedades armazenadas';
  sRegAutoEditorBtnAddPropHint = 'Adicionar/Remover propriedades';
  sRegAutoEditorSort           = 'Ordenar';

 {RAEditor}
  RAEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

{IParser}
 {$IFDEF RA_D}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF RA_D}
 {$IFDEF RA_B}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF RA_B}

{$IFDEF RAINTER}
 {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z',  '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', '_'];
{$ENDIF RAINTER}

{$IFDEF RA_D2}
  SScrollBarRange = 'Valor Scrollbar fora do limite';
{$ENDIF}

 {RADlg}
  SCancel = 'Cancela';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Desenhar';
  SMDInsertItem         = '&Inserir';
  SMDDeleteItem         = '&Excluir';
  SMDCreateSubmenuItem  = 'Criar &SubMenu';

  SCantGetShortCut      = 'Nome de arquivo para ShortCut %s não disponível';


 { RALib 1.23 }
  SPropertyNotExists    = 'Propriedade "%s" não existe';
  SInvalidPropertyType  = 'Propriedade "%s" tem tipo inválido';

implementation

end.
