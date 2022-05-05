{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}
// Portuguese(BR) translation : Nelson Luiz Dumbra (dumbranl@zaz.com.br)

{$INCLUDE RA.INC}

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Cancela';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor de Campos...';

  SDEAddItem          = '&Adiciona campos...';
  SDEDeleteItem       = '&Excluir';
  SDESelectAllItem    = '&Seleciona Todos';
  SDENewItem          = '&Novo Campo...';

  SDEAddFieldsCaption = 'Adiciona campos';
  SDEAvailableFields  = 'Campos Disponíveis';

  SDENewFieldCaption    = 'Novo campo';
  SDEFieldProperties    = 'Propriedades do Campo';
  SDEFieldNameLabel     = '&Nome:';
  SDEFieldTypeLabel     = '&Tipo:';
  SDEComponentNameLabel = '&Componente:';
  SDEFieldSizeLabel     = '&Tamanho:';
  SDEFieldKind          = 'Tipo de Campo';
  SDELookupGroup        = 'Definição de Pesquisa';
  SDEKeyFieldsLabel     = 'C&ampos Chaves:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'C&haves de pesquisa:';
  SDEResultFieldLabel   = 'Ca&mpo resultado:';
  SDEFieldKindItems     = '&Dados'#13'Cal&culados'#13'&Pesquisa';
  SDEFieldTypeMustBeSpecified = 'Tipo de campo deve ser especificado';

  SDBGridColEditor    = '&Editor de Colunas...';

 { Collection Editor }
  SCEEditCollection     = 'Editando %s';
  SCEAdd                = '&Adicionar';
  SCEDelete             = '&Excluir';
  SCEMoveUp             = 'Move A&cima';
  SCEMoveDown           = 'Move A&baixo';
  SCESelectAllItem      = '&Seleciona Todos';

 { Picture Editor }
  SPELoad               = '&Carrega...';
  SPESave               = '&Salva...';
  SPEClear              = '&Limpa';
  SPECopy               = 'C&opia';
  SPEPaste              = '&Colar';

 { Menu Designer }
  SMDMenuDesigner       = 'Menu &Desenhar';
  SMDInsertItem         = '&Inserir';
  SMDDeleteItem         = '&Excluir';
  SMDCreateSubmenuItem  = 'Criar &Submenu';

implementation

end.
