unit DiretivasDeCompilacao;

  {
    ATENÇÃO: Esta unidade devera ser uma das primeiras unidades declaradas no projeto
  }

interface

const
  // 0   - Sem Restricoes
  // 1   - Restricoes leves          - Copy, Cut, Paste procedures
  // 2   - Restricoes intermediarias - SaveToFile (SpreadSheets, Graphical, intermedials results)
  // 3   - Restricoes pesadas        - SaveToFile (Projects, Final results)

  {$ifdef PROGRAMA_PROPAGAR}
  dir_NivelDeRestricao = 0;
  {$else}
  dir_NivelDeRestricao = 0;
  {$endif}

var
  mes_NivelDeRestricao : string;

implementation

var
  s: char;

initialization
  {$IF dir_NivelDeRestricao = 1}
    s := '1';
  {$ELSEIF dir_NivelDeRestricao = 2}
    s := '2';
  {$ELSEIF dir_NivelDeRestricao = 3}
    s := '3';
  {$IFEND}

  mes_NivelDeRestricao := 'Esta é uma versão com restrições de nível ' + s + '.'#13 +
                          'Característica desabilitada.';

end.
