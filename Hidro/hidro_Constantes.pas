unit hidro_Constantes;

interface
uses Messages;

Const
  Aspa = #39;

  AM_ABRIR_ARQUIVO               = WM_USER + 1001;
  AM_ABRIR_APAGAR_ARQUIVO        = WM_USER + 1002;

  cFiltroPascalScript            = 'Arquivos Script Pascal (*.pscript)|*.pscript' + '|' +
                                   'Arquivos Pascal (*.pas)|*.pas' + '|' +
                                   'Todos (*.*)|*.*';

  cFiltroTexto                   = 'Arquivos Texto (*.txt)|*.txt' + '|' +
                                   'Todos (*.*)|*.*';

  cMsgAjuda01   = 'Clique consecutivamente em dois Pontos de Controle ' +
                  'para criar um Trecho D`�gua';

  cMsgAjuda02   = 'Clique em um Ponto de Controle para criar uma Sub-Bacia';

  cMsgAjuda03   = 'Clique em uma janela de projeto para criar um Ponto de Controle';

  cMsgAjuda04   = 'Clique em outro Ponto de Controle para fechar a conex�o';

  cMsgAjuda05   = 'Selecione dois Pontos de Controle pertencentes a um Trecho D`�gua'#13 +
                  'Mas Aten��o: Primeiro o ponto Montante e depois o Jusante';

  cMsgAjuda06   = 'Agora selecione o Ponto de Controle Jusante a este Trecho D`�gua';

  cMsgAjuda07   = 'Clique em um Ponto de Controle ou em uma Sub-Bacia para criar uma Demanda'#13 +
                  'Aten��o: Se houver uma Classe de Demanda selecionada a demanda criada herdar� seus dados';

  cMsgAjuda08   = 'Clique em uma janela de projeto para criar um Reservat�rio';


  cMsgStatus01  = 'Trecho D`�gua criado entre os PCs "%s" e "%s"';

  cMsgStatus02  = 'Primeiro Ponto de Controle: %s';

  cMsgStatus03  = 'Segundo Ponto: ?? (indefinido)';

  cMsgStatus04  = 'Ponto Montante: %s';

  cMsgStatus05  = 'Ponto Jusante: ?? (indefinido)';

  cMsgStatus06   = 'Ponto de Controle inserido entre os PCs "%s" e "%s"';


  cMsgErro01     = 'O PC selecionado "%s" n�o � Jusante ao PC "%s"';

  cMsgErro02     = 'O PC selecionado "%s" n�o � um PC Montante.';

  cMsgErro03     = 'O nome "%s" j� pertence a outro objeto !'#13 +
                   'Por favor, escolha um nome que n�o exista.';

  cMsgErro04     = 'Objeto: %s'#13 +
                   'C�digo de Retorno de Contribui��o Inv�lido';

  cMsgErro05     = 'Este PC j� possui um outro PC a jusante!';

  cMsgErro06     = 'O PC "%s" � um dos PCs a Montante do PC "%s"';

  cMsgError07    = 'M�todo "%s" n�o Implementado na classe "%s"';


  cMsgInfo01     = 'Os dados da Demanda "%s" foram'#13 +
                   'sincronizados com os dados da Classe "%s"';

  cMsgInfo02     = 'Somente posso remover uma classe se n�o houverem descendentes !';

  cMsgInfo03     = 'Inserir uma demanda com base na Classe de Demanda: "%s"';

  cMsgInfo04     = ' %s. Intervalo: (%d a %d)';

implementation

end.
