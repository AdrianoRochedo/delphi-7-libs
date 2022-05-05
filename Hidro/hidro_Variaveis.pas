unit hidro_Variaveis;

interface
uses Classes,
     MessageManager,
     Messages,
     comCTRLs,
     stdCTRLs,
     extCTRLs,
     ErrosDLG,
     graphics,
     controls,
     hidro_Form_LayersManager;

// Mensagens de comunicação entre os objetos
   // UM_  -->   User Message
   // AM_  -->   Application Message
var
  UM_SET_VISIBILITY             : Integer;
  UM_RESET_VISIT                : Integer;
  UM_OBJETO_SE_DESTRUINDO       : Integer;
  UM_OBTER_OBJETO               : Integer;
  UM_REPINTAR_OBJETO            : Integer;
  UM_OBTER_DEMANDA_PELO_PADRAO  : Integer;
  UM_DEMANDA_PADRAO_MUDOU       : Integer;
  UM_NOME_OBJETO_MUDOU          : Integer;
  UM_INICIAR_SIMULACAO          : Integer;
  UM_COMENTARIO_OBJETO_MUDOU    : Integer;
  UM_TROCAR_REFERENCIA          : Integer;
  UM_DESCRICAO_OBJETO_MUDOU     : Integer;
  UM_HABILITAR_DEMANDAS         : Integer;
  UM_DESABILITAR_DEMANDAS       : Integer;
  UM_DESBLOQUEAR_DEMANDAS       : Integer;
  UM_BLOQUEAR_OBJETOS           : Integer;
  UM_PREPARAR_SCRIPTS           : Integer;
  UM_LIBERAR_SCRIPTS            : Integer;
  UM_OBTER_OBJETO_PELA_OPER     : Integer;

  // Layer Manager
  UM_LM_UPDATE_LAYER            : Integer;


  // Variáveis globais
  Var gVersao           : String;                   // Versão do Programa
      gSB               : TStatusBar;               // Barra de Status global
      gLeds             : TImage;                   // Leds de Status de cada janela
      gErros            : TErros_DLG;               // Janela de mensagens
      gExePath          : String;                   // Diretório onde estão os executáveis
      gDir              : String;                   // Diretorio de trabalho atual
      gLM               : THidroForm_LayersManager; // Gerenciador de Camadas

implementation

initialization
  UM_SET_VISIBILITY              := GetMessageManager.RegisterMessageID('UM_SET_VISIBILITY');
  UM_RESET_VISIT                 := GetMessageManager.RegisterMessageID('UM_RESET_VISIT');
  UM_OBJETO_SE_DESTRUINDO        := GetMessageManager.RegisterMessageID('UM_OBJETO_SE_DESTRUINDO');
  UM_OBTER_OBJETO                := GetMessageManager.RegisterMessageID('UM_OBTER_OBJETO');
  UM_REPINTAR_OBJETO             := GetMessageManager.RegisterMessageID('UM_REPINTAR_OBJETO');
  UM_OBTER_DEMANDA_PELO_PADRAO   := GetMessageManager.RegisterMessageID('UM_OBTER_DEMANDA_PELO_PADRAO');
  UM_DEMANDA_PADRAO_MUDOU        := GetMessageManager.RegisterMessageID('UM_DEMANDA_PADRAO_MUDOU');
  UM_NOME_OBJETO_MUDOU           := GetMessageManager.RegisterMessageID('UM_NOME_OBJETO_MUDOU');
  UM_INICIAR_SIMULACAO           := GetMessageManager.RegisterMessageID('UM_INICIAR_SIMULACAO');
  UM_COMENTARIO_OBJETO_MUDOU     := GetMessageManager.RegisterMessageID('UM_COMENTARIO_OBJETO_MUDOU');
  UM_TROCAR_REFERENCIA           := GetMessageManager.RegisterMessageID('UM_TROCAR_REFERENCIA');
  UM_DESCRICAO_OBJETO_MUDOU      := GetMessageManager.RegisterMessageID('UM_DESCRICAO_OBJETO_MUDOU');
  UM_HABILITAR_DEMANDAS          := GetMessageManager.RegisterMessageID('UM_HABILITAR_DEMANDAS');
  UM_DESABILITAR_DEMANDAS        := GetMessageManager.RegisterMessageID('UM_DESABILITAR_DEMANDAS');
  UM_DESBLOQUEAR_DEMANDAS        := GetMessageManager.RegisterMessageID('UM_DESBLOQUEAR_DEMANDAS');
  UM_BLOQUEAR_OBJETOS            := GetMessageManager.RegisterMessageID('UM_BLOQUEAR_OBJETOS');
  UM_PREPARAR_SCRIPTS            := GetMessageManager.RegisterMessageID('UM_PREPARAR_SCRIPTS');
  UM_LIBERAR_SCRIPTS             := GetMessageManager.RegisterMessageID('UM_LIBERAR_SCRIPTS');
  UM_OBTER_OBJETO_PELA_OPER      := GetMessageManager.RegisterMessageID('UM_OBTER_OBJETO_PELA_OPER');
  UM_LM_UPDATE_LAYER             := GetMessageManager.RegisterMessageID('UM_LM_UPDATE_LAYER');

  gLM := THidroForm_LayersManager.Create;
  gLM.Show;

finalization
  gLM.Release;
    
end.
