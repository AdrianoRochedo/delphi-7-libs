unit WindowsManagerMessages;

interface
var
  WINDOWSCREATE     : Integer;
  WINMANAGER_ADD    : Integer;
  WINMANAGER_REMOVE : Integer;

implementation
uses MessageManager;

initialization
  // Notificação de criação de Janelas
  // Parâmetros: Form: TForm
  WINDOWSCREATE := GetMessageManager.RegisterMessageID('WINDOWSCREATE');

  // Adiciona uma janela ao gerenciador de janelas
  // Parâmetros: Form: TForm
  WINMANAGER_ADD := GetMessageManager.RegisterMessageID('WINMANAGER_ADD');

  // Remove uma janela do gerenciador de janelas
  // Parâmetros: Form: TForm
  WINMANAGER_REMOVE := GetMessageManager.RegisterMessageID('WINMANAGER_REMOVE');
end.
