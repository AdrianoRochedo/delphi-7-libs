unit FileUtilsMessages;

interface

var
  FILE_CREATE: Integer;

implementation
uses MessageManager;

initialization
  // Indica a cria��o de um arquivo
  // Par�metros: Nome do Arquivo (String)
  FILE_CREATE := GetMessageManager.RegisterMessageID('FILE_CREATE');
end.
