unit FileUtilsMessages;

interface

var
  FILE_CREATE: Integer;

implementation
uses MessageManager;

initialization
  // Indica a criação de um arquivo
  // Parâmetros: Nome do Arquivo (String)
  FILE_CREATE := GetMessageManager.RegisterMessageID('FILE_CREATE');
end.
