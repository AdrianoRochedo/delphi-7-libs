unit wsVars;

interface

var
  // Mensagens de comunica��o entre objetos
  WSM_REMOVER_OBJETO : Integer;
  WSM_NOME_MUDOU     : Integer;

  // Vari�veis num�ricas
  wsvFuzzVal : Double = 1e-10;

implementation
uses MessageManager;

initialization
  WSM_REMOVER_OBJETO := GetMessageManager.RegisterMessageID('WSM_REMOVER_OBJETO');
  WSM_NOME_MUDOU     := GetMessageManager.RegisterMessageID('WSM_NOME_MUDOU');

end.
