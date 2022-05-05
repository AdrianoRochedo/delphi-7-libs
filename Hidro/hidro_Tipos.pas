unit hidro_Tipos;

interface
uses IniFiles,
     wsVec,
     Arrays;

type
  TV   = TwsSFVec;
  TIV  = TwsSIVec;
  TIF  = TMemIniFile;

  TenTipoDeGrafico   = (tgBarras, tgLinhas);
  TenTipoSimulacao   = (tsDOS, tsWIN);

  TSetByte  = Set of Byte;

implementation

end.
