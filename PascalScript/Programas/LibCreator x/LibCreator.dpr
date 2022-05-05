program LibCreator;

uses
  Forms,
  gb_Classes in 'gb_Classes.pas',
  Principal_2 in 'Principal_2.pas' {DLG_Principal},
  Frame_Parametros in 'Frame_Parametros.pas' {frm_Parametros: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  DLG_Principal := TDLG_Principal.Create(nil);
  Application.Run;
end.
