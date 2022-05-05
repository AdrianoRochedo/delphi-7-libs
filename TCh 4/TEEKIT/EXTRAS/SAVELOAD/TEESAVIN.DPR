program teesavin;

uses
  Forms,
  uteesave in 'UTEESAVE.PAS' {FormVirtual};

{$R *.RES}

begin
  Application.CreateForm(TFormVirtual, FormVirtual);
  Application.Run;
end.
