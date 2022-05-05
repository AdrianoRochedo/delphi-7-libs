
{$INCLUDE ps.INC}

unit pseREG;

interface

  procedure Register;

implementation
uses Classes, DesignIntf, Controls,
     psEditorComp;

{$R pse.dcr}

procedure Register;
begin
  RegisterComponents('AD-2', [TScriptPascalEditor]);
end;

end.
