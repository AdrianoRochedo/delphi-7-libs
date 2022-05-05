library NPTeeCha;

uses
  SysUtils,
  Classes,
  NPPlugin,
  NPForm,
  NPSubCls,
  NPTee in 'NPTee.pas' {TeeChartPluginForm};

exports
  NP_GetEntryPoints index 1,
  NP_Initialize index 2,
  NP_Shutdown index 3;

{$R TeePlug.res}
begin
end.
