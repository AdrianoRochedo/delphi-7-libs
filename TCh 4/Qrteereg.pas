{**********************************************}
{  TeeChart 4.0 --> QuickReports 2.0 or 3.0    }
{                                              }
{  Component Registration Unit.                }
{                                              }
{  Copyright (c) 1996-98 by David Berneda      }
{  All Rights Reserved                         }
{**********************************************}

{$I teedefs.inc}
unit QRTeeReg;

interface

procedure Register;

implementation

Uses Classes,DBChart,TeeConst,QRTee,SysUtils,DsgnIntf,TeeAbout,
     TeePrevi,TeEngine,
     {$IFDEF D1}
     IEdit16,
     {$ELSE}
     IEdiGene,
     {$ENDIF}
     EditChar
     {$IFNDEF NOUSE_BDE}
     {$IFNDEF D1}
     ,DBEditCh
     {$ENDIF}
     {$ENDIF}
     ;

{$IFDEF D1}
{$R TEEQR.R16}
{$ELSE}
{$R TEEQR.RES}
{$ENDIF}

type
  TQRChartCompEditor=class(TComponentEditor)
  public
    procedure ExecuteVerb( Index : Integer ); override;
    procedure Edit; override;
    function GetVerbCount : Integer; override;
    function GetVerb( Index : Integer ) : string; override;
  end;

  TQRChartProperty=class(TClassProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

{ QRChart Editor }
procedure TQRChartCompEditor.ExecuteVerb( Index : Integer );
Var AChart:TQRDBChart;
begin
  AChart:=TQRChart(Component).Chart;
  Case Index of
    3: TeeShowAboutBox({$IFDEF TEETRIAL}True{$ELSE}False{$ENDIF});
    4: Edit;
    5: ChartPreview(nil,AChart);
    6: ChartExport(nil,AChart);
  else
    inherited ExecuteVerb(Index);
  end;
{  ChartExecuteVerb( Index,TQRChart(Component).Chart ); }
end;

procedure TQRChartCompEditor.Edit;
Var AChart:TQRDBChart;
    Part:TChartClickedPart;
begin
  AChart:=TQRChart(Component).Chart;
  With AChart do CalcClickedPart(GetCursorPos,Part);
  EditChartPart(nil,AChart,Part);
{  EditChartDesign(TQRChart(Component).Chart); }
  Designer.Modified;
end;

function TQRChartCompEditor.GetVerbCount : Integer;
begin
  Result := 7;
end;

function TQRChartCompEditor.GetVerb( Index : Integer ) : string;
begin
  result:='';
  Case Index of
    0: result:=TeeMsg_Version;
    1: result:=TeeMsg_Copyright;
    2: result:='-';  { <--- do not change or translate... }
    3: result:=TeeMsg_About;
    4: result:=TeeMsg_EditChart;
    5: result:=TeeMsg_PrintPreview;
    6: result:=TeeMsg_ExportChart;
  end;
end;

{ QRChart property Editor }
procedure TQRChartProperty.Edit;
Var AChart:TDBChart;
    Part:TChartClickedPart;
begin
  AChart:=TDBChart(GetOrdValue);
  With AChart do CalcClickedPart(GetCursorPos,Part);
  EditChartPart(nil,AChart,Part);
{  EditChartDesign(TDBChart(GetOrdValue)); }
  Designer.Modified;
end;

function TQRChartProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TQRChartProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure Register;
begin
  RegisterNoIcon([TQRDBChart]);
  RegisterComponents( tcQReport,[TQRChart] );
  RegisterComponentEditor(TQRChart,TQRChartCompEditor);
  RegisterPropertyEditor( TypeInfo(TQRDBChart),TQRChart,'Chart',TQRChartProperty); { <-- do not translate }
  {$IFDEF D3}
  RegisterNonActiveX([TQRDBChart,TQRChart] , axrIncludeDescendants );
  {$ENDIF}
end;

end.
