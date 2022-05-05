{*****************************************}
{   TeeChart Pro 4.0                      }
{   Copyright (c) 1996-98 David Berneda   }
{                                         }
{       Component Registration Unit       }
{                                         }
{  Funcs:  TCountTeeFunction              }
{          TCurveFittingTeeFunction       }
{          TAverageTeeFunction            }
{          TMovingAverageTeeFunction      }
{          TExpAverageTeeFunction         }
{          TMomentumTeeFunction           }
{          TRSITeeFunction                }
{          TStdDeviationTeeFunction       }
{                                         }
{ Series:  TCandleSeries                  }
{          TVolumeSeries                  }
{          TSurfaceSeries                 }
{          TContourSeries                 }
{          TErrorBarSeries                }
{          TPolarSeries                   }
{          TBezierSeries                  }
{          TPoint3DSeries                 }
{                                         }
{ Other:   TDraw3D                        }
{          TTeeCommander                  }
{          TChartEditor                   }
{          TChartPreviewer                }
{          TChartScrollBar                }
{          TChartListBox                  }
{          TSeriesDataSet                 }
{                                         }
{*****************************************}

{$I teedefs.inc}
unit ChartPro;

interface

Uses Chart;

procedure Register;

implementation

Uses Classes,Graphics,CandleCh,CurvFitt,ErrorBar,ErrBarEd,TeeSurfa,
     SurfEdit,TeePolar,PolarEdi,CandlEdi,StatChar,DesignIntf,DesignEditors,
     TeEngine,TeeProcs,ChartReg,EditPro,TeeMonth,Dialogs,TeeConst,
     TeeProCo,Controls,
     TeeBezie,TeePoin3,TeCanvas,TeeScroB,TeeEdit,TeeComma,TeeVolEd,TeeLisB,
     TeeCount,TeeCumu
     {$IFDEF D3}
     ,TeeData
     {$ENDIF}
     ;

type TChartEditorCompEditor=class(TComponentEditor)
     public
       procedure ExecuteVerb( Index : Integer ); override;
       function GetVerbCount : Integer; override;
       function GetVerb( Index : Integer ) : string; override;
     end;

{ TChartEditorCompEditor }
procedure TChartEditorCompEditor.ExecuteVerb( Index : Integer );
begin
  if Index=0 then (Component as TCustomChartEditor).Execute
             else inherited ExecuteVerb(Index);
end;

function TChartEditorCompEditor.GetVerbCount : Integer;
begin
  Result := inherited GetVerbCount+1;
end;

function TChartEditorCompEditor.GetVerb( Index : Integer ) : string;
begin
  if Index=0 then result:=TeeMsg_Test
             else result:=inherited GetVerb(Index);
end;

procedure Register;
begin
  {$IFNDEF D3}
  RegisterPropertyEditor(TypeInfo(TChartBrush),TSurfaceSeries, 'Brush',TChartBrushProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomErrorSeries, 'ErrorPen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TSurfaceSeries, 'Pen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomPolarSeries, 'Pen',TChartPenProperty);
  RegisterPropertyEditor(TypeInfo(TChartPen), TCustomPolarSeries, 'CirclePen',TChartPenProperty);
  {$ENDIF}

  RegisterComponents(TeeMsg_TeeChartPalette, [ TChartEditor,
                                               TChartPreviewer,
                                               TDraw3D,
                                               TChartScrollBar,
                                               TTeeCommander,
                                               TChartListBox
                                               {$IFDEF D3}
                                               ,TSeriesDataSet
                                               {$ENDIF}
                                              ]);

  RegisterComponentEditor(  TChartEditor,TChartEditorCompEditor);
  RegisterComponentEditor(  TChartPreviewer,TChartEditorCompEditor);
end;

end.
