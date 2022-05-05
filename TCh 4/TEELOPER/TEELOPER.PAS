{**************************************}
{   TeeChart-Pro 3.0                   }
{   Developer Kit Samples Unit         }
{**************************************}
unit TeeLoper;

interface

Uses MyPoint,   { <-- Sample Series: TMyPointSeries derived from TPointSeries }
     Bar3D,     { <-- Sample Series: TBar3DSeries derived from TBarSeries }
     BigCandl,  { <-- Sample Series: TBigCandleSeries derived from TCandleSeries }
     TeeCount,  { <-- Sample Function: TCountFunction  }
     ImaPoint,  { <-- Sample Series: ImagePointSeries and DeltaPointSeries }
     ImageBar   { <-- Sample Series: ImageBarSeries }
     ;

Procedure Register;

implementation

Uses TeEngine,  { <-- needed for TChartPen type }
     ChartReg,  { <-- needed for TChartPenProperty editor type }
     DsgnIntf;

Procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TChartPen), TMyPointSeries, 'LinesPen',TChartPenProperty);
end;

end.
