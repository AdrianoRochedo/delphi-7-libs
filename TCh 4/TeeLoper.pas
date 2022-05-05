{**************************************}
{   TeeChart-Pro 4.0                   }
{   Developer Kit Samples Unit         }
{**************************************}
unit TeeLoper;

interface

Uses MyPoint,   { <-- Sample Series: TMyPointSeries derived from Point Series }
     Bar3D,     { <-- Sample Series: TBar3DSeries derived from Bar Series }
     BigCandl,  { <-- Sample Series: TBigCandleSeries derived from Candle Series }
     ImaPoint,  { <-- Sample Series: ImagePointSeries and DeltaPoint Series }
     ImageBar,  { <-- Sample Series: ImageBarSeries }
     TeeImaEd,  {                      "" Editor dialog }
     TeeRose    { <-- Sample Series: TWindRoseSeries derived from Polar Series }
                { and Sample Series: TClockSeries derived from Polar Series }
     ;

{$IFNDEF D3}
Procedure Register;
{$ENDIF}

implementation

{$IFNDEF D3}
Uses TeCanvas,  { <-- needed for TChartPen type }
     ChartReg,  { <-- needed for TChartPenProperty editor type }
     DsgnIntf;

Procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TChartPen), TMyPointSeries, 'LinesPen',TChartPenProperty);
end;
{$ENDIF}

end.
