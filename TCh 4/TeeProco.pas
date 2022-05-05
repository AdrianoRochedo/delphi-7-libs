{*****************************************}
{   TeeChart-Pro 4.0                      }
{   Copyright (c) 1995-98 David Berneda   }
{                                         }
{    Constants for Pro components         }
{*****************************************}
{$I teedefs.inc}
unit TeeProCo;

interface

{$IFDEF D3}
resourcestring
{$ELSE}
Const
{$ENDIF}
  TeeMsg_GalleryPolar       ='Polar';
  TeeMsg_GalleryCandle      ='Candle';
  TeeMsg_GalleryVolume      ='Volume';
  TeeMsg_GallerySurface     ='Surface';
  TeeMsg_GalleryContour     ='Contour';
  TeeMsg_GalleryBezier      ='Bezier';
  TeeMsg_GalleryPoint3D     ='Point 3D';
  TeeMsg_GalleryRadar       ='Radar';
  TeeMsg_PolyDegreeRange    ='Polynomial degree must be between 1 and 20';
  TeeMsg_AnswerVectorIndex  ='Answer Vector index must be between 1 and %d';
  TeeMsg_FittingError       ='Cannot process fitting';
  TeeMsg_PeriodRange        ='Period must be >= 0';
  TeeMsg_ExpAverageWeight   ='ExpAverage Weight must be between 0 and 1';
  TeeMsg_GalleryErrorBar    ='Error Bar';
  TeeMsg_GalleryError       ='Error';
  TeeMsg_FunctionMomentum    ='Momentum';
  TeeMsg_FunctionExpAverage  ='Exp. Average';
 TeeMsg_FunctionMovingAverage='Moving Average';
  TeeMsg_FunctionRSI         ='R.S.I.';
  TeeMsg_FunctionCurveFitting='Curve Fitting';
  TeeMsg_FunctionTrend       ='Trend';
  TeeMsg_FunctionCumulative  ='Cumulative';
  TeeMsg_FunctionStdDeviation='Std.Deviation';
  TeeMsg_FunctionCount       ='Count';
  TeeMsg_LoadChart           ='Open TeeChart...';
  TeeMsg_SaveChart           ='Save TeeChart...';
  TeeMsg_TeeFiles            ='TeeChart Pro files';

  TeeMsg_GallerySamples      ='Samples';

  TeeMsg_CannotFindEditor    ='Cannot find Series editor Form: %s';


  TeeMsg_CannotLoadChartFromURL='Error code: %d downloading Chart from URL: %s';

  TeeMsg_Test                ='Test...';

  TeeMsg_ValuesDate          ='Date';
  TeeMsg_ValuesOpen          ='Open';
  TeeMsg_ValuesHigh          ='High';
  TeeMsg_ValuesLow           ='Low';
  TeeMsg_ValuesClose         ='Close';
  TeeMsg_ValuesOffset        ='Offset';
  TeeMsg_ValuesStdError      ='StdError';

  TeeMsg_Grid3D              ='Grid 3D';

  TeeMsg_LowBezierPoints     ='Number of Bezier points should be > 1';

  { TeeCommander component... }

      TeeCommanMsg_Normal   = 'Normal';
      TeeCommanMsg_Edit     = 'Edit';
      TeeCommanMsg_Print    = 'Print';
      TeeCommanMsg_Copy     = 'Copy';

      TeeCommanMsg_Rotating = 'Rotation: %d° Elevation: %d°';
      TeeCommanMsg_Rotate   = 'Rotate';

      TeeCommanMsg_Moving   = 'Horiz.Offset: %d Vert.Offset: %d';
      TeeCommanMsg_Move     = 'Move';

      TeeCommanMsg_Zooming  = 'Zoom: %d %%';
      TeeCommanMsg_Zoom     = 'Zoom';

      TeeCommanMsg_Depthing = '3D: %d %%';
      TeeCommanMsg_Depth    = 'Depth';

      TeeCommanMsg_Chart    ='Chart';
      TeeCommanMsg_Panel    ='Panel';

      TeeCommanMsg_RotateLabel='Drag %s to Rotate';
      TeeCommanMsg_MoveLabel  ='Drag %s to Move';
      TeeCommanMsg_ZoomLabel  ='Drag %s to Zoom';
      TeeCommanMsg_DepthLabel ='Drag %s to resize 3D';

      TeeCommanMsg_NormalLabel='Drag Left button to Zoom, Right button to Scroll';
      TeeCommanMsg_NormalPieLabel='Drag a Pie Slice to Explode it';

      TeeCommanMsg_PieExploding = 'Slice: %d Exploded: %d %%';

implementation

end.
