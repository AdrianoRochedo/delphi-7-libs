{****************************************}
{     TeeChart Pro Charting Library      }
{  For Delphi 1,2,3,4 & C++ Builder 1&3  }
{ Copyright (c) 1995-98 by David Berneda }
{         All Rights Reserved            }
{****************************************}
{$I teedefs.inc}
unit TeeConst;

interface

{$IFDEF D3}
resourcestring
{$ELSE}
Const
{$ENDIF}
  TeeMsg_LegendTopPos       ='Top Legend Position must be between 0 and 100 %';
  TeeMsg_LegendFirstValue   ='First Legend Value must be > 0';
  TeeMsg_LegendColorWidth   ='Legend Color Width must be between 0 and 100 %';
  TeeMsg_SeriesSetDataSource='No ParentChart to validate DataSource';
  TeeMsg_SeriesInvDataSource='No valid DataSource: %s';
  TeeMsg_FillSample         ='FillSampleValues NumValues must be > 0';
  TeeMsg_Angle              ='%s Angle must be between 0 and 359 degrees';
  TeeMsg_AxisLogDateTime    ='DateTime Axis cannot be Logarithmic';
  TeeMsg_AxisLogNotPositive ='Logarithmic Axis Min and Max values should be >= 0';
  TeeMsg_AxisLabelSep       ='Labels Separation % must be greater than 0';
  TeeMsg_AxisIncrementNeg   ='Axis increment must be >= 0';
  TeeMsg_AxisMinMax         ='Axis Minimum Value must be <= Maximum';
  TeeMsg_AxisMaxMin         ='Axis Maximum Value must be >= Minimum';
  TeeMsg_AxisLogBase        ='Axis Logarithmic Base should be >= 2';
  TeeMsg_MaxPointsPerPage   ='MaxPointsPerPage must be >= 0';
  TeeMsg_3dPercent          ='3D effect percent must be between %d and %d';
  TeeMsg_CircularSeries     ='Circular Series dependences are not allowed';
  TeeMsg_WarningHiColor     ='16k Color or greater required to get better look';
  TeeMsg_BarWidthPercent    ='Bar Width Percent must be between 1 and 100';
  TeeMsg_BarOffsetPercent   ='Bar Offset Percent must be between -100% and 100%';

  TeeMsg_DefaultPercentOf   ='%s of %s';
  TeeMsg_DefPercentFormat   ='##0.## %';
  TeeMsg_DefValueFormat     ='#,##0.###';
  {$IFDEF TEETRIAL}
  TeeMsg_Version            ='TeeChart Pro 4.01 Trial';
  {$ELSE}
  TeeMsg_Version            ='TeeChart Pro 4.01';
  {$ENDIF}
  TeeMsg_AxisTitle          ='Axis Title';
  TeeMsg_AxisLabels         ='Axis Labels';
  TeeMsg_RefreshInterval    ='Refresh Interval must be between 0 and 60';
  TeeMsg_SeriesParentNoSelf ='Series.ParentChart is not myself!';
  TeeMsg_GalleryLine        ='Line';
  TeeMsg_GalleryPoint       ='Point';
  TeeMsg_GalleryArea        ='Area';
  TeeMsg_GalleryBar         ='Bar';
  TeeMsg_GalleryHorizBar    ='Horiz. Bar';
  TeeMsg_GalleryPie         ='Pie';
  TeeMsg_GalleryFastLine    ='Fast Line';

  TeeMsg_Rotation           ='Rotation';

  TeeMsg_PieSample1         ='Cars';
  TeeMsg_PieSample2         ='Phones';
  TeeMsg_PieSample3         ='Tables';
  TeeMsg_PieSample4         ='Monitors';
  TeeMsg_PieSample5         ='Lamps';
  TeeMsg_PieSample6         ='Keyboards';
  TeeMsg_PieSample7         ='Bikes';
  TeeMsg_PieSample8         ='Chairs';

  TeeMsg_GalleryChartName   ='TeeGalleryChart';
  TeeMsg_GalleryLogoFont    ='Courier New';
  TeeMsg_Editing            ='Editing %s';

  TeeMsg_GalleryStandard    ='Standard';
  TeeMsg_GalleryFunctions   ='Functions';
  TeeMsg_GalleryExtended    ='Extended';

  TeeMsg_Copyright          ='© 1997-98 by David Berneda';
  TeeMsg_EditChart          ='E&dit Chart...';
  TeeMsg_PrintPreview       ='&Print Preview...';
  TeeMsg_ExportChart        ='E&xport Chart...';

  TeeMsg_InvalidEditorClass ='%s: Invalid Editor Class: %s';
  TeeMsg_MissingEditorClass ='%s: has no Editor Dialog';

  TeeMsg_GalleryArrow       ='Arrow';

  TeeMsg_ExpFinish          ='&Finish';
  TeeMsg_ExpNext            ='&Next >';

  TeeMsg_GalleryGantt       ='Gantt';

  TeeMsg_GanttSample1       ='Design';
  TeeMsg_GanttSample2       ='Prototyping';
  TeeMsg_GanttSample3       ='Development';
  TeeMsg_GanttSample4       ='Sales';
  TeeMsg_GanttSample5       ='Marketing';
  TeeMsg_GanttSample6       ='Testing';
  TeeMsg_GanttSample7       ='Manufac.';
  TeeMsg_GanttSample8       ='Debugging';
  TeeMsg_GanttSample9       ='New Version';
  TeeMsg_GanttSample10      ='Banking';

  TeeMsg_ChangeSeriesTitle  ='Change Series Title';
  TeeMsg_NewSeriesTitle     ='New Series Title:';
  TeeMsg_DateTime           ='DateTime';
  TeeMsg_TopAxis            ='Top Axis';
  TeeMsg_BottomAxis         ='Bottom Axis';
  TeeMsg_LeftAxis           ='Left Axis';
  TeeMsg_RightAxis          ='Right Axis';

  TeeMsg_SureToDeleteSeries ='Delete %s?';
  TeeMsg_DateTimeFormat     ='DateTime For&mat:';
  TeeMsg_Default            ='Default: ';
  TeeMsg_ValuesFormat       ='Values For&mat:';
  TeeMsg_Maximum            ='Maximum';
  TeeMsg_Minimum            ='Minimum';
  TeeMsg_DesiredIncrement   ='Desired %s Increment';

  TeeMsg_IncorrectMaxMinValue='Incorrect value. Reason: %s';
  TeeMsg_EnterDateTime      ='Enter [Number of Days] [hh:mm:ss]';

  TeeMsg_SureToApply        ='Apply Changes?';
  TeeMsg_SelectedSeries     ='(Selected Series)';
  TeeMsg_RefreshData        ='&Refresh Data';

  TeeMsg_DefaultFontSize    ='8';
  TeeMsg_DefaultGalleryFontSize='8';
  TeeMsg_FunctionAdd        ='Add';
  TeeMsg_FunctionSubtract   ='Subtract';
  TeeMsg_FunctionMultiply   ='Multiply';
  TeeMsg_FunctionDivide     ='Divide';
  TeeMsg_FunctionHigh       ='High';
  TeeMsg_FunctionLow        ='Low';
  TeeMsg_FunctionAverage    ='Average';

  TeeMsg_GalleryShape       ='Shape';
  TeeMsg_GalleryBubble      ='Bubble';
  TeeMsg_FunctionNone       ='Copy';
  TeeMsg_AxisDlgValue       ='Value:';

  TeeMsg_PrivateDeclarations='{ Private declarations }';
  TeeMsg_PublicDeclarations ='{ Public declarations }';
  TeeMsg_DefaultFontName    ='Arial';

  TeeMsg_CheckPointerSize   ='Pointer size must be greater than zero';
  TeeMsg_About              ='Abo&ut TeeChart...';

  tcAdditional              ='Additional';
  tcDControls               ='Data Controls';
  tcQReport                 ='QReport';

  TeeMsg_DataSet            ='Dataset';
  TeeMsg_AskDataSet         ='&Dataset:';

  TeeMsg_SingleRecord       ='Single Record';
  TeeMsg_AskDataSource      ='&DataSource:';

  TeeMsg_FunctionPeriod     ='Function Period should be >= 0';

  TeeMsg_TeeChartWizard     ='TeeChart Wizard';
  TeeMsg_TeeMachWizard      ='teeMach.TeeChartWizard';
  TeeMsg_TeeMachSL          ='teeMach, SL';
  TeeMsg_WizardTab          ='Business';

  TeeMsg_ClearImage         ='Clea&r';
  TeeMsg_BrowseImage        ='B&rowse...';

  TeeMsg_WizardSureToClose  ='Are you sure that you want to close the TeeChart Wizard?';
  TeeMsg_FieldNotFound      ='Field %s does not exist';
  TeeMsg_OrderClipboard     ='TeeChart Order Form copied to the ClipBoard';
  TeeMsg_CancelOrder        ='Cancel Order Form?';

  { New in TeeChart 4.0... }

  TeeMsg_DepthAxis          ='Depth Axis';
  TeeMsg_PieOther           ='Other';
  TeeMsg_HelpButton         ='&Help...';
  TeeMsg_ShapeGallery1      ='abc';
  TeeMsg_ShapeGallery2      ='123';
  TeeMsg_ValuesX            ='X';
  TeeMsg_ValuesY            ='Y';
  TeeMsg_ValuesPie          ='Pie';
  TeeMsg_ValuesBar          ='Bar';
  TeeMsg_ValuesAngle        ='Angle';
  TeeMsg_ValuesGanttStart   ='Start';
  TeeMsg_ValuesGanttEnd     ='End';
  TeeMsg_ValuesGanttNextTask='NextTask';
  TeeMsg_ValuesBubbleRadius ='Radius';
  TeeMsg_ValuesArrowEndX    ='EndX';
  TeeMsg_ValuesArrowEndY    ='EndY';
  TeeMsg_Legend             ='Legend';
  TeeMsg_Title              ='Title';
  TeeMsg_Foot               ='Footer';
  TeeMsg_Period		    ='Period';
  TeeMsg_PeriodRange        ='Period range';
  TeeMsg_CalcPeriod         ='Calculate %s every:';
  TeeMsg_AsJPEG             ='as &JPEG (JPG)';
  TeeMsg_TeeExtension       ='tee';
  TeeMsg_TeeChartPalette    ='TeeChart';
  TeeMsg_SmallDotsPen       ='Small Dots';

implementation
end.
