�
 TFORM1 0.  TPF0TForm1Form1Left� TopuWidthNHeight�Caption teeMach, SL  --- Chart Templates
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style PixelsPerInch`OnCreate
FormCreate
TextHeight TLabelLabel1LefthTopWidth� HeightCaptionHistogram Chart Example
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TLabelLabel2LeftTop`Width&HeightCaption`In Dephi 3.0 you can create a Component Template of this Chart to reuse it in your new projects.
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TLabelLabel3LeftToppWidth�HeightCaptionPThe green Line Series is a Cumulative Sum function of the red Bar Series values.
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TChartChart1LeftTop(Width9Height1BottomWall.ColorclWhiteFoot.Font.ColorclRedFoot.Font.Height�Foot.Font.NameArialFoot.Font.StylefsItalic LeftWall.Color��� Title.Text.Strings	Histogram 	BackColor��� BottomAxis.Grid.VisibleFrame.ColorclBlueFrame.WidthLeftAxis.Axis.VisibleLeftAxis.Grid.VisibleLeftAxis.TickLengthLeftAxis.Title.CaptionSalesLeftAxis.Title.Font.ColorclBlackLeftAxis.Title.Font.Height�LeftAxis.Title.Font.NameArialLeftAxis.Title.Font.StylefsBold RightAxis.AutomaticRightAxis.AutomaticMinimumRightAxis.Axis.VisibleRightAxis.Grid.ColorclSilverRightAxis.Grid.StylepsSolidRightAxis.LabelsFont.ColorclBlackRightAxis.LabelsFont.Height�RightAxis.LabelsFont.NameArialRightAxis.LabelsFont.Style RightAxis.MinorTickLengthRightAxis.TickLength	RightAxis.Title.CaptionCummulative SalesRightAxis.Title.Font.ColorclBlackRightAxis.Title.Font.Height�RightAxis.Title.Font.NameArialRightAxis.Title.Font.StylefsBold View3DColorclSilverTabOrder  
TBarSeriesSeries1Marks.Arrow.ColorclBlueMarks.Arrow.WidthMarks.ArrowLengthMarks.BackColor��� Marks.Visible	TitleSalesSeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone  TLineSeriesSeries2Marks.ArrowLengthMarks.StylesmsValueMarks.Visible
DataSourceSeries1TitleCummulative SalesSeriesColorclGreenVertAxis
aRightAxisLinePen.StylepsDotPointer.HorizSizePointer.InflateMargins	Pointer.Style	psDiamondPointer.VertSizePointer.Visible	XValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameYYValues.Multiplier       ��?YValues.OrderloNone TCumulativeTeeFunction1    TButtonButton1LeftTopWidthKHeightCaption&Edit Chart...TabOrderOnClickButton1Click   