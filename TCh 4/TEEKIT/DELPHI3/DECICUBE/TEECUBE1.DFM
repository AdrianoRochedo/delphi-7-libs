�
 TDCDEMOFORM 0n  TPF0TDCDemoForm
DCDemoFormLeftOTopWidth�Height"CaptionDecision Cube Example FormFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style Position	poDefaultWindowStatewsMaximizedOnShowFormShowPixelsPerInch`
TextHeight TPanelPanel1Left Top�Width~Height1AlignalBottomCaptionPanel1TabOrder  TDecisionPivotDecisionPivot1LeftTopWidth|Height/ButtonAutoSize	DecisionSourceDecisionSource1GroupLayoutxtHorizontalGroupsxtRows	xtColumnsxtSummaries ButtonSpacingButtonWidth@ButtonHeightGroupSpacing
BorderWidthBorderStylebsNoneAlignalClientEnabledTabOrder    TDecisionGridDecisionGrid1Left Top� Width~Height� DefaultColWidthZDefaultRowHeightCaptionColorclActiveCaptionCaptionFont.CharsetDEFAULT_CHARSETCaptionFont.ColorclCaptionTextCaptionFont.Height�CaptionFont.NameMS Sans SerifCaptionFont.Style 	DataColorclInfoBkDataSumColorclNoneDataFont.CharsetDEFAULT_CHARSETDataFont.ColorclWindowTextDataFont.Height�DataFont.NameMS Sans SerifDataFont.Style LabelFont.CharsetDEFAULT_CHARSETLabelFont.ColorclWindowTextLabelFont.Height�LabelFont.NameMS Sans SerifLabelFont.Style 
LabelColor	clBtnFaceLabelSumColorclTealDecisionSourceDecisionSource1
Dimensions	FieldNamePaymentColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameShipVIAColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameTermsColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameShipDateColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameCOUNT OF AmountPaidColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameSUM OF AmountPaidColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameCOUNT OF ItemsTotalColorclNone	AlignmenttaCenter	Subtotals	 	FieldNameAverage of ORDERS.AmountPaidColorclNone	AlignmenttaCenter	Subtotals	  Totals	ShowCubeEditor	AlignalBottomColor	clBtnFaceGridLineWidthGridLineColorclBlackTabOrder  TDecisionGraphDecisionGraph1Left Top)Width~Height� DecisionSourceDecisionSource1BottomWall.ColorclWhiteFoot.Font.CharsetDEFAULT_CHARSETFoot.Font.ColorclRedFoot.Font.Height�Foot.Font.NameArialFoot.Font.StylefsItalic LeftWall.Color��� Title.Text.StringsTDecisionGraph OnClickSeriesDecisionGraph1ClickSeriesBottomAxis.Title.CaptionPaymentLeftAxis.Title.CaptionSUM OF AmountPaidLegend.VisibleRightAxis.Title.CaptionSUM OF AmountPaidTopAxis.Title.CaptionPaymentAlignalClientTabOrder 
TBarSeriesSeries1ActiveMarks.ArrowLengthMarks.VisibleTitleTemplate: PaymentSeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone
IdentifierTemplate: PaymentStyle=  
TBarSeriesSeries2ActiveMarks.ArrowLengthMarks.VisibleTitleTemplate: ShipVIASeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone
IdentifierTemplate: ShipVIAStyle=  
TBarSeriesSeries3ActiveColorEachPoint	Marks.ArrowLengthMarks.StylesmsValueMarks.Visible	TitleTemplate: TermsSeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone
IdentifierTemplate: TermsStyle=  
TBarSeriesSeries4ActiveMarks.ArrowLengthMarks.VisibleTitleTemplate: ShipDateSeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone
IdentifierTemplate: ShipDateStyle=  
TBarSeriesSeries13ColorEachPoint	Cursor�Marks.ArrowLengthMarks.StylesmsValueMarks.Visible	Title1D Template: PaymentSeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone
Identifier1D Template: PaymentStyle8  
TBarSeriesSeries5ActiveColorEachPoint	Marks.ArrowLengthMarks.StylesmsValueMarks.Visible	Title1D Template: TermsSeriesColorclRedXValues.DateTimeXValues.NameXXValues.Multiplier       ��?XValues.OrderloAscendingYValues.DateTimeYValues.NameBarYValues.Multiplier       ��?YValues.OrderloNone
Identifier1D Template: TermsStyle8   TPanelPanel2Left Top Width~Height)AlignalTopTabOrder TBitBtn
ButtonBackLeft0TopWidthKHeightCaption< &BackEnabledTabOrder OnClickButtonBackClick   TDecisionCubeDecisionCube1DataSetDecisionQuery1DimensionMap
ActiveFlag
diAsNeeded	FieldTypeftString	FieldnamePaymentMethodBaseNameORDERS.PaymentMethodNamePaymentDerivedFrom�DimensionTypedimDimensionBinTypebinNone
ValueCountActive	 
ActiveFlag
diAsNeeded	FieldTypeftString	FieldnameShipVIABaseNameORDERS.ShipVIANameShipVIADerivedFrom�DimensionTypedimDimensionBinTypebinNone
ValueCountActive	 
ActiveFlag
diAsNeeded	FieldTypeftString	FieldnameTermsBaseNameORDERS.TermsNameTermsDerivedFrom�DimensionTypedimDimensionBinTypebinNone
ValueCountActive	 
ActiveFlag
diAsNeeded	FieldType
ftDateTime	FieldnameShipDateBaseNameORDERS.ShipDateNameShipDateDerivedFrom�DimensionTypedimDimensionBinTypebinYear
ValueCountActive	
StartValue      g�@ 
ActiveFlag
diAsNeeded	FieldType	ftInteger	FieldnameCOUNT OF AmountPaidBaseNameORDERS.AmountPaidNameCOUNT OF AmountPaidDerivedFrom�DimensionTypedimCountBinTypebinNone
ValueCount�Active	 
ActiveFlag
diAsNeeded	FieldType
ftCurrency	FieldnameSUM OF AmountPaidBaseNameORDERS.AmountPaidNameSUM OF AmountPaidDerivedFrom�DimensionTypedimSumBinTypebinNone
ValueCount�Active	 
ActiveFlag
diAsNeeded	FieldType	ftInteger	FieldnameCOUNT OF ItemsTotalBaseNameORDERS.ItemsTotalNameCOUNT OF ItemsTotalDerivedFrom�DimensionTypedimCountBinTypebinNone
ValueCount�Active	 
ActiveFlagdiActive	FieldType	ftUnknown	FieldnameAverage of ORDERS.AmountPaidBaseNameORDERS.AmountPaidNameAverage of ORDERS.AmountPaidDerivedFromDimensionType
dimAverageBinTypebinNone
ValueCount�Active	  ShowProgressDialog	MaxDimensionsMaxSummariesMaxCells@B Left3Top  TDecisionQueryDecisionQuery1Active	DatabaseNameDBDEMOSSQL.Strings�SELECT ORDERS.PaymentMethod, "ORDERS.DB".ShipVIA, "ORDERS.DB".Terms, "ORDERS.DB".ShipDate, COUNT( "ORDERS.DB".AmountPaid ), SUM( "ORDERS.DB".AmountPaid ), COUNT( "ORDERS.DB".ItemsTotal )FROM "ORDERS.DB" ORDERS[GROUP BY ORDERS.PaymentMethod, "ORDERS.DB".ShipVIA, "ORDERS.DB".Terms, "ORDERS.DB".ShipDate Left�Top  TDecisionSourceDecisionSource1DecisionCubeDecisionCube1ControlTypextCheck
SparseRows
SparseColsOnSummaryChangeDecisionSource1SummaryChangeLeft�DimensionCountSummaryCountCurrentSummary
SparseRows
SparseColsDimensionInfo   �  � ��    