?
 TSTDLG 0)  TPF0TSTDlgSTDlgLeft? Top? Width?Height'ActiveControl	CreateBtnCaptionStTree Example
Font.ColorclBlackFont.Height?	Font.NameMS Sans Serif
Font.Style PixelsPerInch`PositionpoScreenCenterShowHint	
OnActivateFormActivateOnClose	FormCloseOnCreate
FormCreate
TextHeight TLabelLabel1LeftTop? WidthHeightCaptionFirst  TLabelLabel2LeftTop? WidthHeightCaptionLast  TLabelLabel3LeftTop? WidthHeightCaptionAge  TButton	CreateBtnLeftTopWidthKHeight!HintCreate a TreeCaptionCreateTabOrder OnClickCreateBtnClick  TButtonClearBtnLeftjTopWidthKHeight!Hint
Clear TreeCaptionClearTabOrderOnClickClearBtnClick  TListBoxLB1Left? TopWidth? Height? HintDblClk to delete selected itemTabStop
ItemHeightTabOrder	
OnDblClickLB1DblClick  TEditEdit1Left:Top? WidthYHeightHint1 to 10 characters	MaxLength
TabOrder  TEditEdit2Left:Top? WidthYHeightHint1 to 15 characters	MaxLengthTabOrder  TEditEdit3Left:Top? WidthHeightHint
1 to 32627	MaxLengthTabOrder  TButton	InsertBtnLeftTop4WidthKHeight!HintInsert new recordCaptionInsertTabOrderOnClickInsertBtnClick  TButton	DeleteBtnLeftjTop4WidthKHeight!HintDelete a recordCaptionDeleteTabOrderOnClickDeleteBtnClick  TButtonFindBtnLeftTop^WidthKHeight!HintFind a recordCaptionFindTabOrderOnClickFindBtnClick  TButton	SearchBtnLeftjTop^WidthKHeight!HintSearch by last nameCaptionSearchTabOrderOnClickSearchBtnClick  TButtonLoadBtnLeftTop? WidthKHeight!HintLoad from diskCaptionLoadTabOrder
OnClickLoadBtnClick  TButtonSaveBtnLeftjTop? WidthKHeight!HintSave to diskCaptionSaveTabOrderOnClickSaveBtnClick  TOpenDialogOD1
DefaultExtTDFFilter,*.tdf (Tree files)|*.tdf|*.* (All files)|*.*Left? Top?   TSaveDialogSD1
DefaultExtTDFFilter,*.tdf (Tree files)|*.tdf|*.* (All files)|*.*OptionsofOverwritePrompt Left? Top?    