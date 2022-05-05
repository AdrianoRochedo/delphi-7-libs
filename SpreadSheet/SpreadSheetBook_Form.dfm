object SpreadSheetBookForm: TSpreadSheetBookForm
  Left = 202
  Top = 122
  Width = 486
  Height = 263
  Caption = ' Planilha'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inline Frame: TSpreadSheetBookFrame
    Left = 0
    Top = 0
    Width = 478
    Height = 236
    Align = alClient
    TabOrder = 0
    inherited SS: TcxSpreadSheetBook
      Width = 478
    end
  end
end
