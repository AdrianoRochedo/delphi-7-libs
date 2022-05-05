object fr_HTML_IE: Tfr_HTML_IE
  Left = 0
  Top = 0
  Width = 531
  Height = 210
  TabOrder = 0
  object WB: TWebBrowser
    Left = 0
    Top = 0
    Width = 531
    Height = 188
    Align = alClient
    TabOrder = 0
    OnProgressChange = WBProgressChange
    OnDownloadBegin = WBDownloadBegin
    OnDownloadComplete = WBDownloadComplete
    ControlData = {
      4C000000E13600006E1300000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Info: TPanel
    Left = 0
    Top = 188
    Width = 531
    Height = 22
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      531
      22)
    object ProgressBar: TProgressBar
      Left = 386
      Top = 3
      Width = 142
      Height = 16
      Anchors = [akRight]
      TabOrder = 0
    end
  end
end
