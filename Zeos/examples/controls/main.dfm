object MainForm: TMainForm
  Left = 47
  Top = 49
  BorderStyle = bsDialog
  Caption = 'Controls example'
  ClientHeight = 446
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 306
    Top = 3
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 46
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 357
      Top = 3
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 6
      Top = 3
      Width = 39
      Height = 13
      Caption = 'Protocol'
    end
    object Label3: TLabel
      Left = 447
      Top = 3
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label4: TLabel
      Left = 156
      Top = 3
      Width = 46
      Height = 13
      Caption = 'Database'
    end
    object SpeedButton1: TSpeedButton
      Left = 279
      Top = 18
      Width = 23
      Height = 22
      Caption = '...'
      Flat = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 540
      Top = 15
      Width = 82
      Height = 22
      Caption = 'Connect'
      Flat = True
      OnClick = SpeedButton2Click
    end
    object Label13: TLabel
      Left = 306
      Top = 3
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object ZProtocol: TComboBox
      Left = 6
      Top = 18
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'mysql-3.23'
      Items.Strings = (
        'mysql-3.20'
        'mysql-3.23'
        'mysql-4'
        'prostgresql'
        'prostgresql-7.2'
        'interbase-5'
        'interbase-6'
        'firebird-1.0'
        'firebird-1.5'
        'mssql'
        'sybase')
    end
    object ZPassword: TEdit
      Left = 447
      Top = 18
      Width = 85
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object ZDatabase: TEdit
      Left = 156
      Top = 18
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'zeoslib'
    end
    object ZUername: TEdit
      Left = 357
      Top = 18
      Width = 85
      Height = 21
      TabOrder = 2
      Text = 'root'
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 46
    Width = 688
    Height = 400
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Data Controls'
      object Label5: TLabel
        Left = 3
        Top = 168
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label6: TLabel
        Left = 57
        Top = 168
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label7: TLabel
        Left = 3
        Top = 210
        Width = 87
        Height = 13
        Caption = 'Begin of work time'
      end
      object Label8: TLabel
        Left = 129
        Top = 210
        Width = 90
        Height = 13
        Caption = 'Finish of work time '
      end
      object Label9: TLabel
        Left = 3
        Top = 249
        Width = 39
        Height = 13
        Caption = 'Resume'
      end
      object Label10: TLabel
        Left = 459
        Top = 138
        Width = 28
        Height = 13
        Caption = 'Photo'
      end
      object Label11: TLabel
        Left = 258
        Top = 165
        Width = 55
        Height = 13
        Caption = 'Department'
      end
      object DBGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 680
        Height = 133
        Align = alTop
        DataSource = DSPeople
        ReadOnly = True
        TabOrder = 10
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBNavigator1: TDBNavigator
        Left = 3
        Top = 138
        Width = 240
        Height = 25
        DataSource = DSPeople
        TabOrder = 7
      end
      object DBId: TDBEdit
        Left = 3
        Top = 183
        Width = 49
        Height = 21
        DataField = 'p_id'
        DataSource = DSPeople
        TabOrder = 0
      end
      object DBName: TDBEdit
        Left = 57
        Top = 183
        Width = 196
        Height = 21
        DataField = 'p_name'
        DataSource = DSPeople
        TabOrder = 1
      end
      object DBBeginWorkTime: TDBEdit
        Left = 3
        Top = 225
        Width = 121
        Height = 21
        DataField = 'p_begin_work'
        DataSource = DSPeople
        TabOrder = 3
      end
      object DBResume: TDBMemo
        Left = 3
        Top = 264
        Width = 451
        Height = 109
        DataField = 'p_resume'
        DataSource = DSPeople
        TabOrder = 5
      end
      object DBPicture: TDBImage
        Left = 459
        Top = 153
        Width = 220
        Height = 219
        DataField = 'p_picture'
        DataSource = DSPeople
        TabOrder = 6
      end
      object DBEndWorkTime: TDBEdit
        Left = 129
        Top = 225
        Width = 121
        Height = 21
        DataField = 'p_end_work'
        DataSource = DSPeople
        TabOrder = 4
      end
      object LoadImageBtn: TButton
        Left = 336
        Top = 138
        Width = 75
        Height = 25
        Caption = 'Load Image'
        TabOrder = 9
        OnClick = LoadImageBtnClick
      end
      object LoadResumeBtn: TButton
        Left = 249
        Top = 138
        Width = 82
        Height = 25
        Caption = 'Load Resume'
        TabOrder = 8
        OnClick = LoadResumeBtnClick
      end
      object DBDepartment: TDBLookupComboBox
        Left = 258
        Top = 183
        Width = 145
        Height = 21
        DataField = 'p_dep_id'
        DataSource = DSPeople
        DropDownRows = 5
        KeyField = 'dep_id'
        ListField = 'dep_name'
        ListSource = DSDepartment
        TabOrder = 2
      end
    end
  end
  object ZPort: TEdit
    Left = 306
    Top = 18
    Width = 46
    Height = 21
    TabOrder = 2
  end
  object ZConnection: TZConnection
    Protocol = 'mysql'
    Port = 0
    Database = 'zeoslib'
    User = 'root'
    AutoCommit = False
    ReadOnly = True
    TransactIsolationLevel = tiNone
    Connected = False
    Left = 165
    Top = 81
  end
  object ZPeople: TZQuery
    Connection = ZConnection
    RequestLive = True
    CachedUpdates = False
    SQL.Strings = (
      'select * from people')
    ParamCheck = True
    Params = <>
    ShowRecordTypes = [utModified, utInserted, utUnmodified]
    Left = 24
    Top = 123
    object ZPeoplep_id: TSmallintField
      DisplayLabel = 'ID'
      FieldName = 'p_id'
      Required = True
    end
    object ZPeopledeprtment: TStringField
      FieldKind = fkLookup
      FieldName = 'deprtment'
      LookupDataSet = ZDepartment
      LookupKeyFields = 'dep_id'
      LookupResultField = 'dep_name'
      KeyFields = 'p_dep_id'
      Size = 32
      Lookup = True
    end
    object ZPeoplep_name: TStringField
      DisplayLabel = 'Name'
      FieldName = 'p_name'
      Size = 40
    end
    object ZPeoplep_begin_work: TTimeField
      DisplayLabel = 'Begin work'
      FieldName = 'p_begin_work'
    end
    object ZPeoplep_end_work: TTimeField
      DisplayLabel = 'End work'
      FieldName = 'p_end_work'
    end
    object ZPeoplep_picture: TBlobField
      DisplayLabel = 'Picture'
      FieldName = 'p_picture'
    end
    object ZPeoplep_resume: TMemoField
      DisplayLabel = 'Resume'
      FieldName = 'p_resume'
      BlobType = ftMemo
    end
    object ZPeoplep_redundant: TSmallintField
      DisplayLabel = 'Redundant'
      FieldName = 'p_redundant'
    end
    object ZPeoplep_dep_id: TSmallintField
      FieldName = 'p_dep_id'
      Visible = False
    end
  end
  object DSPeople: TDataSource
    DataSet = ZPeople
    Left = 72
    Top = 123
  end
  object ZDepartment: TZReadOnlyQuery
    Connection = ZConnection
    SQL.Strings = (
      'select * from department')
    ParamCheck = True
    Params = <>
    Left = 300
    Top = 129
  end
  object DSDepartment: TDataSource
    DataSet = ZDepartment
    Left = 378
    Top = 126
  end
end
