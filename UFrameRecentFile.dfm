object FrameRecentFile: TFrameRecentFile
  Left = 0
  Top = 0
  Width = 335
  Height = 342
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 261
    Width = 335
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 305
    ExplicitWidth = 150
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 50
    Width = 335
    Height = 211
    Hint = #26368#36817#25171#24320#36807#30340#25991#20214
    Align = alClient
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    PopupMenu = PopupMenu1
    ReadOnly = True
    ShowHint = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    OnCellClick = DBGrid1CellClick
    OnDblClick = DBGrid1DblClick
    Columns = <
      item
        Expanded = False
        FieldName = 'FileName'
        Title.Caption = #25991#20214#21517
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FullName'
        Title.Caption = #20840#21517
        Width = 120
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastTime'
        Title.Caption = #26368#36817#35775#38382#26102#38388
        Visible = True
      end>
  end
  object DBMemo1: TDBMemo
    Left = 0
    Top = 264
    Width = 335
    Height = 78
    Hint = #25991#20214#30340#25688#35201#35828#26126
    Align = alBottom
    DataField = 'Summary'
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 335
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = #24494#36719#38597#40657
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 112
      Height = 50
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 15
        Width = 98
        Height = 20
        Caption = #26368#36817#25171#24320#30340#25991#20214
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = #24494#36719#38597#40657
        Font.Style = []
        ParentFont = False
      end
    end
    object Panel3: TPanel
      Left = 112
      Top = 0
      Width = 223
      Height = 50
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object DBText1: TDBText
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 44
        Align = alClient
        AutoSize = True
        DataField = 'FileName'
        DataSource = DataSource1
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = #24494#36719#38597#40657
        Font.Style = []
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 56
        ExplicitHeight = 20
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = DmJaMarkdown.FdqRecentFiles
    Left = 128
    Top = 176
  end
  object ActionList1: TActionList
    Left = 216
    Top = 112
    object AcOpen: TAction
      Caption = #25171#24320
      OnExecute = AcOpenExecute
    end
    object AcDelete: TAction
      Caption = #21024#38500
      OnExecute = AcDeleteExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 64
    Top = 80
    object N1: TMenuItem
      Action = AcOpen
    end
    object N2: TMenuItem
      Action = AcDelete
    end
  end
end
