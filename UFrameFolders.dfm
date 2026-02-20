object FrameFolders: TFrameFolders
  Left = 0
  Top = 0
  Width = 267
  Height = 361
  TabOrder = 0
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 267
    Height = 361
    Hint = #24120#29992#25991#20214#22841
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
    ShowHint = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    OnDblClick = DBGrid1DblClick
    Columns = <
      item
        Expanded = False
        FieldName = 'FolderName'
        Title.Caption = #30446#24405#21517#31216
        Width = 149
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FolderFullName'
        Title.Caption = #30446#24405#20840#21517
        Visible = True
      end>
  end
  object DataSource1: TDataSource
    DataSet = DmJaMarkdown.FdqFolders
    Left = 120
    Top = 168
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Markdown'
        FileMask = '*.md'
      end
      item
        DisplayName = 'Txt'
        FileMask = '*.txt'
      end>
    Options = [fdoPickFolders]
    Title = #36873#25321#25991#20214#22841
    Left = 104
    Top = 72
  end
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 264
    object N1: TMenuItem
      Action = AcSelectFolder
    end
    object N2: TMenuItem
      Action = AcOpenFile
    end
    object N3: TMenuItem
      Action = AcDelete
    end
  end
  object ActionList1: TActionList
    Left = 192
    Top = 136
    object AcSelectFolder: TAction
      Caption = #36873#25321#25991#20214#22841
      OnExecute = AcSelectFolderExecute
    end
    object AcDelete: TAction
      Caption = #21024#38500
      OnExecute = AcDeleteExecute
    end
    object AcOpenFile: TAction
      Caption = #25171#24320#25991#20214
      OnExecute = AcOpenFileExecute
    end
  end
end
