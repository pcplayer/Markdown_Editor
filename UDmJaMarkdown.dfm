object DmJaMarkdown: TDmJaMarkdown
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 407
  Width = 598
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=D:\Pro\Markdown'#32534#36753#22120'\Data\JaMarkdownData.db'
      'DriverID=SQLite')
    LoginPrompt = False
    BeforeConnect = FDConnection1BeforeConnect
    Left = 112
    Top = 80
  end
  object FdqFolders: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from MyFolders')
    Left = 112
    Top = 176
    object FdqFoldersFolderName: TStringField
      FieldName = 'FolderName'
      Origin = 'FolderName'
    end
    object FdqFoldersFolderFullName: TStringField
      FieldName = 'FolderFullName'
      Origin = 'FolderFullName'
      Size = 200
    end
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 280
    Top = 184
  end
  object FdqRecentFiles: TFDQuery
    Indexes = <
      item
        Active = True
        Selected = True
        Name = 'LastTime'
        Fields = 'LastTime'
        DescFields = 'LastTime'
      end>
    IndexName = 'LastTime'
    Connection = FDConnection1
    SQL.Strings = (
      'select * from RecentFiles'
      'order by LastTime desc')
    Left = 104
    Top = 256
    object FdqRecentFilesFileName: TStringField
      FieldName = 'FileName'
      Origin = 'FileName'
      Size = 100
    end
    object FdqRecentFilesFullName: TStringField
      FieldName = 'FullName'
      Origin = 'FullName'
      Size = 200
    end
    object FdqRecentFilesSummary: TStringField
      FieldName = 'Summary'
      Origin = 'Summary'
      Size = 200
    end
    object FdqRecentFilesLastTime: TDateTimeField
      FieldName = 'LastTime'
      Origin = 'LastTime'
    end
  end
end
