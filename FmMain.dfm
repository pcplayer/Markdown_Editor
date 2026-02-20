object Form1: TForm1
  Left = 271
  Top = 114
  Caption = 'Markdown '#32534#36753#22120
  ClientHeight = 477
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 49
    Width = 711
    Height = 428
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 336
    ExplicitTop = 256
    ExplicitWidth = 289
    ExplicitHeight = 193
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object RichEdit1: TRichEdit
        Left = 0
        Top = 0
        Width = 703
        Height = 400
        Align = alClient
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'RichEdit1')
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 144
        ExplicitTop = 104
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object EdgeBrowser1: TEdgeBrowser
        Left = 0
        Top = 0
        Width = 703
        Height = 400
        Align = alClient
        TabOrder = 0
        AllowSingleSignOnUsingOSPrimaryAccount = False
        TargetCompatibleBrowserVersion = '117.0.2045.28'
        UserDataFolder = '%LOCALAPPDATA%\bds.exe.WebView2'
        ExplicitLeft = 144
        ExplicitTop = 64
        ExplicitWidth = 100
        ExplicitHeight = 41
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 711
    Height = 49
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object ButtonOpenBrowser: TButton
      Left = 360
      Top = 8
      Width = 107
      Height = 25
      Caption = 'Open Browser'
      TabOrder = 0
      OnClick = ButtonOpenBrowserClick
    end
    object ButtonStart: TButton
      Left = 176
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = ButtonStartClick
    end
    object ButtonStop: TButton
      Left = 265
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      OnClick = ButtonStopClick
    end
    object EditPort: TEdit
      Left = 49
      Top = 10
      Width = 112
      Height = 21
      TabOrder = 3
      Text = '8080'
    end
    object Button1: TButton
      Left = 504
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 4
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 120
    Top = 384
  end
  object ActionList1: TActionList
    Left = 352
    Top = 248
    object AcOpen: TAction
      Caption = #25171#24320
      OnExecute = AcOpenExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 472
    Top = 216
  end
end
