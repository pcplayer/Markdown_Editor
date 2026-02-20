unit UFmMain;
{---------------------------------------------------------------------------
  markdown 文件编辑器。预览使用浏览器。内置 WebServer。
  默认文件里面的图片和文件在相同目录。因此，加载文件时，要根据文件目录，指定 WEB 的 Root，
  方便浏览器加载图片。

  但是，CSS/JS 这些，应该是在固定位置。因此，需要判断浏览器需要的 CSS/JS 然后去加载。

  Ctrl+S 快捷键保存，仅仅只需要设置用于保存的那个 Action 的 ShortCut 属性，下拉选择 Ctrl+S 就可以了。

  pcplayer 2025-11-14
----------------------------------------------------------------------------}
interface

uses
  Winapi.Messages, System.SysUtils, System.Variants, System.Types,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp,
  Vcl.ExtCtrls, Vcl.ComCtrls, Winapi.WebView2, Winapi.ActiveX, Vcl.Edge,
  System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls, UFrameFolders,
  UFrameRecentFile, Vcl.Menus, Vcl.Samples.Spin, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Vcl.Samples.Bind.Editors, Data.Bind.Components, Vcl.TitleBarCtrls,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.VirtualImage, Vcl.Imaging.jpeg, JvAppInst;

type
  TFmMain = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    EdgeBrowser1: TEdgeBrowser;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PanelLocalServer: TPanel;
    SpeedButton1: TSpeedButton;
    ActionManager1: TActionManager;
    AcOpen: TAction;
    AcSave: TAction;
    AcNew: TAction;
    AcLocalServer: TAction;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    ControlBar1: TControlBar;
    Splitter1: TSplitter;
    PanelLeft: TPanel;
    Panel1: TPanel;
    SpeedButton6: TSpeedButton;
    AcShowLeftPanel: TAction;
    FrameFolders1: TFrameFolders;
    Splitter2: TSplitter;
    Bevel1: TBevel;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    BindingsList1: TBindingsList;
    LinkControlToPropertyFontSize: TLinkControlToProperty;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    TitleBarPanel1: TTitleBarPanel;
    VCLStylesCB: TComboBox;
    VirtualImageList1: TVirtualImageList;
    ImageCollection1: TImageCollection;
    VirtualImage1: TVirtualImage;
    FrameRecentFile1: TFrameRecentFile;
    JvAppInstances1: TJvAppInstances;
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure AcLocalServerExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
    procedure AcOpenExecute(Sender: TObject);
    procedure AcSaveExecute(Sender: TObject);
    procedure AcShowLeftPanelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvAppInstances1CmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure Memo1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VCLStylesCBChange(Sender: TObject);
    procedure VCLStylesCBKeyPress(Sender: TObject; var Key: Char);
  private
    FServer: TIdHTTPWebBrokerBridge;  // Web Server

    FMarkdownFileName: string;
    procedure StartServer;
    function GetMarkdownFilePath: string;
    { Private declarations }
    procedure OpenFile(const FileName: string; const Preview: Boolean);
    procedure RichEditScrollToTop(const AEdit: TRichEdit);

    procedure DoOnFolderOpen(Sender: TObject);
    procedure DoOnRecentFileOpen(Sender: TObject);
    procedure ReadFileNameFromParam;
    function GetMyVersionInfo: string;

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    { Public declarations }
    property MarkdownFileName: string read FMarkdownFileName;
    property MarkdownFilePath: string read GetMarkdownFilePath;
  end;

var
  FmMain: TFmMain;

implementation


{$R *.dfm}

uses
  System.Threading, System.IOUtils,
{$IFDEF MSWINDOWS}
  WinApi.Windows, Winapi.ShellApi,
{$ENDIF}
  System.Generics.Collections, UDmJaMarkdown, Vcl.Themes, Vcl.Styles;


const LEFT_PANEL_WIDTH = 300;


procedure GetBuildInfo(FileName:string; var vs:string);
var VerInfoSize,VerValueSize,Dummy:DWORD;
    VerInfo: Pointer;
    VerValue: PVSFixedFileInfo;
    V1,V2,V3,V4:Word;
begin
  vs:='';
  if not FileExists(FileName) then exit;
  VerInfoSize:=GetFileVersionInfoSize(PChar(FileName),Dummy);
  if VerInfoSize=0 then exit;
  GetMem(VerInfo,VerInfoSize);
  if not GetFileVersionInfo(PChar(FileName),0,VerInfoSize,VerInfo) then exit;
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    V1:=dwFileVersionMS shr 16;
    V2:=dwFileVersionMS and $FFFF;
    V3:=dwFileVersionLS shr 16;
    V4:=dwFileVersionLS and $FFFF;
    vs:=inttostr(v1)+'.'+inttostr(v2)+'.'+inttostr(v3)+'.'+inttostr(v4);
  end;
  FreeMem(VerInfo,VerInfoSize);
end;

procedure TFmMain.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TFmMain.AcLocalServerExecute(Sender: TObject);
begin
  //模拟一个弹框
  PanelLocalServer.Left := Trunc((Self.Width - PanelLocalServer.Width) / 2);
  PanelLocalServer.Top := Trunc((Self.ClientHeight - PanelLocalServer.Height) / 2);
  PanelLocalServer.Visible := True;
  PanelLocalServer.BringToFront;
end;

procedure TFmMain.AcNewExecute(Sender: TObject);
begin
  Self.FMarkdownFileName := '';
  Memo1.Lines.Clear;
  PageControl1.ActivePageIndex := 0;  //显示编辑页
  EdgeBrowser1.NavigateToString('');
  Self.Caption := '新建文件';
end;

procedure TFmMain.AcOpenExecute(Sender: TObject);
var
  SL: TStringList;
begin
  //打开文件
  if OpenDialog1.Execute() then
  begin
    Self.OpenFile(OpenDialog1.FileName, False);
  end;
end;

procedure TFmMain.AcSaveExecute(Sender: TObject);
var
  APreview: Boolean;
begin
  APreview := False;
  if FMarkdownFileName <> '' then
  begin
    Memo1.Lines.SaveToFile(Self.FMarkdownFileName, TEncoding.UTF8);
    APreview := True;
  end
  else
  begin
    SaveDialog1.Title := 'Markdown编辑器另存为: ' + DmJaMarkdown.MyConfig.LastFolder;
    if SaveDialog1.Execute() then
    begin
      FMarkdownFileName := SaveDialog1.FileName;
      Memo1.Lines.SaveToFile(FMarkdownFileName, TEncoding.UTF8);
      APreview := True;
      Self.Caption := FMarkdownFileName;
      DmJaMarkdown.MyConfig.LastFolder := FMarkdownFileName;
    end;
  end;

  //刷新预览
  if APreview then
  begin
    EdgeBrowser1.Navigate('http://localhost:' + EditPort.Text);
    PageControl1.ActivePageIndex := 1;
  end;
end;

procedure TFmMain.AcShowLeftPanelExecute(Sender: TObject);
begin
  if PanelLeft.Width <= SpeedButton6.Width then
  begin
    PanelLeft.Width := LEFT_PANEL_WIDTH;
  end
  else PanelLeft.Width := SpeedButton6.Width;
end;

procedure TFmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TFmMain.ButtonOpenBrowserClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  LURL: string;
{$ENDIF}
begin
  StartServer;
{$IFDEF MSWINDOWS}
  LURL := Format('http://localhost:%s', [EditPort.Text]);
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
{$ENDIF}
end;

procedure TFmMain.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TFmMain.ButtonStopClick(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TFmMain.DoOnFolderOpen(Sender: TObject);
begin
  //打开数据库里面指定的文件夹
  if (Sender is TFrameFolders) then
  begin
    Self.OpenFile(TFrameFolders(Sender).FileName, True);
  end;
end;

procedure TFmMain.DoOnRecentFileOpen(Sender: TObject);
begin
  if (Sender is TFrameRecentFile) then
  begin
    Self.OpenFile(TFrameRecentFile(Sender).CurrentFileName, True);
  end;
end;

procedure TFmMain.FormActivate(Sender: TObject);
begin
  StartServer;
  Memo1.Font.Size := DmJaMarkdown.EditorFontSize;
  SpinEdit1.Value := DmJaMarkdown.EditorFontSize;

  EdgeBrowser1.CreateWebView;

  {---------------------------------------------------
    这里的问题是，如果先执行 EdgeBrowser1.CreateWebView; 然后再执行 TStyleManager.TrySetStyle
    会导致 EdgeBrowser1.CreateWebView 这个出错。因此之前搞了个延迟启动 Style，是等待 Edge 加载完成。
    现在改为先设置 Style，然后再加载 EdgeBrowser，就没有问题了。
    因此，在 OnCreate 里面设置 Style，然后在 OnActive 里面加载 EdgeBrowser，问题解决。
  VCLStylesCB.ItemIndex := VCLStylesCB.Items.IndexOf(DmJaMarkdown.MyConfig.StyleName);

  TTask.Run(
  procedure
  begin
    for var i := 0 to 3 do
    begin
      Sleep(100);
    end;

    TThread.Synchronize(nil,
    procedure
    begin
      try
        TStyleManager.TrySetStyle(VCLStylesCB.Items[VCLStylesCB.ItemIndex], False);
      except

      end;
    end);
  end);
  ---------------------------------------------}
end;

procedure TFmMain.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  for StyleName in TStyleManager.StyleNames do
    VCLStylesCB.Items.Add(StyleName);
  VCLStylesCB.ItemIndex := VCLStylesCB.Items.IndexOf(TStyleManager.ActiveStyle.Name);

  FServer := TIdHTTPWebBrokerBridge.Create(Self);

  FrameFolders1.OnFileOpen := Self.DoOnFolderOpen;
  FrameRecentFile1.OnFileOpen := Self.DoOnRecentFileOpen;
  PanelLeft.Width := DmJaMarkdown.LeftPanelWidth;

  VCLStylesCB.ItemIndex := VCLStylesCB.Items.IndexOf(DmJaMarkdown.MyConfig.StyleName);
  TStyleManager.TrySetStyle(VCLStylesCB.Items[VCLStylesCB.ItemIndex], False);

  DragAcceptFiles(Handle, True);  //让程序支持拖放文件到界面上；这句必须放在 Style 操作之后；放在前面不起作用。
  Self.ReadFileNameFromParam;
  DmJaMarkdown.LoadDataFromDB;
//  Self.CustomTitleBar.SystemColors := False;
//  Self.CustomTitleBar.BackgroundColor := clBlue;
end;

procedure TFmMain.FormResize(Sender: TObject);
begin
  //摆到右边
  SpeedButton5.Left := Self.ClientWidth - SpeedButton5.Width;
end;

procedure TFmMain.FormShow(Sender: TObject);
begin
  StatusBar1.Panels[2].Text :=  '版本号：' + Self.GetMyVersionInfo;
end;

function TFmMain.GetMarkdownFilePath: string;
begin
  Result := ExtractFilePath(Self.FMarkdownFileName);
end;

function TFmMain.GetMyVersionInfo: string;
begin
  GetBuildInfo(Application.ExeName, Result);
end;

procedure TFmMain.JvAppInstances1CmdLineReceived(Sender: TObject; CmdLine:
    TStrings);
var
  Fn: string;
begin
  if CmdLine.Count < 1 then Exit;
  Fn := CmdLine[0];
  Self.OpenFile(Fn, True);
end;

procedure TFmMain.Memo1Change(Sender: TObject);
begin
  AcSave.Enabled := True;
end;

procedure TFmMain.OpenFile(const FileName: string; const Preview: Boolean);
var
  SL: TStringList;
  AExt: string;
begin
  if not FileExists(FileName) then
  begin
    ShowMessage(FileName + '不存在');
    Exit;
  end;

  AExt := TPath.GetExtension(FileName);
  if Uppercase(AExt) <> '.MD' then Exit;

  FMarkdownFileName := FileName;
  EdgeBrowser1.Navigate('http://localhost:' + EditPort.Text);

  Memo1.Lines.LoadFromFile(FMarkdownFileName, TEncoding.UTF8);

  AcSave.Enabled := True;
  Self.Caption := 'JaMarkdown Editor: ' + FileName;

  //写入数据库
  DmJaMarkdown.LoadDataFromDB;
  DmJaMarkdown.SetCurrentFileName(FileName);
  DmJaMarkdown.MyConfig.LastFolder := FileName;

  if Preview then
  begin
    //显示浏览器
    PageControl1.ActivePageIndex := 1;
  end
  else PageControl1.ActivePageIndex := 0;
end;

procedure TFmMain.ReadFileNameFromParam;
var
  AFileName: string;
begin
  //Wiondows 资源管理器里面双击 .md 或者打开 .md 触发启动本程序；
  //本程序依靠读取参数来获得文件名
  // 获取命令行参数
  if ParamCount > 0 then
  begin
    AFileName := ParamStr(1);
    Self.OpenFile(AFileName, True);

//    // 验证文件是否存在
//    if FileExists(AFileName) then
//    begin
//      FMarkdownFileName := AFileName;
//
//      Self.OpenFile(AFileName, True);
//    end;
  end
end;

procedure TFmMain.RichEditScrollToTop(const AEdit: TRichEdit);
begin
  PageControl1.ActivePageIndex := 0; //必须切换过来，否则 RichEdit 无法 SetFocus
  AEdit.SelStart := 0;
  AEdit.SelLength := 0; // 可选：取消任何文本选择
  SendMessage(AEdit.Handle, EM_SCROLLCARET, 0, 0); // .ScrollToCaret;  // 滚动到光标位置
  AEdit.SetFocus;
end;

procedure TFmMain.SpeedButton1Click(Sender: TObject);
begin
  PanelLocalServer.Visible := False;
end;

procedure TFmMain.SpinEdit1Change(Sender: TObject);
begin
  DmJaMarkdown.EditorFontSize := SpinEdit1.Value;
end;

procedure TFmMain.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

procedure TFmMain.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := DateTimeToStr(Now);
end;

procedure TFmMain.VCLStylesCBChange(Sender: TObject);
begin
  //DragAcceptFiles(Handle, False);
  TStyleManager.TrySetStyle(VCLStylesCB.Text);
  DmJaMarkdown.MyConfig.StyleName := VCLStylesCB.Text;

  //目前的问题：切换 style 后，拖放支持失效。即便在这里加上也不行。可能原因：
  // Style 的切换是异步的，需要加上消息处理方法，在切换成功后的消息里面去执行重新注册的功能；
  //策略：因为 style 很少切换，通常切换完成后就不会再改变，因此这个问题暂时不处理。
  //DragAcceptFiles(Handle, True); //设置 style 后必须重新执行这句话，否则拖放支持失效
end;

procedure TFmMain.VCLStylesCBKeyPress(Sender: TObject; var Key: Char);
begin
//  Key := #0;
end;

procedure TFmMain.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array[0..MAX_PATH] of Char;
begin
  //处理 Windows 消息，.md 文件拖放过来。
  //以下代码测试通过；处理拖放消息；
  try
    if DragQueryFile(Msg.Drop, 0, FileName, SizeOf(FileName)) > 0 then
    begin
      Self.OpenFile(FileName, True);
    end;
  finally
    DragFinish(Msg.Drop);
    Msg.Result := 0;
  end;
end;

end.
