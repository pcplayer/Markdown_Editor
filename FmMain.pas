unit FmMain;
{---------------------------------------------------------------------------
  markdown 文件编辑器。预览使用浏览器。内置 WebServer。
  默认文件里面的图片和文件在相同目录。因此，加载文件时，要根据文件目录，指定 WEB 的 Root，
  方便浏览器加载图片。

  但是，CSS/JS 这些，应该是在固定位置。因此，需要判断浏览器需要的 CSS/JS 然后去加载。

  pcplayer 2025-11-14
----------------------------------------------------------------------------}
interface

uses
  Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, IdGlobal, Web.HTTPApp,
  Vcl.ExtCtrls, Vcl.ComCtrls, Winapi.WebView2, Winapi.ActiveX, Vcl.Edge,
  System.Actions, Vcl.ActnList;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    RichEdit1: TRichEdit;
    EdgeBrowser1: TEdgeBrowser;
    Button1: TButton;
    ActionList1: TActionList;
    AcOpen: TAction;
    OpenDialog1: TOpenDialog;
    procedure AcOpenExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;

    FMarkdownFileName: string;


    procedure StartServer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Winapi.ShellApi,
{$ENDIF}
  System.Generics.Collections;

procedure TForm1.AcOpenExecute(Sender: TObject);
begin
  //打开文件
  if OpenDialog1.Execute() then
  begin
    FMarkdownFileName := OpenDialog1.FileName;

    EdgeBrowser1.Navigate('http://localhost:' + EditPort.Text);
  end;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure TForm1.ButtonOpenBrowserClick(Sender: TObject);
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

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
end;

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

end.
