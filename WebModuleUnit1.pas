unit WebModuleUnit1;
{
  pcplayer 2025-11-14
}
interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    WebFileDispatcher1: TWebFileDispatcher;
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleBeforeDispatch(Sender: TObject; Request: TWebRequest;
        Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
    function GetTemplatePath: string;
    function LoadMarkdownFileToHTML(const Fn: string): string;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses System.IOUtils, MarkdownProcessor, MarkdownUtils, UFmMain;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TWebModule1.GetTemplatePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function TWebModule1.LoadMarkdownFileToHTML(const Fn: string): string;
var
  SL: TStringList;
  Processor: TMarkdownProcessor;
  S: string;
  TempFn, TempStr: string;
begin
  if not FileExists(Fn) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(Fn, TEncoding.UTF8);
    S := SL.Text;
  finally
    SL.Free;
  end;

  Processor := TMarkdownProcessor.CreateDialect(TMarkdownProcessorDialect.mdCommonMark);
  try
    S := Processor.Process(S);
  finally
    Processor.Free;
  end;

  //加载模板
  TempFn := Self.GetTemplatePath; // ExtractFilePath(ParamStr(0));
  TempFn := TPath.Combine(TempFn, 'Template.html');
  SL := TStringList.Create;
  try
    SL.LoadFromFile(TempFn, TEncoding.UTF8);
    TempStr := SL.Text;
  finally
    SL.Free;
  end;

  Result := TempStr.Replace('#MyContent', S);
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
{-----------------------------------------------------------------------
  加载 Markdown 文件，转换为 HTML 输出

------------------------------------------------------------------------}
  if Request.PathInfo = '/' then
  begin
    Response.Content := Self.LoadMarkdownFileToHTML(FmMain.MarkdownFileName);
    Handled := True;
  end;
end;

procedure TWebModule1.WebModuleBeforeDispatch(Sender: TObject; Request:
    TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Fext: string;
begin
  Fext := Lowercase(ExtractFileExt(Request.PathInfo));

  if (Fext = '.jpg') or (Fext = '.png') or (Fext = '.bmp')  then
  begin
    WebFileDispatcher1.RootDirectory := FmMain.MarkdownFilePath; //图片路径
  end
  else
  if (Fext = '.css') or (Fext = '.js') then
  begin
    WebFileDispatcher1.RootDirectory := Self.GetTemplatePath;
  end;
end;

end.
