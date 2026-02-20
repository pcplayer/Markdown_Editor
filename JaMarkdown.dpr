program JaMarkdown;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  UFmMain in 'UFmMain.pas' {FmMain},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  MarkdownProcessor in 'D:\Delphi\GitHub\Markdown3\MarkdownProcessor\source\MarkdownProcessor.pas',
  MarkdownUtils in 'D:\Delphi\GitHub\Markdown3\MarkdownProcessor\source\MarkdownUtils.pas',
  ObjectConfigSerializer in 'ObjectConfigSerializer.pas',
  UJaMarkdownConfig in 'UJaMarkdownConfig.pas',
  UDmJaMarkdown in 'UDmJaMarkdown.pas' {DmJaMarkdown: TDataModule},
  UFrameFolders in 'UFrameFolders.pas' {FrameFolders: TFrame},
  UFrameRecentFile in 'UFrameRecentFile.pas' {FrameRecentFile: TFrame},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  TStyleManager.TrySetStyle('Sky');
  Application.CreateForm(TDmJaMarkdown, DmJaMarkdown);
  Application.CreateForm(TFmMain, FmMain);

  Application.Run;
end.
