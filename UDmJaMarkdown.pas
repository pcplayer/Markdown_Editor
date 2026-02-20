unit UDmJaMarkdown;
{
  JaMarkdown 的数据库模块；

  业务功能，业务逻辑：
  1. 把数据保存进 SQLite 数据库；
  2. 记录打开过的 Markdown 文件，按照最近时间排序；
  3. 指定几个固定的文件夹（常用的文档的文件夹），记录下来，使用 DBGrid 打开让用户选择打开哪个文件夹；
  4.

  pcplayer 2025-11-14
}
interface

uses
  System.SysUtils, System.Classes, UJaMarkdownConfig, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TDmJaMarkdown = class(TDataModule)
    FDConnection1: TFDConnection;
    FdqFolders: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FdqRecentFiles: TFDQuery;
    FdqFoldersFolderName: TStringField;
    FdqFoldersFolderFullName: TStringField;
    FdqRecentFilesFileName: TStringField;
    FdqRecentFilesFullName: TStringField;
    FdqRecentFilesSummary: TStringField;
    FdqRecentFilesLastTime: TDateTimeField;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
    FConfig: TMarkdownConfig;


    function GetMyPath: string;
    function GetConfigFileName: string;
    function GetCurrentFolder: string;
    function GetCurrentFileName: string;
    function GetLeftPanelWidth: Integer;
    function GetEditorFontSize: Integer;
    procedure SetEditorFontSize(const Value: Integer);
  public
    { Public declarations }
    procedure SetCurrentFileName(const AFileName: string);
    procedure NewFolder(const FolderName: string);
    property CurrentFolder: string read GetCurrentFolder;
    property CurrentFileName: string read GetCurrentFileName;

    property LeftPanelWidth: Integer read GetLeftPanelWidth;
    property EditorFontSize: Integer read GetEditorFontSize write SetEditorFontSize;

    property MyConfig: TMarkdownConfig read FConfig; //还是直接暴露，代码会少写一点。

    procedure LoadDataFromDB;
    procedure Test;
  end;

var
  DmJaMarkdown: TDmJaMarkdown;

implementation

uses System.IOUtils, ObjectConfigSerializer;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDmJaMarkdown.DataModuleDestroy(Sender: TObject);
begin
  TObjectConfigSerializer.SaveToFile(FConfig, Self.GetConfigFileName);
  FConfig.Free;
end;

procedure TDmJaMarkdown.DataModuleCreate(Sender: TObject);
begin
  FConfig := TMarkdownConfig.Create;
  TObjectConfigSerializer.LoadFromFile(FConfig, Self.GetConfigFileName);


end;

procedure TDmJaMarkdown.FDConnection1BeforeConnect(Sender: TObject);
begin
  FdConnection1.Params.Values['DataBase'] := Self.FConfig.DataBase;
end;

function TDmJaMarkdown.GetConfigFileName: string;
begin
  Result := TPath.Combine(Self.GetMyPath, 'MyConfig.txt');
end;

function TDmJaMarkdown.GetCurrentFileName: string;
begin
  Result := FdqRecentFiles.FieldByName('FullName').AsString;
end;

function TDmJaMarkdown.GetCurrentFolder: string;
begin
  Result := FdqFolders.FieldByName('FolderFullName').AsString;
end;

function TDmJaMarkdown.GetEditorFontSize: Integer;
begin
  Result := FConfig.EditorFontSize;
end;

function TDmJaMarkdown.GetLeftPanelWidth: Integer;
begin
  Result := FConfig.LeftPanelWidth;
end;

function TDmJaMarkdown.GetMyPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TDmJaMarkdown.LoadDataFromDB;
begin
  if FdqFolders.Active then Exit;
  
  FdqFolders.Open();
  FdqRecentFiles.Open();
end;

procedure TDmJaMarkdown.NewFolder(const FolderName: string);
var
  AName: string;
begin
  with FdqFolders do
  begin
    if not Active then Open;
    if Locate('FolderFullName', FolderName, []) then Exit;

    AName := ExtractFileName(FolderName);
    Insert;
    FieldByName('FolderName').AsString := AName;
    FieldByName('FolderFullName').AsString := FolderName;
    Post;
  end;
end;

procedure TDmJaMarkdown.SetCurrentFileName(const AFileName: string);
var
  Fn: string;
begin
  with FdqRecentFiles do
  begin
    if Locate('FullName', AFileName, []) then
    begin
      Edit;
      FieldByName('LastTime').AsDateTime := Now;
      Post;
    end
    else
    begin
      Fn := ExtractFileName(AFileName);
      Insert;

      FieldByName('FileName').AsString := Fn;
      FieldByName('FullName').AsString := AFileName;
      FieldByName('LastTime').AsDateTime := Now;
      Post;
    end;
  end;
end;

procedure TDmJaMarkdown.SetEditorFontSize(const Value: Integer);
begin
  FConfig.EditorFontSize := Value; //这里如果采用接口委托，就不用重复写这样的代码了。
end;

procedure TDmJaMarkdown.Test;
begin


end;

end.
