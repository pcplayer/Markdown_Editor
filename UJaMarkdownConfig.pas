unit UJaMarkdownConfig;

interface

uses System.SysUtils, ObjectConfigSerializer;

type
  TMarkdownConfig = class(TBaseConfig)
  private
    FDBFileName: string;
    FWebServerPort: Integer;
    FLeftPanelWidth: Integer;
    FFontSize: Integer;
    FStyleName: string;
    FLastFolder: string;
    procedure SetLastFolder(const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy;

    procedure AfterLoad; virtual;
    procedure BeforeSave; virtual;
  published
    [ConfigProperty('数据库文件名')]
    property DataBase: string read FDBFileName write FDBFileName;

    [ConfigProperty('本地WebServer端口号')]
    property WebPort: Integer read FWebServerPort write FWebServerPort;

    [ConfigProperty('主界面左侧面板宽度')]
    property LeftPanelWidth: Integer read FLeftPanelWidth write FLeftPanelWidth;

    [ConfigProperty('编辑器字体大小')]
    property EditorFontSize: Integer read FFontSize write FFontSize;

    [ConfigProperty('窗体风格')]
    property StyleName: string read FStyleName write FStyleName;

    [ConfigProperty('上次打开的文件夹')]
    property LastFolder: string read FLastFolder write SetLastFolder;
  end;

implementation

{ TMarkdownConfig }

procedure TMarkdownConfig.AfterLoad;
begin
  inherited;
end;

procedure TMarkdownConfig.BeforeSave;
begin
  inherited;
end;

constructor TMarkdownConfig.Create;
begin
  FWebServerPort := 8080;
  FLeftPanelWidth := 300;
  FFontSize := 11;
  FStyleName := 'Sky';
  inherited;
end;

destructor TMarkdownConfig.Destroy;
begin
  inherited;
end;

procedure TMarkdownConfig.SetLastFolder(const Value: string);
begin
  FLastFolder := ExtractFilePath(Value);
end;

end.
