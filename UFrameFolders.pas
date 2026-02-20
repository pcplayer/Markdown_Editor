unit UFrameFolders;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids,
  Vcl.DBGrids, System.Actions, Vcl.ActnList, Vcl.Menus;

type
  TFrameFolders = class(TFrame)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    FileOpenDialog1: TFileOpenDialog;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    AcSelectFolder: TAction;
    N1: TMenuItem;
    AcDelete: TAction;
    AcOpenFile: TAction;
    N2: TMenuItem;
    N3: TMenuItem;
    procedure AcDeleteExecute(Sender: TObject);
    procedure AcOpenFileExecute(Sender: TObject);
    procedure AcSelectFolderExecute(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
  private
    { Private declarations }
    FOnFileOpen: TNotifyEvent;
    FFileName: string;

    procedure DoOnFileOpen;
    function GetFileName: string;
    procedure OpenFile;
  public
    { Public declarations }

    property OnFileOpen: TNotifyEvent read FOnFileOpen write FOnFileOpen;

    property FileName: string read GetFileName;
  end;

implementation

uses UDmJaMarkdown;

{$R *.dfm}

procedure TFrameFolders.AcDeleteExecute(Sender: TObject);
begin
  DataSource1.DataSet.Delete;
end;

procedure TFrameFolders.AcOpenFileExecute(Sender: TObject);
begin
  Self.OpenFile;
end;

procedure TFrameFolders.AcSelectFolderExecute(Sender: TObject);
begin
  FileOpenDialog1.Options := FileOpenDialog1.Options + [TFileDialogOption.fdoPickFolders];
  FileOpenDialog1.Title := '选择文件夹';
  if FileOpenDialog1.Execute then
  begin
    DmJaMarkdown.NewFolder(FileOpenDialog1.FileName);
  end;
end;

procedure TFrameFolders.DBGrid1DblClick(Sender: TObject);
begin
  Self.OpenFile;
end;

procedure TFrameFolders.DoOnFileOpen;
begin
  if Assigned(FOnFileOpen) then FOnFileOpen(Self);
end;

function TFrameFolders.GetFileName: string;
begin
  Result := Self.FFileName;
end;

procedure TFrameFolders.OpenFile;
begin
  FileOpenDialog1.DefaultFolder := DmJaMarkdown.CurrentFolder;
  FileOpenDialog1.Options := FileOpenDialog1.Options - [TFileDialogOption.fdoPickFolders];
  FileOpenDialog1.Title := '打开文件';
  if FileOpenDialog1.Execute then
  begin
    FFileName := FileOpenDialog1.FileName;
    Self.DoOnFileOpen;
  end;
end;

end.
