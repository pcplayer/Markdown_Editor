unit UFrameRecentFile;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls,
  Vcl.DBCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids, System.Actions,
  Vcl.ActnList, Vcl.Menus;

type
  TFrameRecentFile = class(TFrame)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Splitter1: TSplitter;
    DBMemo1: TDBMemo;
    ActionList1: TActionList;
    PopupMenu1: TPopupMenu;
    AcOpen: TAction;
    N1: TMenuItem;
    AcDelete: TAction;
    N2: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    DBText1: TDBText;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure AcDeleteExecute(Sender: TObject);
    procedure AcOpenExecute(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure DBGrid1DblClick(Sender: TObject);
  private
    FOnFileOpen: TNotifyEvent;

    function GetCurrentFileName: string;
    procedure OpenSelectedFile;
  public
    { Public declarations }
    property CurrentFileName: string read GetCurrentFileName;

    property OnFileOpen: TNotifyEvent read FOnFileOpen write FOnFileOpen;
  end;

implementation

uses UDmJaMarkdown;

{$R *.dfm}

procedure TFrameRecentFile.AcDeleteExecute(Sender: TObject);
begin
  DataSource1.DataSet.Delete;
end;

procedure TFrameRecentFile.AcOpenExecute(Sender: TObject);
begin
  Self.OpenSelectedFile;
end;

procedure TFrameRecentFile.DBGrid1CellClick(Column: TColumn);
begin
  DBGrid1.Hint := DBGrid1.DataSource.DataSet.FieldByName('FileName').AsString;
end;

procedure TFrameRecentFile.DBGrid1DblClick(Sender: TObject);
begin
  Self.OpenSelectedFile;
end;

function TFrameRecentFile.GetCurrentFileName: string;
begin
  Result := DmJaMarkdown.CurrentFileName;
end;

procedure TFrameRecentFile.OpenSelectedFile;
begin
  if Assigned(FOnFileOpen) then FOnFileOpen(Self);
end;

end.
