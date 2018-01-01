unit DataGrabber.DataView.Base;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Data.DB,

  DataGrabber.Interfaces;

type
  TBaseDataView = class(TForm, IDataView)
    dscMain: TDataSource;

  private
    FData         : IData;
    FDataSet      : TDataSet;
    FManager      : IConnectionViewManager;
    FToolBar      : TToolBar;
    FToolBarPanel : TPanel;

  protected
    {$REGION 'property access methods'}
    function GetName: string; virtual;
    function GetGridType: string; virtual;
    function GetSettings: IDataViewSettings;
    function GetData: IData;
    function GetRecordCount: Integer; virtual;
    function GetPopupMenu: TPopupMenu; reintroduce; virtual;
    procedure SetPopupMenu(const Value: TPopupMenu); virtual;
    function GetDataSet: TDataSet; virtual;
    {$ENDREGION}

    procedure DataAfterExecute(Sender: TObject); virtual;
    procedure SettingsChanged(Sender: TObject); virtual;

    procedure AssignParent(AParent: TWinControl); virtual;
    procedure UpdateView; virtual;
    procedure HideSelectedColumns; virtual;
    function IsActiveDataView: Boolean; virtual;
    procedure Inspect; virtual;
    procedure AutoSizeColumns; virtual;
    procedure Copy; virtual;
    procedure Close; virtual;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    function SelectionToCommaText(
      AQuoteItems: Boolean = True
    ): string; virtual;
    function SelectionToDelimitedTable(
      ADelimiter     : string = #9;
      AIncludeHeader : Boolean = True
    ): string; virtual;
    function SelectionToTextTable(
      AIncludeHeader: Boolean = False
    ): string; virtual;
    function SelectionToWikiTable(
      AIncludeHeader: Boolean = False
    ): string; virtual;
    function SelectionToFields(
      AQuoteItems: Boolean = True
    ): string; virtual;

    procedure ApplyGridSettings; virtual;
    procedure UpdateActions; override;

   public
     constructor Create(
       AOwner    : TComponent;
       AManager  : IConnectionViewManager;
       AData     : IData;
       ADataSet  : TDataSet = nil
     ); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Name: string
      read GetName;

    property GridType: string
      read GetGridType;

    property DataSet: TDataSet
      read GetDataSet;

    property Data: IData
      read GetData;

    property RecordCount: Integer
      read GetRecordCount;

    property Settings: IDataViewSettings
      read GetSettings;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

  end;

implementation

{$R *.dfm}

uses
  Spring,

  DataGrabber.Utils, DataGrabber.Factories;

{$REGION 'construction and destruction'}
constructor TBaseDataView.Create(AOwner: TComponent; AManager:
  IConnectionViewManager; AData: IData; ADataSet: TDataSet);
begin
  inherited Create(AOwner);
  Guard.CheckNotNull(AManager, 'AManager');
  Guard.CheckNotNull(AData, 'AData');
  FData    := AData;
  FManager := AManager;
  if Assigned(ADataSet) then
    FDataSet := ADataSet
  else
    FDataSet := FData.DataSet;
  dscMain.DataSet := FDataSet;
end;

procedure TBaseDataView.AfterConstruction;
begin
  inherited AfterConstruction;
  FToolBarPanel            := TPanel.Create(Self);
  FToolBarPanel.Parent     := Self;
  FToolBarPanel.BevelOuter := bvNone;
  FToolBarPanel.Color      := clWhite;
  FToolBarPanel.Align      := alRight;
  FToolBarPanel.Visible    := False;
  FToolbar := TDataGrabberFactories.CreateDataViewToolbar(
    Self,
    FToolBarPanel,
    FManager
  );
  FToolbar.Align         := alRight;
  FToolbar.Visible       := True;
  FToolbar.Transparent   := True;
  FToolbar.AutoSize      := True;
  FToolBarPanel.AutoSize := True;
  Data.OnAfterExecute.Add(DataAfterExecute);
  Settings.OnChanged.Add(SettingsChanged);
end;

procedure TBaseDataView.BeforeDestruction;
begin
  Settings.OnChanged.Remove(SettingsChanged);
  FData.OnAfterExecute.Remove(DataAfterExecute);
  FDataSet  := nil;
  FData     := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TBaseDataView.GetData: IData;
begin
  Result := FData;
end;

function TBaseDataView.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

function TBaseDataView.GetGridType: string;
begin
  Result := '';
end;

function TBaseDataView.GetName: string;
begin
  Result := inherited Name;
end;

function TBaseDataView.GetPopupMenu: TPopupMenu;
begin
  Result := inherited PopupMenu;
end;

procedure TBaseDataView.SetPopupMenu(const Value: TPopupMenu);
begin
  inherited PopupMenu := Value;
end;

function TBaseDataView.GetRecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TBaseDataView.GetSettings: IDataViewSettings;
begin
  Result := FManager.Settings as IDataViewSettings;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TBaseDataView.DataAfterExecute(Sender: TObject);
begin
  UpdateView;
end;

procedure TBaseDataView.SettingsChanged(Sender: TObject);
begin
  ApplyGridSettings;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TBaseDataView.ApplyGridSettings;
begin
//
end;

procedure TBaseDataView.AssignParent(AParent: TWinControl);
begin
  Parent      := AParent;
  BorderStyle := bsNone;
  Align       := alClient;
  Visible     := True;
end;

procedure TBaseDataView.AutoSizeColumns;
begin
//
end;

procedure TBaseDataView.BeginUpdate;
begin
//
end;

procedure TBaseDataView.EndUpdate;
begin
//
end;

procedure TBaseDataView.Close;
begin
//
end;

procedure TBaseDataView.Copy;
begin
//
end;

procedure TBaseDataView.HideSelectedColumns;
begin
//
end;

procedure TBaseDataView.Inspect;
begin
//
end;

function TBaseDataView.IsActiveDataView: Boolean;
begin
  Result := ContainsFocus(Self);
end;

function TBaseDataView.SelectionToCommaText(AQuoteItems: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToDelimitedTable(ADelimiter: string;
  AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToFields(AQuoteItems: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToTextTable(AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

function TBaseDataView.SelectionToWikiTable(AIncludeHeader: Boolean): string;
begin
  Result := '';
end;

procedure TBaseDataView.UpdateActions;
begin
  inherited UpdateActions;
  FToolBarPanel.Visible := IsActiveDataView;
end;

procedure TBaseDataView.UpdateView;
begin
  UpdateActions;
end;
{$ENDREGION}

end.
