unit DataGrabber.VirtualDBTree;

{ Author: Tim Sinaeve
  Coded a long time ago for applications like BATBase, RecordPlayer and
  SnippetSource and brought back to life.

  (16/01/2011)

  REMARK:

  ClipboardFormats.Text needs to be assigned if we want to support Drag
  operations. After a long investigation (13/06/2010) this turned out to be
  the reason why drag and drop did not work if we dynamically create the
  TCheckVirtualDBTreeEx instance.
  At designtime this property is assigned by default.
}

//*****************************************************************************

interface

uses
  ActnList, Buttons, Classes, ComCtrls, Controls, DB, DBCtrls, Dialogs,
  ExtCtrls, Forms, Graphics, ImgList, Menus, Messages, ToolWin, Variants,
  Windows, ActiveX,

  VirtualTrees;

  //VirtualDBTreeEx;

{ Form holding data-aware treeview for flat database files.
  To support this treeview, the table must include following fields for storing
  the node data:

  ID    : the primary key of the table
  Parent: reference to the parent node ID (0 means that record is the rootnode)
  Type  : node-type      0 : node item
                         1 : folder node
  Name  : name to be shown as the node identifier in the treeview }

{ TODO: support DB-update for a selection of multiple nodes like for a delete
        operation. }

const
  DEFAULT_KEYFIELDNAME    = 'ID';
  DEFAULT_LEVELFIELDNAME  = 'ID';
  DEFAULT_PARENTFIELDNAME = 'ParentID';
  DEFAULT_PATHFIELDNAME   = '';
  DEFAULT_IMGIDXFIELDNAME = 'NodeType';
  DEFAULT_VIEWFIELDNAME   = 'NodeName';

type
  TNewFolderNodeEvent = procedure(Sender: TObject) of object;
  TNewItemNodeEvent   = procedure(Sender: TObject) of object;
  TDropFilesEvent     = procedure(
    Sender : TBaseVirtualTree;
    AFiles : TStrings;
    AAttachMode : TVTNodeAttachMode
    ) of object;

type
  TfrmVirtualDBTree = class(TForm)
    {$REGION 'designer controls'}
    actCollapseAllNodes    : TAction;
    actDeleteSelectedNodes : TAction;
    actExpandAllNodes      : TAction;
    actInspectComponent    : TAction;
    actNewFolderNode       : TAction;
    actNewItemNode         : TAction;
    actNewRootFolderNode   : TAction;
    actSavePersistent      : TAction;
    alsMain                : TActionList;
    btnCollapseNodes       : TToolButton;
    btnDelete              : TToolButton;
    btnExpandNodes         : TToolButton;
    btnNewFolder           : TToolButton;
    btnNewItem             : TToolButton;
    btnNewRoot             : TToolButton;
    dscMain                : TDataSource;
    mniDelete              : TMenuItem;
    mniNewChild            : TMenuItem;
    mniNewFolder           : TMenuItem;
    mniNewRoot             : TMenuItem;
    N1                     : TMenuItem;
    pnlBottom              : TPanel;
    pnlMain                : TPanel;
    pnlTop                 : TPanel;
    ppmTreeView            : TPopupMenu;
    tlbBottomRight         : TToolBar;
    tlbTopLeft             : TToolBar;
    vtvMain                : TCheckVirtualDBTreeEx;
    imlTreeView            : TImageList;
    tlbNavigator           : TToolBar;
    actApply               : TAction;
    actCancel              : TAction;
    btnApply               : TToolButton;
    btnCancel              : TToolButton;
    imlMain                : TImageList;
    {$ENDREGION}

    procedure actNewRootFolderNodeExecute(Sender: TObject);
    procedure actNewFolderNodeExecute(Sender: TObject);
    procedure actNewItemNodeExecute(Sender: TObject);
    procedure actDeleteSelectedNodesExecute(Sender: TObject);
    procedure actExpandAllNodesExecute(Sender: TObject);
    procedure actCollapseAllNodesExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);

    procedure vtvMainDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);

    procedure vtvMainEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vtvMainDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vtvMainDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);

  private
    FOnDropFiles    : TDropFilesEvent;
    FDataSet        : TDataSet;
    FOnNewFolderNode: TNewFolderNodeEvent;
    FOnNewItemNode  : TNewItemNodeEvent;
    function GetImgIdxField: TField;
    function GetImgIdxFieldName: string;
    function GetKeyField: TField;
    function GetKeyFieldName: string;
    function GetLevelField: TField;
    function GetLevelFieldName: string;
    function GetParentField: TField;
    function GetParentFieldName: string;
    function GetPathField: TField;
    function GetPathFieldName: string;
    function GetViewField: TField;
    function GetViewFieldName: string;
    procedure SetImgIdxFieldName(const AValue: string);
    procedure SetKeyFieldName(const AValue: string);
    procedure SetLevelFieldName(const AValue: string);
    procedure SetParentFieldName(const AValue: string);
    procedure SetPathFieldName(const AValue: string);
    procedure SetViewFieldName(const AValue: string);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetMultiSelect(const Value: Boolean);
    function GetMultiSelect: Boolean;
    procedure SetToolbarBottomVisible(const Value: Boolean);
    procedure SetToolbarTopVisible(const Value: Boolean);
    function GetToolbarBottomVisible: Boolean;
    function GetToolbarTopVisible: Boolean;

  protected
    procedure PostTreeData(AParentID, ANodeType: Integer; const AName: string);
      virtual;
    // property access methods
    // event dispatch methods
    procedure DoNewFolderNode; dynamic;
    procedure DoNewItemNode; dynamic;
    procedure DoDropFiles(AFiles: TStrings;
      const AAttachMode : TVTNodeAttachMode); dynamic;
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure NewFolderNode;
    procedure NewItemNode;
    procedure NewSubItemNode;
    procedure NewRootFolderNode;
    // public properties
    property KeyField: TField
      read GetKeyField;

    property ParentField: TField
      read GetParentField;

    property LevelField: TField
      read GetLevelField;

    property ImgIdxField: TField
      read GetImgIdxField;

    property PathField: TField
      read GetPathField;

    property ViewField: TField
      read GetViewField;

    property DataSet: TDataSet
      read FDataSet write SetDataSet;

    property MultiSelect: Boolean
      read GetMultiSelect write SetMultiSelect default True;

    property ToolbarTopVisible: Boolean
      read GetToolbarTopVisible write SetToolbarTopVisible default True;

    property ToolbarBottomVisible: Boolean
      read GetToolbarBottomVisible write SetToolbarBottomVisible default True;

    property KeyFieldName: string
      read GetKeyFieldName write SetKeyFieldName;

    property ParentFieldName: string
      read GetParentFieldName write SetParentFieldName;

    property LevelFieldName: string
      read GetLevelFieldName write SetLevelFieldName;

    property PathFieldName: string
      read GetPathFieldName write SetPathFieldName;

    property ImgIdxFieldName: string
      read GetImgIdxFieldName write SetImgIdxFieldName;

    property ViewFieldName: string
      read GetViewFieldName write SetViewFieldName;

    // public events
    property OnNewFolderNode: TNewFolderNodeEvent
      read FOnNewFolderNode write FOnNewFolderNode;

    property OnNewItemNode: TNewItemNodeEvent
      read FOnNewItemNode write FOnNewItemNode;

    property OnDropFiles: TDropFilesEvent
      read FOnDropFiles write FOnDropFiles;
  end;


//*****************************************************************************

implementation

{$R *.dfm}

uses
  SysUtils, DBClient;

resourcestring
  SDeleteSelectedItems = 'Are you sure you want to delete te selected item(s)?';
  SNewFolder = 'New folder';
  SNew = 'New';

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TfrmVirtualDBTree.AfterConstruction;
begin
  inherited AfterConstruction;
  Doublebuffered := True;
  MultiSelect := True;
  ToolbarTopVisible := True;
  ToolbarBottomVisible := True;
  KeyFieldName := DEFAULT_KEYFIELDNAME;
  LevelFieldName := DEFAULT_LEVELFIELDNAME;
  ParentFieldName := DEFAULT_PARENTFIELDNAME;
  PathFieldName := DEFAULT_PATHFIELDNAME;
  ImgIdxFieldName := DEFAULT_IMGIDXFIELDNAME;
  ViewFieldName := DEFAULT_VIEWFIELDNAME;
end;

procedure TfrmVirtualDBTree.BeforeDestruction;
begin
  FDataSet := nil;
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

procedure TfrmVirtualDBTree.SetDataSet(const Value: TDataSet);
begin
  if Value <> DataSet then
  begin
    dscMain.DataSet := Value;
    FDataSet := Value;
  end;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetToolbarBottomVisible: Boolean;
begin
  Result := pnlBottom.Visible;
end;

procedure TfrmVirtualDBTree.SetToolbarBottomVisible(const Value: Boolean);
begin
  pnlBottom.Visible := Value;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetToolbarTopVisible: Boolean;
begin
  Result := pnlTop.Visible;
end;

procedure TfrmVirtualDBTree.SetToolbarTopVisible(const Value: Boolean);
begin
  pnlTop.Visible := Value;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetMultiSelect: Boolean;
begin
  Result := toMultiSelect in vtvMain.TreeOptions.SelectionOptions;
end;

procedure TfrmVirtualDBTree.SetMultiSelect(const Value: Boolean);
begin
  if Value then
    vtvMain.TreeOptions.SelectionOptions :=
      vtvMain.TreeOptions.SelectionOptions + [toMultiSelect]
  else
    vtvMain.TreeOptions.SelectionOptions :=
      vtvMain.TreeOptions.SelectionOptions - [toMultiSelect];
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetKeyFieldName: string;
begin
  Result := vtvMain.KeyFieldName;
end;

procedure TfrmVirtualDBTree.SetKeyFieldName(const AValue: string);
begin
  vtvMain.KeyFieldName := AValue;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetLevelField: TField;
begin
  Result := vtvMain.LevelField;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetImgIdxFieldName: string;
begin
  Result := vtvMain.ImgIdxFieldName;
end;

procedure TfrmVirtualDBTree.SetImgIdxFieldName(const AValue: string);
begin
  vtvMain.ImgIdxFieldName := AValue;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetImgIdxField: TField;
begin
  Result := vtvMain.ImgIdxField;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetKeyField: TField;
begin
  Result := vtvMain.KeyField;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetLevelFieldName: string;
begin
  Result := vtvMain.LevelFieldName;
end;

procedure TfrmVirtualDBTree.SetLevelFieldName(const AValue: string);
begin
  vtvMain.LevelFieldName := AValue;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetParentField: TField;
begin
  Result := vtvMain.ParentField;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetParentFieldName: string;
begin
  Result := vtvMain.ParentFieldName;
end;

procedure TfrmVirtualDBTree.SetParentFieldName(const AValue: string);
begin
  vtvMain.ParentFieldName := AValue;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetPathField: TField;
begin
  Result := vtvMain.PathField;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetPathFieldName: string;
begin
  Result := vtvMain.PathFieldName;
end;

procedure TfrmVirtualDBTree.SetPathFieldName(const AValue: string);
begin
  vtvMain.PathFieldName := AValue;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetViewField: TField;
begin
  Result := vtvMain.ViewField;
end;

//-----------------------------------------------------------------------------

function TfrmVirtualDBTree.GetViewFieldName: string;
begin
  Result := vtvMain.ViewFieldName;
end;

procedure TfrmVirtualDBTree.SetViewFieldName(const AValue: string);
begin
  vtvMain.ViewFieldName := AValue;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event dispatch methods                                                BEGIN
//*****************************************************************************

procedure TfrmVirtualDBTree.DoNewFolderNode;
begin
  if Assigned(FOnNewFolderNode) then
    FOnNewFolderNode(Self);
end;

procedure TfrmVirtualDBTree.DoNewItemNode;
begin
  if Assigned(FOnNewItemNode) then
    FOnNewItemNode(Self);
end;

procedure TfrmVirtualDBTree.DoDropFiles(AFiles: TStrings;
  const AAttachMode : TVTNodeAttachMode);
begin
  if Assigned(AFiles) and Assigned(FOnDropFiles) then
    FOnDropFiles(vtvMain, AFiles, AAttachMode);
end;
//*****************************************************************************
// event dispatch methods                                                  END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmVirtualDBTree.actNewRootFolderNodeExecute(Sender: TObject);
begin
  NewRootFolderNode;
end;

procedure TfrmVirtualDBTree.actNewFolderNodeExecute(Sender: TObject);
begin
  NewFolderNode;
end;

procedure TfrmVirtualDBTree.actNewItemNodeExecute(Sender: TObject);
begin
  NewItemNode;
end;

procedure TfrmVirtualDBTree.actDeleteSelectedNodesExecute(
  Sender: TObject);
begin
  if MessageDlg(SDeleteSelectedItems, mtWarning, [mbYes, mbNo], 0)
    in [mrYes] then
    vtvMain.DeleteSelection;
end;

procedure TfrmVirtualDBTree.actExpandAllNodesExecute(Sender: TObject);
begin
  vtvMain.ExpandAll;
end;

procedure TfrmVirtualDBTree.actApplyExecute(Sender: TObject);
begin
  FDataSet.Post;
end;

procedure TfrmVirtualDBTree.actCancelExecute(Sender: TObject);
begin
  FDataSet.Cancel;
end;

procedure TfrmVirtualDBTree.actCollapseAllNodesExecute(Sender: TObject);
begin
  vtvMain.CollapseAll;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

//= TfrmVirtualDBTree =========================================================

//= vtvMain ===================================================================

procedure TfrmVirtualDBTree.vtvMainDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmVirtualDBTree.vtvMainDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  I          : Integer;
  SL         : TStringList;
  AttachMode : TVTNodeAttachMode;
begin
  if Mode = dmOnNode then
    AttachMode := amInsertBefore
  else if Mode = dmAbove then
    AttachMode := amInsertBefore
  else if Mode = dmBelow then
    AttachMode := amInsertAfter
  else
    AttachMode := amAddChildLast;
  SL := TStringList.Create;
  try
    for I := 0 to High(Formats) - 1 do
    begin
      if (Formats[I] = CF_HDROP) then
      begin
        //GetFileListFromObj(DataObject, SL);
        DoDropFiles(SL, AttachMode);
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmVirtualDBTree.vtvMainDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmVirtualDBTree.vtvMainEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  ViewField.AsString := vtvMain.NodeText[Node];
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TfrmVirtualDBTree.UpdateActions;
begin
  inherited;
  actApply.Enabled := Assigned(FDataSet) and (FDataSet.State in dsEditModes);
  actCancel.Enabled := actApply.Enabled;
end;

//-----------------------------------------------------------------------------

procedure TfrmVirtualDBTree.PostTreeData(AParentID, ANodeType: Integer;
  const AName: string);
begin
  DataSet.Append;
  ParentField.AsInteger := AParentID;
  ImgIdxField.AsInteger := ANodeType;
  ViewField.AsString := AName;
  DataSet.Post;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TfrmVirtualDBTree.NewFolderNode;
var
  iNodeType: Integer;
  iID      : Integer;
begin
  iID := KeyField.AsInteger;
  iNodeType := ImgIdxField.AsInteger;
  if iNodeType <> 1 then
    iID := 0;
  PostTreeData(iID, 1, SNewFolder);
  DoNewFolderNode;
end;

procedure TfrmVirtualDBTree.NewItemNode;
var
  ID: Integer;
begin
  if ImgIdxField.AsInteger = 0 then
    ID := ParentField.AsInteger
  else
    ID := KeyField.AsInteger;
  PostTreeData(ID, 0, SNew);
  DoNewItemNode;
end;

procedure TfrmVirtualDBTree.NewSubItemNode;
var
  ID: Integer;
begin
  ID := KeyField.AsInteger;
  PostTreeData(ID, 0, SNew);
  DoNewItemNode;
end;

procedure TfrmVirtualDBTree.NewRootFolderNode;
begin
  PostTreeData(0, 1, 'New folder');
  DoNewFolderNode;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

//*****************************************************************************

end.
