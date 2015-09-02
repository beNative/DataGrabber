unit ts.Modules.List.View;

{
  Generic listform which interfaces through a IData-instance with a data module.
}

{
  REMARKS:
    This module is mainly based on the code in the FList unit of the Listings
    project and is intended to be more generic.

  TODO:
    - full IListSelection support.

}

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ActnList, Menus, ComCtrls, StdCtrls, ExtCtrls,

  DB,

  JvExStdCtrls, JvButton, JvCtrls,

  JclCounter,

  Ex_Grid, Ex_DBGrid,

  SBPro,

  ts.Components.DBGridViewSort,
  //tsFocusEnhancer, tsSpecialKeyHandler,tsCustomDBLookup,

  ts.Modules.Interfaces, ts.Modules.Form;

//=============================================================================

type
  TfrmCustomList = class(TfrmtsForm)
    actClear           : TAction;
    actDeleteRecord    : TAction;
    actExecute         : TAction;
    actExecuteShowSQL  : TAction;
    actInsertRecord    : TAction;
    actToggleGridFocus : TAction;
    aniBusy            : TAnimate;
    btnClose           : TJvImgBtn;
    dscMain            : TDataSource;
    grdMain            : TDBGridView;
    grpFooterLine      : TGroupBox;
    lblBusy            : TLabel;
    mniClose           : TMenuItem;
    mniExecute         : TMenuItem;
    mniN1              : TMenuItem;
    pnlBusy            : TPanel;
    pnlButtons         : TFlowPanel;
    pnlFooter          : TPanel;
    pnlGrid            : TPanel;
    pnlHeader          : TPanel;
    ppmMain            : TPopupMenu;

    procedure actToggleGridFocusExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actExecuteShowSQLExecute(Sender: TObject);

    procedure grdMainEnter(Sender: TObject);
    procedure grdMainEditButtonPress(Sender: TObject; Cell: TGridCell);
    procedure actInsertRecordExecute(Sender: TObject);
    procedure actDeleteRecordExecute(Sender: TObject);
    procedure grdMainKeyPress(Sender: TObject; var Key: Char);
    procedure grdMainChanging(Sender: TObject; var Cell: TGridCell;
      var Selected: Boolean);
    procedure grdMainCheckClick(Sender: TObject; Cell: TGridCell);
    procedure grdMainClearMultiSelect(Sender: TObject);
    procedure grdMainCellAcceptCursor(Sender: TObject; Cell: TGridCell;
      var Accept: Boolean);
    procedure grdMainEditCanModify(Sender: TObject; Cell: TGridCell;
      var CanModify: Boolean);
    procedure grdMainGetCellReadOnly(Sender: TObject; Cell: TGridCell;
      var CellReadOnly: Boolean);
    procedure grdMainGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure grdMainGetCellColors(Sender: TObject; Cell: TGridCell;
      Canvas: TCanvas);
    procedure grdMainRowMultiSelect(Sender: TObject; Row: Integer;
      var Select: Boolean);
    procedure grdMainClick(Sender: TObject);

  private
    FCounter      : TJclCounter;
    FFirstControl : TWinControl; // the initial value of the form's
                                 // ActiveControl property.
    FData         : IData;

  protected
    // protected property access methods
    function GetDataSet: TDataSet; virtual;

    function AutoAppend(AGridView : TDBGridView) : Boolean; virtual;

    procedure Initialize; override;
    function IsLargeList : Boolean; virtual;
    procedure Execute(AShowSQL : Boolean = False); virtual;

    procedure InitializeGridColumns;
    procedure InitializeGridColumn(AGridColumn: TDBGridColumn); virtual;

    procedure UpdateDisplayValues; virtual;

    // shortcut methods
    function IsLookupField(const AFieldName : string) : Boolean; virtual;
    function IsCheckBoxField(const AFieldName : string) : Boolean; virtual;
    function IsCellReadOnly(const ACell : TGridCell) : Boolean; virtual;

    procedure ShowBusyPanel(const AString : string);
    procedure HideBusyPanel;

    function ExecuteFieldLookup(      ALookupDialog : TtsCustomDBLookup;
                                      AEdit         : TCustomEdit;
                                const ADisplayField : string = '';
                                const AResultField  : string = 'ID';
                                      AUseEditText  : Boolean = False;
                                      ANullIfEmpty  : Boolean = False): Variant;

    function GridLookup(const AFieldName : string;
                              AEdit      : TCustomEdit;
                              ADataSet   : TDataSet) : Boolean; virtual;

    function ValidateListOperation : Boolean; virtual;

    procedure UpdateStatusBar; virtual;


  public
    constructor Create(AOwner: TComponent; AData : IData); reintroduce; virtual;
    destructor Destroy; override;

    property Data : IData
      read FData;

    property DataSet : TDataSet
      read GetDataSet;
  end;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  Math,

  ts.Utils, ts.Classes.KeyValues;

//=============================================================================

resourcestring
  SLargeListWarning =
    'De huidige lijst bevat %d records. Deze bewerking kan hierdoor' + #13#10 +
    'heel wat tijd in beslag nemen.' + #13#10#13#10 +
    'Wil U zeker hiermee doorgaan?';
  SFetchingRecords  = 'Bezig met ophalen en berekenen' + #13#10 +
                      'van lijstgegevens...' + #13#10 + #13#10 +
                      'Een ogenblik geduld a.u.b.';
  SSelectionIsLimited =
    'De selectie is veiligheidshalve beperkt tot slechts %d records';

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

constructor TfrmCustomList.Create(AOwner: TComponent; AData: IData);
begin
  inherited Create(AOwner);
  FCounter      := TJclCounter.Create;
  FData         := AData;
  Data.Executed := False;
end;

//-----------------------------------------------------------------------------

destructor TfrmCustomList.Destroy;
begin
  FCounter.Free;
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

//---|DataSet|-----------------------------------------------------------------

function TfrmCustomList.GetDataSet: TDataSet;
begin
  Result := Data.DataSet;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// action handlers                                                       BEGIN
//*****************************************************************************

procedure TfrmCustomList.actToggleGridFocusExecute(Sender: TObject);
begin
  inherited;
  if (not grdMain.Focused) and grdMain.Enabled then
    grdMain.SetFocus
  else if Assigned(FFirstControl) and FFirstControl.Enabled then
    FFirstControl.SetFocus;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.actDeleteRecordExecute(Sender: TObject);
begin
  inherited;
  if Assigned(grdMain.DataSource) and Assigned(grdMain.DataSource.DataSet)
  and (grdMain.DataSource.DataSet.Active) and (not grdMain.DataSource.DataSet.IsEmpty)
  then
    grdMain.DataSource.DataSet.Delete;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.actExecuteExecute(Sender: TObject);
begin
  inherited;
  Execute;
end;

//-----------------------------------------------------------------------------

{ Only for debugging! }

procedure TfrmCustomList.actExecuteShowSQLExecute(Sender: TObject);
begin
  inherited;
  Execute(True);
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.actInsertRecordExecute(Sender: TObject);
begin
  if Assigned(grdMain.DataSource) and Assigned(grdMain.DataSource.DataSet)
  and (grdMain.DataSource.DataSet.Active) and (not grdMain.DataSource.DataSet.IsEmpty)
  then
    grdMain.DataSource.DataSet.Insert;
end;

//*****************************************************************************
// action handlers                                                         END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

//---|grdMain|-----------------------------------------------------------------

procedure TfrmCustomList.grdMainCellAcceptCursor(Sender: TObject;
  Cell: TGridCell; var Accept: Boolean);
begin
  inherited;
  Accept := Accept and (not grdMain.Columns[Cell.Col].ReadOnly) and
    (not IsCellReadOnly(Cell));
end;

procedure TfrmCustomList.grdMainChanging(Sender: TObject; var Cell: TGridCell;
  var Selected: Boolean);
begin
  if Assigned(dscMain.DataSet) then
  begin
    if DataSet.State = dsEdit then
    begin
      DataSet.Post;
      grdMain.AutoSizeCols;
    end;
  end;
end;

//-----------------------------------------------------------------------------

{ Only fired when CheckBoxes = True }

procedure TfrmCustomList.grdMainCheckClick(Sender: TObject; Cell: TGridCell);
var
  S  : string;
  C  : TDBGridColumn;
  GV : TDBGridView;
begin
  inherited;
  GV := Sender as TDBGridView;
  C  := GV.Columns[Cell.Col];
  S  := C.FieldName;
  if C.AllowClick and IsCheckBoxField(S) then
  begin
    DataSet.Edit;
    if C.Field.IsNull then
       C.Field.Value := 1
    else
    begin
      if C.Field.Value = 0 then
        C.Field.Value := 1
      else
        C.Field.Value := 0
    end;
    if DataSet.State in dsEditModes then
      DataSet.Post;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainClearMultiSelect(Sender: TObject);
begin
  inherited;
  (Data as IListSelection).SelectedRecords.Clear;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainClick(Sender: TObject);
begin
  inherited;
  AutoAppend(Sender as TDBGridView);
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainEditButtonPress(Sender: TObject;
  Cell: TGridCell);
var
  GV : TDBGridView;
  S  : string;
begin
  inherited;
  GV := Sender as TDBGridView;
  S  := GV.SelectedField.FieldName;
  if GridLookup(S, nil, GV.DataSource.DataSet) then
    GV.AutoSizeCols;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainEditCanModify(Sender: TObject; Cell: TGridCell;
  var CanModify: Boolean);
begin
  CanModify := CanModify and not IsCellReadOnly(Cell);
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainEnter(Sender: TObject);
begin
  AutoAppend(Sender as TDBGridView);
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainGetCellColors(Sender: TObject; Cell: TGridCell;
  Canvas: TCanvas);
var
  GV : TDBGridView;
  D  : Double;
begin
  GV := Sender as TDBGridView;
  if not GV.IsCellHighlighted(Cell) and
     (GV.Columns[Cell.Col].Field is TNumericField) then
  begin
    D := GV.Columns[Cell.Col].Field.AsFloat;
    if IsZero(D) then
      Canvas.Font.Color := clBlue
    else if D < 0 then
      Canvas.Font.Color := clRed;
  end
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainGetCellReadOnly(Sender: TObject;
  Cell: TGridCell; var CellReadOnly: Boolean);
begin
  CellReadOnly := CellReadOnly and IsCellReadOnly(Cell);
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainGetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
var
  GV : TDBGridView;
begin
  inherited;
  GV := Sender as TDBGridView;
  if IsCheckBoxField(GV.Columns[Cell.Col].FieldName) then
    Value := '';
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainKeyPress(Sender: TObject; var Key: Char);
var
  GV   : TDBGridView;
  S    : string;
  Edit : TCustomEdit;
begin
  inherited;
  GV := Sender as TDBGridView;
  Edit := GV.Edit;
  S  := GV.SelectedField.FieldName;
  if (Ord(Key) = VK_RETURN) and GV.Editing and GV.Edit.Modified then
  begin
    if not GridLookup(S, Edit, GV.DataSource.DataSet) then
      Key := #0
    else
      grdMain.AutoSizeCols;
  end;

  if GV.Editing and (Key = '.') and (GV.SelectedField is TFloatField) then
    Key := ',';
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.grdMainRowMultiSelect(Sender: TObject; Row: Integer;
  var Select: Boolean);
var
  KV : TtsKeyValues;
  N  : Integer;
  LS : IListSelection;
begin
  LS := Data as IListSelection;
  KV := LS.SelectedRecords;
  if Select then
    KV[IntToStr(DataSet.RecNo)] := DataSet.Bookmark
  else
  begin
    N := KV.IndexOf(IntToStr(DataSet.RecNo));
    if N <> -1 then
      KV.Delete(N);
  end;

  if (grdMain.SelectedRows.Count > Pred(LS.MaxSelectionCount)) and Select then
  begin
    ShowWarning(SSelectionIsLimited, [LS.MaxSelectionCount]);
    Abort;
  end;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

function TfrmCustomList.AutoAppend(AGridView: TDBGridView): Boolean;
var
  DS : TDataSet;
begin
  Result := False;
  if Assigned(AGridView.DataSource) and
     Assigned(AGridView.DataSource.DataSet) and
     (AGridView.DataSource.DataSet.FieldCount > 0) and
     AGridView.AllowInsertRecord then
  begin
    DS := AGridView.DataSource.DataSet;
    if DS.Active and DS.IsEmpty then
    begin
      DS.Append;
      Result := True;
    end;
  end;
end;

//-----------------------------------------------------------------------------

{
  REMARK: This method is very similar to the one found in tsLookupUtils. The
          only difference is that this function sets the Executed property of
          the list form (or in fact the Executed property of the ListData
          instance).

  Centralizes lookup functionality. This method can do the following things:

    - Return for a given keyfield in the lookup table (AResultField) the
      selected value after lookup.
    - The lookupstring used for the lookup is provided by the inputtext of the
      given edit control (AEdit). Optionally a corresponding display value for
      the resultfield (e.g. a description or name) can be specified by passing
      the ADisplayField value. If a selection is made after lookup, the
      corresponding value of ADisplayField will be assigned to AEdit.Text.
    - The Modified flag of the edit control is always set to False after a call
      to this method. This enables the calling method to check if a lookup
      operation should be issued when the input text in the edit control is
      changed.

  -----------------------------------------------------------------------------

  ALookupDialog : Lookup component used for the lookup. This component is
                  connected to the lookup table which will be used to fetch
                  the values of AResultField and (optionally) ADisplayField.

  AEdit         : TEdit control that is used to:
                    (1) Enter the lookup string (if AUseEditText is True)
                    (2) Display the associated display value (ADisplayField) for
                        the given key field (AResultField).

  ADisplayField : Name of the field who's value will be used to display in the
    ['']          edit control. If the value is empty, no value is returned.

  AResultField  : Name of the field who's value will be returned by the
    ['ID']        function. Typically this field will represent the primary key
                  or (another unique key) in the lookup table.

  AUseEditText  : Determines if the current text in the Edit control should be
    [False]       used as the searchstring in the lookup dialog. If
                  AUseEditText = True and AEdit.Text = '' no lookup will be
                  performed. The value returned will be Unassigned, or Null
                  (if ANullIfEmpty is True).

  ANullIfEmpty  : If AEdit.Text is empty AND AUserEditText is True, Null is
    [False]       returned. If this value is False and AEdit.Text is empty,
                  Unassigned is returned.

  -----------------------------------------------------------------------------
  Examples:

  --------
  // Trigger a lookup if the value in a edit control is changed.
  procedure TMyForm.edtExit(Sender: TObject);
  var
    Edit : TEdit;
  begin
    Edit := nil;
    if Sender is TEdit then
      Edit := TEdit(Sender);

    if Assigned(Edit) and Edit.Modified then
    begin
      if Edit = edtBrand then
        Params['prmBrand'] :=
          ExecuteFieldLookup(dlgLookupBrand, Edit, 'Name', 'ID', True)
      else if Edit = ... then
        ...
    end;
  end;

  --------
  // Execute a lookup operation assigned to a TAction.
  procedure TMyForm.actLookupBrandExecute(Sender: TObject);
  begin
    Params['prmBrand'] := ExecuteFieldLookup(dlgLookupBrand, edtBrand, 'Name');
  end;

  --------
  // Execute a lookup operation from an input column in a TDBGridView
  procedure TMyForm.grdMainKeyPress(Sender: TObject; var Key: Char);
  var
    GV : TDBGridView;
    S  : string;
  begin
    GV := Sender as TDBGridView;
    S  := GV.SelectedField.FieldName;
    if (Ord(Key) = VK_RETURN) and GV.Editing and GV.Edit.Modified then
    begin
      if S = 'Brand' then
        DataSet.['BrandID'] :=
          ExecuteFieldLookup(dlgLookupBrand, GV.Edit, '', 'ID', True, True)
      else if S = ...
      ...
    end;
  end;

  -----------------------------------------------------------------------------
}

function TfrmCustomList.ExecuteFieldLookup(ALookupDialog: TtsCustomDBLookup;
  AEdit: TCustomEdit; const ADisplayField, AResultField: string; AUseEditText,
  ANullIfEmpty: Boolean): Variant;
var
  S : string;
begin
  if AUseEditText then
    S := AEdit.Text
  else
    S := '';

  if AUseEditText and (S = '') then
  begin
    if AEdit is TEdit then
      Data.Executed := False;
    if ANullIfEmpty then
      Result := Null
    else
      Result := Unassigned;
  end
  else
  begin
    if ALookupDialog.Execute(S) then
    begin
      if ADisplayField <> '' then
        AEdit.Text := ALookupDialog.AsString(ADisplayField);
      if AEdit is TEdit then
        Data.Executed := False;
      Result := ALookupDialog[AResultField];
    end
    else
    begin
     if AUseEditText then
      begin
        AEdit.SetFocus;
        AEdit.SelectAll;
      end;
      Result := Unassigned;
    end;
  end;
  AEdit.Modified := False;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.HideBusyPanel;
begin
  pnlBusy.Visible := False;
  aniBusy.Active  := False;
  Screen.Cursor   := crDefault;
  Application.ProcessMessages;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.InitializeGridColumns;
var
  I : Integer;
begin
  grdMain.Columns.BeginUpdate;
  for I := 0 to grdMain.Columns.Count - 1 do
    InitializeGridColumn(grdMain.Columns[I]);
  grdMain.Columns.EndUpdate;
end;

//-----------------------------------------------------------------------------

function TfrmCustomList.IsCheckBoxField(const AFieldName: string): Boolean;
begin
  Result := Data.IsCheckBoxField(AFieldName);
end;

//-----------------------------------------------------------------------------

{ Checks for large datasets. }

function TfrmCustomList.IsLargeList: Boolean;
var
  N : Integer;
begin
  if Data.MaxRecords = 0 then
    N := 10000
  else
    N := Data.MaxRecords div 3;

  Result := (DataSet.RecordCount < Data.RecordCount) and
            (Data.RecordCount > N);
end;

//-----------------------------------------------------------------------------

function TfrmCustomList.IsLookupField(const AFieldName: string): Boolean;
begin
  Result := Data.IsLookupField(AFieldName);
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.ShowBusyPanel(const AString: string);
begin
  Screen.Cursor   := crSQLWait;
  lblBusy.Caption := AString;
  pnlBusy.Left    := (Width div 2) - (pnlBusy.Width div 2);
  pnlBusy.Top     := (Height div 2) - (pnlBusy.Height div 2);
  pnlBusy.Visible := True;
  aniBusy.Active  := True;
  Application.ProcessMessages;
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

//*****************************************************************************
// protected virtual methods                                             BEGIN
//*****************************************************************************

procedure TfrmCustomList.Execute(AShowSQL: Boolean);
begin
  ShowBusyPanel(SFetchingRecords);
  try
    FCounter.Start;
    grdMain.SelectedRows.Clear;
    Data.Execute(AShowSQL);
    InitializeGridColumns;
    UpdateDisplayValues;
    grdMain.AutoSizeCols;
    grdMain.Enabled := Data.Executed and DataSet.Active;
    FCounter.Stop;
    UpdateStatusBar;
  finally
    HideBusyPanel;
  end;
end;

//-----------------------------------------------------------------------------

function TfrmCustomList.GridLookup(const AFieldName: string; AEdit: TCustomEdit;
  ADataSet: TDataSet): Boolean;
begin
  Result := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.Initialize;
begin
  inherited;
  gvsMain.DBGridView := grdMain;
  sbrMain.Panels[0].Indent := 5;
  sbrMain.Panels[1].Indent := 5;
  sbrMain.AutoHintPanelIndex := 4;
  sbrMain.AutoHint := True;
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.InitializeGridColumn(AGridColumn: TDBGridColumn);
var
  S : string;
begin
  S := AGridColumn.FieldName;
  if IsLookupField(S) then
  begin
    AGridColumn.EditStyle := geEllipsis;
  end
  else if IsCheckBoxField(S) then
  begin
    AGridColumn.CheckKind      := gcCheckBox;
    AGridColumn.AllowEdit      := False;
    AGridColumn.CheckAlignment := taCenter;
    AGridColumn.MaxWidth       := 100;
  end;
end;

//-----------------------------------------------------------------------------

function TfrmCustomList.IsCellReadOnly(const ACell: TGridCell): Boolean;
begin
  Result := False;
end;

//-----------------------------------------------------------------------------

{ Assign the DisplayValues to the corresponding form controls. }

procedure TfrmCustomList.UpdateDisplayValues;
begin
  //
end;

//-----------------------------------------------------------------------------

procedure TfrmCustomList.UpdateStatusBar;
var
//  N : Integer;
  P : TStatusPanelPro;
begin
  sbrMain.Panels[0].Text :=
    Format('%d / %d', [DataSet.RecNo, Data.RecordCount]);

  P := sbrMain.Panels[1]; // elapsed time
  if FCounter.Counting then
    P.Text := FormatElapsedTime(Int(FCounter.RunElapsedTime))
  else
    P.Text := FormatElapsedTime(FCounter.ElapsedTime);

//  if MultiSelect then
//  begin
//    P := sbrMain.Panels[3]; // multiselection status
//    N := grdMain.SelectedRows.Count;
//    if N > 0 then
//    begin
//      P.AutoWidth := True;
//      P.Indent    := 5;
//      if N = 1 then
//        P.Text := SRecordSelected
//      else
//        P.Text := Format(SRecordsSelected, [N]);
//    end
//    else
//    begin
//      P.AutoWidth := False;
//      P.Indent    := 0;
//      P.Width     := 0;
//    end;
//  end;
end;

//-----------------------------------------------------------------------------

{ Checks if the current operation has an impact on the performance (e.g.
  sorting large lists). }

function TfrmCustomList.ValidateListOperation: Boolean;
begin
  if IsLargeList then
    Result :=
      MessageBox(Handle,
      PChar(Format(SLargeListWarning, [Data.RecordCount])),
      PChar(Application.Title), MB_YESNO + MB_ICONWARNING + MB_DEFBUTTON2
      + MB_TOPMOST) = IDYES
  else
    Result := True;
end;

//*****************************************************************************
// protected virtual methods                                               END
//*****************************************************************************

end.




