unit DataGrabber.RepositoryData;

//*****************************************************************************

interface

uses
  Classes, DB,

  ZConnection, ZDataset, ZAbstractRODataset, ZAbstractDataset,
  ZAbstractConnection;

//=============================================================================
type
  TdmRepositoryData = class(TDataModule)
    qryMain: TZQuery;
    conMain: TZConnection;

    procedure qryMainAfterOpen(DataSet: TDataSet);

  private
    function GetDataSet: TDataSet;

  public
    procedure Execute(const ASQL: string);

    property DataSet: TDataSet
      read GetDataSet;

    procedure BeforeDestruction; override;
  end;

//*****************************************************************************

implementation

uses
  Forms, SysUtils;

{$R *.dfm}

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TdmRepositoryData.BeforeDestruction;
begin
  //conMain.ExecuteDirect('Vacuum;');
  inherited BeforeDestruction;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TdmRepositoryData.GetDataSet: TDataSet;
begin
  Result := qryMain;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// event handlers                                                        BEGIN
//*****************************************************************************

procedure TdmRepositoryData.qryMainAfterOpen(DataSet: TDataSet);
begin
  if DataSet.IsEmpty then
  begin
     DataSet.Append;
     DataSet.FieldByName('ParentID').AsInteger := -1; // root has no parent
     DataSet.FieldByName('NodeType').AsInteger := 1; // folder
     DataSet.FieldByName('NodeName').AsString := 'root';
     //DataSet.FieldByName('DateCreated').AsDateTime := Now;
     //DataSet.FieldByName('DateModified').AsDateTime := Now;
     DataSet.Post;
   end;
end;

//*****************************************************************************
// event handlers                                                          END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

{ Execute SQL string }

procedure TdmRepositoryData.Execute(const ASQL: string);
begin
  DataSet.Active := False;
  qryMain.SQL.Text := ASQL;
  DataSet.Active := True;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

//*****************************************************************************

end.

