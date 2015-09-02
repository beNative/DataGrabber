unit ts.Data.NativeZEOS;

//*****************************************************************************

interface

uses
  DB,

  ZAbstractRODataset, ZAbstractDataset, ZDataset,

  ts.Interfaces, ts.Data.Native;

//=============================================================================

type
  TNativeZEOSDataSet = class(TNativeDataSet, INativeDataSet)
  private
    FDataSet: TZQuery;

  protected
    function GetDataSet: TDataSet; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

//*****************************************************************************

implementation

uses
  SysUtils,

  ZAbstractConnection;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TNativeZEOSDataSet.AfterConstruction;
begin
  inherited;
  FDataSet := TZQuery.Create(nil);
  FDataSet.Connection := Connection.Connection as TZAbstractConnection;
end;

//-----------------------------------------------------------------------------

procedure TNativeZEOSDataSet.BeforeDestruction;
begin
  FreeAndNil(FDataSet);
  inherited;
end;

//*****************************************************************************
// construction and destruction                                            END
//*****************************************************************************

//*****************************************************************************
// property access methods                                               BEGIN
//*****************************************************************************

function TNativeZEOSDataSet.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

end.
