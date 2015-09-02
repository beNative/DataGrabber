unit ts.Data.NativeUNI;

//*****************************************************************************

interface

uses
  DB,

  MemDS, DBAccess, Uni,

  ts.Interfaces, ts.Data.Native;

//=============================================================================

type
  TNativeUNIDataSet = class(TNativeDataSet, INativeDataSet)
  private
    FDataSet: TUniQuery;

  protected
    function GetDataSet: TDataSet; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateProviderMode(var AEnabled: Boolean);

  end;

//*****************************************************************************

implementation

uses
  SysUtils;

//*****************************************************************************
// construction and destruction                                          BEGIN
//*****************************************************************************

procedure TNativeUNIDataSet.AfterConstruction;
begin
  inherited;
  FDataSet := TUniQuery.Create(nil);
  FDataSet.Connection := Connection.Connection as TUniConnection;
end;

//-----------------------------------------------------------------------------

procedure TNativeUNIDataSet.BeforeDestruction;
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

function TNativeUNIDataSet.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

//*****************************************************************************
// property access methods                                                 END
//*****************************************************************************

//*****************************************************************************
// public methods                                                        BEGIN
//*****************************************************************************

procedure TNativeUNIDataSet.UpdateProviderMode(var AEnabled: Boolean);
begin
  FDataSet.UniDirectional := AEnabled;
end;

//*****************************************************************************
// public methods                                                          END
//*****************************************************************************

end.
