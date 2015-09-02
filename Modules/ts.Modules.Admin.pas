{ Author: Tim Sinaeve }

unit ts.Modules.Admin;

//*****************************************************************************

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  DB, DBClient, Provider, ADODB,

  ts.Modules.Data;

//=============================================================================

type
  TdmCustomAdmin = class(TdmCustomModule)
  protected
    function GetRowID : Variant; override;
    procedure AssignRecordCount; override;

  end;

//=============================================================================

type
  TAdminDataClass = class of TdmCustomAdmin;

//*****************************************************************************

implementation

{$R *.dfm}

uses
  ts.DBUtils;

//*****************************************************************************
// protected methods                                                     BEGIN
//*****************************************************************************

procedure TdmCustomAdmin.AssignRecordCount;
begin
  RecordCount := -1;
end;

//-----------------------------------------------------------------------------

function TdmCustomAdmin.GetRowID: Variant;
const
  QUERY = 'SELECT TOP 1 Fn_RowID() FROM %s';
begin
  //Result := QueryLookup(Connection.ADOConnection, QUERY, [FromClause]);
end;

//*****************************************************************************
// protected methods                                                       END
//*****************************************************************************

end.

