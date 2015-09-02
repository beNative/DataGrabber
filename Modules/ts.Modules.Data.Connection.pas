unit ts.Modules.Data.Connection;

//*****************************************************************************

interface

uses
  ts.Modules.Interfaces;

//=============================================================================

function MainConnection : IConnection;

//*****************************************************************************

implementation

uses
  Forms,

  ts.Modules.Data.MainConnection;

//=============================================================================

var
  FMainConnection : TdmtsMainConnection;

//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

function MainConnection : IConnection;
begin
  if not Assigned(FMainConnection) then
    FMainConnection := TdmtsMainConnection.Create(Application);
  Result := FMainConnection;
end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************

end.

