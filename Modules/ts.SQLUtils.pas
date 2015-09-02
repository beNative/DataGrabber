unit ts.SQLUtils;

{ Author: Tim Sinaeve }

//*****************************************************************************

interface

uses
  Classes, Variants;

//=============================================================================

function ParseSQLSelect(const ASQL     : string;
                              AFields  : TStrings;
                              AAliases : TStrings;
                          out ATable   : string
                       ): Boolean;

function CompareStrUpper(const AValue1, AValue2: string): Integer;

function DateTimeToSQLDate(ADateTime : TDateTime) : string;

function DateTimeToSQLTime(ADateTime : TDateTime) : string;

function DateTimeToSQLTimeStamp(ADateTime : TDateTime) : string;

function FloatToSQLFloat(AFloat : Extended) : string;

//*****************************************************************************

implementation

uses
  SysUtils;

const
  SQL_DATE      = '{ d ''%s'' }';
  SQL_TIME      = '{ t ''%s'' }';
  SQL_TIMESTAMP = '{ ts ''%s'' }';

//=============================================================================

//*****************************************************************************
// non-interfaced routines                                               BEGIN
//*****************************************************************************

function CompareStrUpper(const AValue1, AValue2: string): Integer; assembler;
asm
        PUSH    ESI;
        PUSH    EDI;
        PUSH    EBX;
        MOV     ESI,EAX;
        MOV     EDI,EDX;
        OR      EAX,EAX;
        JE      @@0;
        MOV     EAX,[EAX-4];
@@0:    OR      EDX,EDX;
        JE      @@1;
        MOV     EDX,[EDX-4];
@@1:    MOV     ECX,EAX;
        CMP     ECX,EDX;
        JBE     @@2;
        MOV     ECX,EDX;
@@2:    CMP     ECX,ECX;
@@3:    REPE    CMPSB;
        JE      @@6;
        MOV     BL,BYTE PTR [ESI-1];
        CMP     BL,'a';
        JB      @@4;
        CMP     BL,'z';
        JA      @@4;
        SUB     BL,20H;
@@4:    MOV     BH,BYTE PTR [EDI-1];
        CMP     BH,'a';
        JB      @@5;
        CMP     BH,'z';
        JA      @@5;
        SUB     BH,20H;
@@5:    CMP     BL,BH;
        JE      @@3;
        MOVZX   EAX,BL;
        MOVZX   EDX,BH;
@@6:    SUB     EAX,EDX;
        POP     EBX;
        POP     EDI;
        POP     ESI;
end;

//*****************************************************************************
// non-interfaced routines                                                 END
//*****************************************************************************

//*****************************************************************************
// interfaced routines                                                   BEGIN
//*****************************************************************************

{ Converts a TDateTime value to a ANSI SQL date representation. }

function DateTimeToSQLDate(ADateTime : TDateTime) : string;
begin
  Result := Format(SQL_DATE, [FormatDateTime('yyyy-mm-dd', ADateTime)]);
end;

//-----------------------------------------------------------------------------

{ Converts a TDateTime value to a ANSI SQL time representation. }

function DateTimeToSQLTime(ADateTime : TDateTime) : string;
begin
  Result := Format(SQL_TIME, [FormatDateTime('hh:nn:ss', ADateTime)]);
end;

//-----------------------------------------------------------------------------

{ Converts a TDateTime value to a ANSI SQL timestamp representation. }

function DateTimeToSQLTimeStamp(ADateTime : TDateTime) : string;
begin
  Result := Format(SQL_TIMESTAMP,
                   [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADateTime)]);
end;

//-----------------------------------------------------------------------------

{ Converts a float value to a ANSI SQL float representation. }

function FloatToSQLFloat(AFloat : Extended) : string;
var
  FS : TFormatSettings;
begin
  FS.ThousandSeparator := #0;
  FS.DecimalSeparator  := '.';
  Result := Format('%g', [AFloat], FS);
end;

//-----------------------------------------------------------------------------

{
TODO!!!
SELECT
  rfGroep->ID AS Groep,
  ID
FROM
  DfBkh.BasisRelaties

  this works BUT...

SELECT
  ID,
  rfGroep->ID AS Groep
FROM
  DfBkh.BasisRelaties

  does not!
}


{
  ParseSQLSelect

  Input
    ASQL : the SQL SELECT statement to parse

  Output
    AFields  : stringlist holing all selected fieldnames
    AAliases : stringlist holing all field aliases specified with
               <fieldname> AS <alias> in the SELECT clause of the query.
    ATable   : tablename where the selection of fields is applied to. Multiple
               tables in the FROM clause are not supported.

  Result
    True if the given SQL SELECT statement was successfully parsed.
}

{ TODO : if an alias is specified for the last field in the selection, it is not
         added to the list of aliases. }


function ParseSQLSelect(const ASQL: string; AFields, AAliases: TStrings;
out ATable :string): Boolean;
  var
    bIsFound: Boolean;
    I, N, iStart, iPos: Integer;
    S: string;
  {}
  function SkipSpace(const ASQL: string; var APosNum: Integer): Boolean;
    var
      I, iStart: Integer;
  begin
    iStart := APosNum;
    APosNum := Length(ASQL) + 1;
    for I := iStart to Length(ASQL) do
      if ASQL[I] > ' ' then
      begin
        APosNum := I;
        Break;
      end;
    Result := (APosNum <= Length(ASQL));
  end;
  {}
  function SkipStr(const ASQL, AValue: string; var APosNum: Integer): Boolean;
    var
      I: Integer;
  begin
    Result := False;
    if Length(ASQL) < APosNum + Length(AValue) then
      Exit;
    for I := 1 to Length(AValue) do
      if UpCase(ASQL[(APosNum - 1) + I]) <> AValue[I] then
        Exit;
    APosNum := APosNum + Length(AValue);
    if ASQL[APosNum] > ' ' then
      Exit;
    APosNum := APosNum + 1;
    Result := True;
  end;
  {}
  function SkipWord(const ASQL: string; var APosNum: Integer;
  out AValue: string; AIsForce: Boolean): Boolean;
    var
      I, iStart: Integer;
  begin
    iStart := APosNum;
    APosNum := Length(ASQL) + 1;
    for I := iStart to Length(ASQL) do
      if (ASQL[I] <= ' ') or (ASQL[I] = ',') then
      begin
        APosNum := I;
        Break;
      end;
    Result := (AIsForce) or (APosNum <= Length(ASQL));
    if Result then
      AValue := Copy(ASQL, iStart, APosNum - iStart)
    else
      AValue := '';
  end;
  {}
begin
  Result := False;
  ATable := '';
  iPos := 1;
  if not SkipSpace(ASQL, iPos) then
    Exit;
  if not SkipStr(ASQL, 'SELECT', iPos) then
    Exit;
  repeat
    if not SkipSpace(ASQL, iPos) then
      Exit;
    if ASQL[iPos] = '''' then
    begin
      iStart := iPos;
      iPos := Length(ASQL) + 1;
      N := 0;
      bIsFound := False;
      for I := iStart + 1 to Length(ASQL) do
        if bIsFound then
          if ASQL[I] = '''' then
          begin
            bIsFound := False;
            N := N + 1;
          end
          else if (ASQL[I] <= ' ') or (ASQL[I] = ',') then
          begin
            iPos := I;
            Break;
          end
          else
            Exit
        else
          if ASQL[I] = '''' then
            bIsFound := True
          else
            N := N + 1;
      if iPos > Length(ASQL) then
        Exit;
      SetLength(S, N);
      bIsFound := False;
      N := 0;
      for I := iStart + 1 to iPos - 2 do
      begin
        if ASQL[I] = '''' then
          bIsFound := (not bIsFound);
        if (ASQL[I] <> '''') or (not bIsFound) then
        begin
          N := N + 1;
          S[N] := ASQL[I];
        end;
      end;
      S := '';
    end
    else
    begin
      if not SkipWord(ASQL, iPos, S, False) then
        Exit;
      if CompareStrUpper(S, 'FROM') = 0 then
        Exit;
    end;
    if Assigned(AFields) then
      AFields.Add(S);
    if Assigned(AAliases) then
      AAliases.Add('');
    if not SkipSpace(ASQL, iPos) then
      Exit;
    if ASQL[iPos] <> ',' then
    begin
      if not SkipWord(ASQL, iPos, S, False) then
        Exit;
      if CompareStrUpper(S, 'FROM') = 0 then
      begin
        if ASQL[iPos] = ',' then
          Exit;
        iPos := iPos + 1;
        Break;
      end
      else if CompareStrUpper(S, 'AS') = 0 then
      begin
        if not SkipSpace(ASQL, iPos) then
          Exit;
        if ASQL[iPos] = ',' then
          Exit;
        if not SkipWord(ASQL, iPos, S, False) then
          Exit;
        if CompareStrUpper(S, 'FROM') = 0 then
          Exit;
      end;
      if Assigned(AAliases) then
        AAliases[AAliases.Count - 1] := S;
      if not SkipSpace(ASQL, iPos) then
        Exit;
      if ASQL[iPos] <> ',' then
        if not SkipStr(ASQL, 'FROM', iPos) then
          Exit;
    end;
    iPos := iPos + 1;
  until False;
  if not SkipSpace(ASQL, iPos) then
    Exit;
  if not SkipWord(ASQL, iPos, S, True) then
    Exit;
  Result := True;
  ATable := S;
end;

//*****************************************************************************
// interfaced routines                                                     END
//*****************************************************************************

end.
