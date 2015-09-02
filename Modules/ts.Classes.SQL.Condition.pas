{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit ts.Classes.SQL.Condition;

interface

uses
  System.SysUtils, System.Classes,

  ts.Classes.SQL.Params;

type
  ESQLCondition = class(Exception);

type
  TCustomSQLCondition = class(TPersistent)
  private
    FEnabled         : Boolean;
    FConditionString : string;
    FOnChanged       : TNotifyEvent;
    FParamIndex      : Integer;
    FParams          : TSQLParams;
    FQuoteValues     : Boolean;
    FUpdateLock      : Integer;

    procedure OnParamsChanged(Sender: TObject);

  protected
    // protected methods
    function GetConditionString: string; virtual;
    procedure SetConditionString(const Value : string); virtual;
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetQuoteValues(const Value: Boolean);

    procedure Changed; virtual;
    procedure UpdateConditionString; virtual;

    property Params : TSQLParams
      read FParams write FParams;

    property ParamIndex : Integer
      read FParamIndex write FParamIndex;

  public
    // construction and destruction
    constructor Create(ASQLParams : TSQLParams); overload; virtual;
    constructor Create; overload;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    procedure Update; virtual;

    // public properties
    { If Active = True this property will hold the generated condition expression
      for the current condition.  }
    property ConditionString : string
      read GetConditionString write SetConditionString;

    { Determines if expression values should be quoted. For expressions that
      require quotes this setting will be ignored. }
    property QuoteValues : Boolean
      read FQuoteValues write SetQuoteValues default True;

    { When Enabled = True then for this condition a ConditionString will be
      generated. If Enabled = False then ConditionString will return an empty
      string.  }
    property Enabled: Boolean
      read GetEnabled write SetEnabled default True;

    property OnChanged : TNotifyEvent
      read FOnChanged write FOnChanged;
  end;

  TSQLCondition = class(TCustomSQLCondition)
  published
    property ConditionString;
    property Enabled;
    property QuoteValues;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TCustomSQLCondition.Create(ASQLParams : TSQLParams);
begin
  inherited Create;
  BeginUpdate;
  FUpdateLock       := 0;
  FEnabled          := True;
  FParams           := ASQLParams;
  FQuoteValues      := True;
  if Assigned(FParams) then
    FParams.OnChanged := OnParamsChanged;
  FParamIndex       := -1;
  EndUpdate;
end;

constructor TCustomSQLCondition.Create;
begin
  Create(nil);
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|ConditionString|---------------------------------------------------------

function TCustomSQLCondition.GetConditionString: string;
begin
  if Enabled then
    UpdateConditionString
  else
    FConditionString := '';
  Result := FConditionString;
end;

procedure TCustomSQLCondition.SetConditionString(const Value: string);
begin
  FConditionString := Value;
end;

//---|Enabled|-----------------------------------------------------------------

function TCustomSQLCondition.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TCustomSQLCondition.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;
//---|QuoteValues|-------------------------------------------------------------

procedure TCustomSQLCondition.SetQuoteValues(const Value: Boolean);
begin
  FQuoteValues := Value;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TCustomSQLCondition.OnParamsChanged(Sender: TObject);
begin
  Changed;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TCustomSQLCondition.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomSQLCondition.EndUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then
    Update;
end;

procedure TCustomSQLCondition.Changed;
begin
  if FUpdateLock = 0 then
  begin
    UpdateConditionString;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

{ In descendant classes this method holds the code to construct the condition
  expression using the condition specific properties. }

procedure TCustomSQLCondition.UpdateConditionString;
begin
// needs to be overridden in descendants
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TCustomSQLCondition.Update;
begin
  UpdateConditionString;
end;
{$ENDREGION}

end.
