{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit ts.Classes.Alignment;

interface

uses
  System.SysUtils, System.Classes;

type
  EtsAlignment = class(Exception);

type
  THorizontalAlignment = (
    haLeft,
    haRight,
    haCenter
  );
  TVerticalAlignment   = (
    vaTop,
    vaBottom,
    vaCenter
  );

type
  TtsAlignment = class(TPersistent)
  private
    FHorizontal : THorizontalAlignment;
    FVertical   : TVerticalAlignment;
    FWordWrap   : Boolean;
    FOnChange   : TNotifyEvent;

  protected
    procedure SetHorizontal(const Value: THorizontalAlignment); virtual;
    procedure SetVertical(const Value: TVerticalAlignment); virtual;
    procedure SetWordWrap(const Value: Boolean); virtual;

  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;

  published
    property Horizontal: THorizontalAlignment
      read FHorizontal write SetHorizontal default haCenter;

    property Vertical: TVerticalAlignment
      read FVertical write SetVertical default vaCenter;

    property WordWrap: Boolean
      read FWordWrap write SetWordWrap;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TtsAlignment.Create;
begin
  inherited;
  FHorizontal := haCenter;
  FVertical   := vaCenter;
  FWordWrap   := False;
end;
{$ENDREGION}

{$REGION 'property access methods'}
//---|Horizontal|--------------------------------------------------------------

procedure TtsAlignment.SetHorizontal(const Value: THorizontalAlignment);
begin
  if Value <> Horizontal then
  begin
    FHorizontal := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

//---|Vertical|----------------------------------------------------------------

procedure TtsAlignment.SetVertical(const Value: TVerticalAlignment);
begin
  if Value <> Vertical then
  begin
    FVertical := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

//---|WordWrap|----------------------------------------------------------------

procedure TtsAlignment.SetWordWrap(const Value: Boolean);
begin
  if Value <> WordWrap then
  begin
    FWordWrap := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TtsAlignment.Assign(Source: TPersistent);
var
  A: TtsAlignment;
begin
  if Source is TtsAlignment then
  begin
    A := TtsAlignment(Source);
    Horizontal := A.Horizontal;
    Vertical   := A.Vertical;
    WordWrap   := A.WordWrap;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}

end.

