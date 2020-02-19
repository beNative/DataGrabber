{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.FormSettings;

{ Persistable form settings. }

interface

uses
  System.Classes,
  Vcl.Forms,

  Spring;

type
  TFormSettings = class(TPersistent)
  private
    FOnChanged    : Event<TNotifyEvent>;
    FWidth        : Integer;
    FHeight       : Integer;
    FLeft         : Integer;
    FTop          : Integer;
    FFormStyle    : TFormStyle;
    FVSplitterPos : Integer;
    FHSplitterPos : Integer;
    FWindowState  : TWindowState;

    {$REGION 'property access methods'}
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetOnChanged: IEvent<TNotifyEvent>;
    procedure SetHSplitterPos(const Value: Integer);
    procedure SetVSplitterPos(const Value: Integer);
    procedure SetWindowState(const Value: TWindowState);
    {$ENDREGION}

  protected
    procedure DoChanged;

  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Left: Integer
      read FLeft write SetLeft;

    property Top: Integer
      read FTop write SetTop;

    property Width: Integer
      read FWidth write SetWidth;

    property Height: Integer
      read FHeight write SetHeight;

    property FormStyle: TFormStyle
      read FFormStyle write SetFormStyle;

    property VSplitterPos: Integer
      read FVSplitterPos write SetVSplitterPos;

    property HSplitterPos: Integer
      read FHSplitterPos write SetHSplitterPos;

    property WindowState: TWindowState
      read FWindowState write SetWindowState;
  end;

implementation

{$REGION 'property access methods'}
function TFormSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

procedure TFormSettings.SetFormStyle(const Value: TFormStyle);
begin
 if FormStyle <> Value then
  begin
    FFormStyle := Value;
    DoChanged;
  end;
end;

procedure TFormSettings.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    FHeight := Value;
    DoChanged;
  end;
end;

procedure TFormSettings.SetHSplitterPos(const Value: Integer);
begin
  if Value <> HSplitterPos then
  begin
    FHSplitterPos := Value;
    DoChanged;
  end;
end;

procedure TFormSettings.SetLeft(const Value: Integer);
begin
  if Left <> Value then
  begin
    FLeft := Value;
    DoChanged
  end;
end;

procedure TFormSettings.SetTop(const Value: Integer);
begin
  if Top <> Value then
  begin
    FTop := Value;
    DoChanged;
  end;
end;

procedure TFormSettings.SetVSplitterPos(const Value: Integer);
begin
  if Value <> VSplitterPos then
  begin
    FVSplitterPos := Value;
    DoChanged;
  end;
end;

procedure TFormSettings.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    FWidth := Value;
    DoChanged;
  end;
end;

procedure TFormSettings.SetWindowState(const Value: TWindowState);
begin
  if (Value <> WindowState) and (Value <> wsMinimized) then
  begin
    FWindowState := Value;
    DoChanged;
  end;
end;

{$ENDREGION}

{$REGION 'protected methods'}
procedure TFormSettings.DoChanged;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TFormSettings.Assign(Source: TPersistent);
var
  LForm : TForm;
begin
  if Source is TForm then
  begin
    LForm        := TForm(Source);
    Left        := LForm.Left;
    Top         := LForm.Top;
    Width       := LForm.Width;
    Height      := LForm.Height;
    FormStyle   := LForm.FormStyle;
    WindowState := LForm.WindowState;
  end
  else
    inherited Assign(Source);
end;

procedure TFormSettings.AssignTo(Dest: TPersistent);
var
  LForm : TForm;
begin
  if Dest is TForm then
  begin
    LForm             := TForm(Dest);
    LForm.Left        := Left;
    LForm.Top         := Top;
    LForm.Width       := Width;
    LForm.Height      := Height;
    LForm.FormStyle   := FormStyle;
    LForm.WindowState := WindowState;
  end
  else
    inherited AssignTo(Dest);
end;
{$ENDREGION}

initialization
  RegisterClass(TFormSettings);

end.
