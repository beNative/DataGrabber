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

unit DataGrabber.FormSettings;

interface

uses
  System.Classes,
  Vcl.Forms;

type
  TFormSettings = class(TPersistent)
  private
    FWidth        : Integer;
    FHeight       : Integer;
    FLeft         : Integer;
    FTop          : Integer;
    FFormStyle    : TFormStyle;
    FVSplitterPos : Integer;
    FHSplitterPos : Integer;

  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

  published
    property Left: Integer
      read FLeft write FLeft;

    property Top: Integer
      read FTop write FTop;

    property Width: Integer
      read FWidth write FWidth;

    property Height: Integer
      read FHeight write FHeight;

    property FormStyle: TFormStyle
      read FFormStyle write FFormStyle;

    property VSplitterPos: Integer
      read FVSplitterPos write FVSplitterPos;

    property HSplitterPos: Integer
      read FHSplitterPos write FHSplitterPos;
  end;

implementation

{$REGION 'public methods'}
procedure TFormSettings.Assign(Source: TPersistent);
var
  Form : TForm;
begin
  if Source is TForm then
  begin
    Form      := TForm(Source);
    Left      := Form.Left;
    Top       := Form.Top;
    Width     := Form.Width;
    Height    := Form.Height;
    FormStyle := Form.FormStyle;
  end
  else
    inherited Assign(Source);
end;

procedure TFormSettings.AssignTo(Dest: TPersistent);
var
  Form : TForm;
begin
  if Dest is TForm then
  begin
    Form           := TForm(Dest);
    Form.Left      := Left;
    Form.Top       := Top;
    Form.Width     := Width;
    Form.Height    := Height;
    Form.FormStyle := FormStyle;
  end
  else
    inherited;
end;
{$ENDREGION}

initialization
  RegisterClass(TFormSettings);

end.
