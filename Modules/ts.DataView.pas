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

unit ts.DataView;

{ Abstract data view form with common implementation for interfaced dataviews. }

{
   TfrmDataView is intended to be overridden to support any data-aware grid
   that supports a TDataSource - link.
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Data.DB,

  ts.Interfaces;

type
  TfrmDataView = class(TForm, IDataView)
    dscMain: TDataSource;

    procedure dscMainStateChange(Sender: TObject);
    procedure dscMainDataChange(Sender: TObject; Field: TField);
    procedure dscMainUpdateData(Sender: TObject);

  private
    FConstantColumnsVisible : Boolean;
    FEmptyColumnsVisible    : Boolean;
    FData                   : IData;
    FSettings               : IDataViewSettings;
    function GetName: string;


  protected
    // property access methods
    function GetConstantColumnsVisible: Boolean;
    procedure SetConstantColumnsVisible(const Value: Boolean); virtual;
    function GetData: IData;
    procedure SetData(const Value: IData);
    function GetDataSet: TDataSet;
    function GetSettings: IDataViewSettings;
    procedure SetSettings(const Value: IDataViewSettings);
    function GetEmptyColumnsVisible: Boolean;
    procedure SetEmptyColumnsVisible(const Value: Boolean); virtual;

    // TDataSource events
    procedure StateChange; virtual; abstract;
    procedure UpdateData; virtual; abstract;
    procedure DataChange(AField: TField); virtual; abstract;

    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;

    procedure UpdateColumnLists; virtual; abstract;

    procedure ShowAllColumns; virtual;
    procedure AutoSizeColumns; virtual; abstract;

    { IDataView }
    procedure UpdateView;

  public
    procedure AfterConstruction; override;
    property DataSet: TDataSet
       read GetDataSet;

    property Data: IData
      read GetData write SetData;

    property Settings: IDataViewSettings
      read GetSettings write SetSettings;

    property ConstantColumnsVisible: Boolean
      read GetConstantColumnsVisible write SetConstantColumnsVisible;

    property EmptyColumnsVisible: Boolean
      read GetEmptyColumnsVisible write SetEmptyColumnsVisible;

    published
      property Name: string
        read GetName;
  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmDataView.AfterConstruction;
begin
  inherited;
  FConstantColumnsVisible := True;
  FEmptyColumnsVisible    := True;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmDataView.GetConstantColumnsVisible: Boolean;
begin
  Result := FConstantColumnsVisible;
end;

procedure TfrmDataView.SetConstantColumnsVisible(const Value: Boolean);
begin
  FConstantColumnsVisible := Value;
end;

function TfrmDataView.GetEmptyColumnsVisible: Boolean;
begin
  Result := FEmptyColumnsVisible;
end;

function TfrmDataView.GetName: string;
begin
  Result := inherited Name;
end;

procedure TfrmDataView.SetEmptyColumnsVisible(const Value: Boolean);
begin
  FEmptyColumnsVisible := Value;
end;

function TfrmDataView.GetSettings: IDataViewSettings;
begin
  Result := FSettings;
end;

procedure TfrmDataView.SetSettings(const Value: IDataViewSettings);
begin
  if Value <> FSettings then
  begin
    FSettings := Value;
  end;
end;

function TfrmDataView.GetData: IData;
begin
  Result := FData;
end;

procedure TfrmDataView.SetData(const Value: IData);
begin
  if Value <> Data then
  begin
    FData := Value;
    FData.RegisterDataView(Self);
    dscMain.DataSet := FData.DataSet;
    UpdateView;
  end;
end;

function TfrmDataView.GetDataSet: TDataSet;
begin
  if Assigned(FData) then
    Result := FData.DataSet
  else
    Result := nil;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmDataView.dscMainDataChange(Sender: TObject; Field: TField);
begin
  DataChange(Field);
end;

procedure TfrmDataView.dscMainStateChange(Sender: TObject);
begin
  StateChange;
end;

procedure TfrmDataView.dscMainUpdateData(Sender: TObject);
begin
  UpdateData;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmDataView.ShowAllColumns;
begin
  FEmptyColumnsVisible    := True;
  FConstantColumnsVisible := True;
end;

procedure TfrmDataView.UpdateView;
begin
//
end;
{$ENDREGION}

end.
