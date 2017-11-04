{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DataGrabber.RegisterServices;

interface

{$I DataGrabber.inc}

procedure RegisterServices;

implementation

uses
  System.SysUtils,
  Vcl.Forms,

  ts.Interfaces,
  {$IFDEF ADO}
  ts.Connection.ADOConnectionAdapter,
  {$ENDIF}

  {$IFDEF DBX}
  ts.Connection.DBXConnectionAdapter,
  {$ENDIF}

  {$IFDEF ZEOSDBO}
  ts.Connection.ZEOSConnectionAdapter,
  {$ENDIF}

  {$IFDEF UNIDAC}
  ts.Connection.UNIConnectionAdapter,
  {$ENDIF}

  {$IFDEF FIREDAC}
  ts.Connection.FireDACConnectionAdapter,
  {$ENDIF}

  DataGrabber.ConnectionViewManager, DataGrabber.Settings,
  DataGrabber.Interfaces, DataGrabber.EditorView,

  {$IFDEF DEVEXPRESS}
  DataGrabber.DataView.cxGrid,
  {$ENDIF}
  DataGrabber.DataView.GridView,
  {$IFDEF KGRID}
  DataGrabber.DataView.KGrid,
  {$ENDIF}
  DataGrabber.ConnectionView,

  Spring, Spring.Container.Common, Spring.Container;

type
  TRefCounting = Spring.Container.Common.TRefCounting;

const
  MIN_POOLSIZE = 1;
  MAX_POOLSIZE = 5;

procedure RegisterServices;
begin
  GlobalContainer.RegisterType<TDGSettings>
                 .Implements<IDGSettings>
                 .AsSingleton(TRefCounting.True);

  GlobalContainer.RegisterType<TdmConnectionViewManager>
                 .Implements<IConnectionViewManager>
                 .AsSingleton(TRefCounting.True);

  GlobalContainer.RegisterType<TfrmConnectionView>
                 .Implements<IConnectionView>;

  {$IFDEF ADO}
  GlobalContainer.RegisterType<TADOConnectionAdapter>
                 .Implements<IConnection>('ADO')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF ZEOSDBO}
  GlobalContainer.RegisterType<TZEOSConnectionAdapter>
                 .Implements<IConnection>('ZEOS')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF UNIDAC}
  GlobalContainer.RegisterType<TUNIConnectionAdapter>
                 .Implements<IConnection>('UNI')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF FIREDAC}
  GlobalContainer.RegisterType<TFireDACConnectionAdapter>
                 .Implements<IConnection>('FireDAC')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF DBX}
  GlobalContainer.RegisterType<TDBXConnectionAdapter>
                 .Implements<IConnection>('DBX')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF DEVEXPRESS}
  GlobalContainer.RegisterType<TfrmcxGrid>
                 .Implements<IDGDataView>('cxGrid');
  {$ENDIF}

  {$IFDEF KGRID}
  GlobalContainer.RegisterType<TfrmKGrid>
                 .Implements<IDGDataView>('KGrid');
  {$ENDIF}

  GlobalContainer.RegisterType<TfrmGridView>
                 .Implements<IDGDataView>('GridView');

  GlobalContainer.RegisterType<TfrmEditorView>
                 .Implements<IEditorView>;

  GlobalContainer.Build;
end;

end.
