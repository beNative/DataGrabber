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

unit DataGrabber.RegisterServices;

interface

{$I 'DataGrabber.inc'}

procedure RegisterServices;

implementation

uses
  System.SysUtils,
  Vcl.Forms,

  ts.Interfaces,
  {$IFDEF ADO}
  ts.Connection.ADOConnectionAdaptor,
  {$ENDIF}

  {$IFDEF DBX}
  ts.Connection.DBXConnectionAdaptor,
  {$ENDIF}

  {$IFDEF ZEOSDBO}
    ts.Connection.ZEOSConnectionAdaptor,
  {$ENDIF}

  {$IFDEF UNIDAC}
    ts.Connection.UNIConnectionAdaptor,
  {$ENDIF}

  {$IFDEF FIREDAC}
    ts.Connection.FireDACConnectionAdaptor,
  {$ENDIF}

  DataGrabber.ConnectionViewManager, DataGrabber.Settings,
  DataGrabber.Interfaces, DataGrabber.EditorView,

  {$IFDEF DEVEXPRESS}
  DataGrabber.DataView.cxGrid,
  {$ENDIF}
  DataGrabber.DataView.GridView,
  DataGrabber.DataView.VirtualDBGrid,
  {$IFDEF KGRID}
  DataGrabber.KGrid,
  {$ENDIF}
  DataGrabber.ConnectionView,

  Spring, Spring.Container.Common, Spring.Container, Spring.Services;

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
  GlobalContainer.RegisterType<TADOConnectionAdaptor>
                 .Implements<IConnection>('ADO')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF ZEOSDBO}
  GlobalContainer.RegisterType<TZEOSConnectionAdaptor>
                 .Implements<IConnection>('ZEOS')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF UNIDAC}
  GlobalContainer.RegisterType<TUNIConnectionAdaptor>
                 .Implements<IConnection>('UNI')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF FIREDAC}
  GlobalContainer.RegisterType<TFireDACConnectionAdaptor>
                 .Implements<IConnection>('FireDAC')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF DBX}
  GlobalContainer.RegisterType<TDBXConnectionAdaptor>
                 .Implements<IConnection>('DBX')
                 .AsSingleton(TRefCounting.True)
                 .AsPooled(MIN_POOLSIZE, MAX_POOLSIZE);
  {$ENDIF}

  {$IFDEF DEVEXPRESS}
  GlobalContainer.RegisterType<TfrmcxGrid>
                 .Implements<IDGDataView>('cxGrid');
  {$ENDIF}

  GlobalContainer.RegisterType<TfrmGridView>
                 .Implements<IDGDataView>('GridView');

  GlobalContainer.RegisterType<TfrmVirtualDBGrid>
                 .Implements<IDGDataView>('VirtualDBGrid');

  GlobalContainer.RegisterType<TfrmEditorView>
                 .Implements<IEditorView>;

  GlobalContainer.Build;
end;

end.
