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

  // TODO: use factories instead of DI ?

procedure RegisterServices;
begin
  GlobalContainer.RegisterType<TSettings>
                 .Implements<ISettings>
                 .AsSingleton(TRefCounting.True);

  GlobalContainer.RegisterType<TdmConnectionViewManager>
                 .Implements<IConnectionViewManager>
                 .AsSingleton(TRefCounting.True);

  GlobalContainer.RegisterType<TfrmConnectionView>
                 .Implements<IConnectionView>;

  {$IFDEF DEVEXPRESS}
  GlobalContainer.RegisterType<TfrmcxGrid>
                 .Implements<IDataView>('cxGrid');
  {$ENDIF}

  {$IFDEF KGRID}
  GlobalContainer.RegisterType<TfrmKGrid>
                 .Implements<IDataView>('KGrid');
  {$ENDIF}

  GlobalContainer.RegisterType<TfrmGridView>
                 .Implements<IDataView>('GridView');

  GlobalContainer.RegisterType<TfrmEditorView>
                 .Implements<IEditorView>;

  GlobalContainer.Build;
end;

end.
