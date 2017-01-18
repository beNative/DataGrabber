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

unit DataGrabber.Resources;

interface

const
  STableItem     = '\color{clBlue}table\column{}\color{clBlack}%s';
  SFieldItem     = '\color{clRed}field\column{}\color{clBlack}%s';
  SProcedureItem = '\color{clPurple}procedure\column{}\color{clBlack}%s';

resourcestring
  SFetchingData       = 'Fetching data...';
  SUpdatingView       = 'Updating view...';
  SReady              = 'Ready';
  SRecordCount        = '%d records';
  SFieldCount         = '%d fields';
  SConstantFieldCount = '%d constant fields';
  SEmptyFieldCount    = '%d empty fields';
  SUpdateable         = 'Updateable';
  SReadOnly           = 'ReadOnly';
  SProviderMode       = 'Provider mode';
  SNativeMode         = 'Native mode';
  SConnected          = 'Connected';
  SDisconnected       = 'Disconnected';

implementation

end.
