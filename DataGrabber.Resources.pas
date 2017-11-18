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
//  STableItem     = '\color{clBlue}table\column{}\color{clBlack}%s';
//  SFieldItem     = '\color{clRed}field\column{}\color{clBlack}%s';
//  SProcedureItem = '\color{clPurple}procedure\column{}\color{clBlack}%s';

  EXAMPLE_QUERY =
    'select'                                  + #13#10 +
    '  *'                                     + #13#10 +
    'from'                                    + #13#10 +
    '  InvoiceLine il'                        + #13#10 +
    '  inner join Invoice i'                  + #13#10 +
    '    on (i.InvoiceId = il.InvoiceId)'     + #13#10 +
    '  inner join Customer c'                 + #13#10 +
    '    on (c.CustomerId = i.CustomerId)'    + #13#10 +
    '  inner join Employee e'                 + #13#10 +
    '    on (e.EmployeeId = c.SupportRepId)'  + #13#10 +
    '  inner join Track t'                    + #13#10 +
    '    on (il.TrackId = t.TrackId)'         + #13#10 +
    '  inner join Album al'                   + #13#10 +
    '    on (al.AlbumId = t.AlbumId)'         + #13#10 +
    '  inner join MediaType mt'               + #13#10 +
    '    on (mt.MediaTypeId = t.MediaTypeId)' + #13#10 +
    '  inner join Genre g'                    + #13#10 +
    '    on (g.GenreId = t.GenreId)';

  SETTINGS_FILE = 'settings.json';

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
  SConnected          = 'Connected';
  SDisconnected       = 'Disconnected';


implementation

end.
