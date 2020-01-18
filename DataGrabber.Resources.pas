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

unit DataGrabber.Resources;

{ Constants and resourcestrings used by the application. }

interface

const
  STableItem     = '\color{clBlue}table\column{}\color{clBlack}%s';
  SFieldItem     = '\color{clRed}field\column{}\color{clBlack}%s';
  SProcedureItem = '\color{clPurple}procedure\column{}\color{clBlack}%s';

  CHINOOK_EXAMPLE_QUERY =
    '/* Chinook database example query */'    + #13#10 +
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

  CHINOOK_EXAMPLE_QUERY1 =
    'select'                                  + #13#10 +
    '  al.*,'                                 + #13#10 +
    '  ar.*,'                                 + #13#10 +
    '  t.*,'                                  + #13#10 +
    '  mt.*,'                                 + #13#10 +
    '  g.*'                                   + #13#10 +
    'from'                                    + #13#10 +
    '  Album al'                              + #13#10 +
    '  inner join Artist ar'                  + #13#10 +
    '    on (al.ArtistId = ar.ArtistId)'      + #13#10 +
    '  inner join Track t'                    + #13#10 +
    '    on (t.AlbumId = al.AlbumId)'         + #13#10 +
    '  inner join MediaType mt'               + #13#10 +
    '    on (mt.MediaTypeId = t.MediaTypeId)' + #13#10 +
    '  inner join Genre g'                    + #13#10 +
    '    on (g.GenreId = t.GenreId)';

    CHINOOK_EXAMPLE_QUERY2 =
    'select '       + #13#10 +
    '  1 as A, '    + #13#10 +
    '  2 as B, '    + #13#10 +
    '  * '          + #13#10 +
    'from '         + #13#10 +
    '  Customer c ' + #13#10 +
    '; '            + #13#10 +
    'select '       + #13#10 +
    '  3 as A, '    + #13#10 +
    '  4 as B, '    + #13#10 +
    '  5 as C, '    + #13#10 +
    '  null as D, ' + #13#10 +
    '  null as E, ' + #13#10 +
    '  * '          + #13#10 +
    'from '         + #13#10 +
    '  Employee e ';

  SETTINGS_FILE = 'settings.json';

resourcestring
  SFetchingData             = 'Fetching data...';
  SUpdatingView             = 'Updating view...';
  SRecordCount              = '%d records';
  SFieldCount               = '%d fields';
  SConstantFieldCount       = '%d constant fields';
  SEmptyFieldCount          = '%d empty fields';
  SHiddenFieldCount         = '%d hidden fields';
  SUpdateable               = 'Live resultset';
  SReadOnly                 = 'Offline results';
  SConnected                = 'Connected';
  SDisconnected             = 'Disconnected';
  SConnectionFailed         = 'Connection failed! %s';
  SAskSaveChanges           = 'Do you want to save changes?';
  SConfirmDeleteProfile     = 'Are you sure you want to delete this profile?';
  SOpenSettingsFileLocation = 'Open settings file location (%s)';
  SConnectionProfileNameCannotBeEmpty =
    'The connection profile name cannot be empty!';
  SResultSet                = 'Resultset %d';

implementation

end.
