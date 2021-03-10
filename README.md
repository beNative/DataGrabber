# DataGrabber

## Introduction

**DataGrabber** is a query tool that started years ago and is now being ported and extended to support Delphi 10 and later. 
It makes use of my **DDuce** library which holds some common modules that are shared by some of my other projects and is maturing along with this project.

**Connection topology:**
- FireDAC

**External dependencies and references:**
* [Spring4D](http://bitbucket.org/sglienke/spring4d)
* [DSharp](http://bitbucket.org/sglienke/dsharp)
* [DDuce](http://github.com/beNative/dduce)
* [Virtual treeview](http://github.com/Virtual-TreeView/Virtual-TreeView)
* [TzObjectInspector](http://github.com/MahdiSafsafi/zcontrols)
* [JsonDataObjects](http://github.com/ahausladen/JsonDataObjects)
* [Chinook database](http://github.com/lerocha/chinook-database)
* [SQLite](http://www.sqlite.org/)
* [SynEdit](http://github.com/SynEdit/SynEdit)
* [TChromeTabs](http://github.com/norgepaul/TChromeTabs)
* [KControls](http://bitbucket.org/tomkrysl/kcontrols)

**Commercial components (optional):**
- DevExpress cxGrid

## Main form

![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.png)

## Grouping

![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.Grouping.png)

## Data export

![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.Export.png)

```
+----------+--------+---------+-------------------+---------+----------+----------+---------------------------+
|EmployeeId|LastName|FirstName|Title              |ReportsTo|BirthDate |HireDate  |Address                    |
+----------+--------+---------+-------------------+---------+----------+----------+---------------------------+
|1         |Adams   |Andrew   |General Manager    |         |18/02/1962|14/08/2002|11120 Jasper Ave NW        |
|2         |Edwards |Nancy    |Sales Manager      |1        |8/12/1958 |1/05/2002 |825 8 Ave SW               |
|3         |Peacock |Jane     |Sales Support Agent|2        |29/08/1973|1/04/2002 |1111 6 Ave SW              |
|4         |Park    |Margaret |Sales Support Agent|2        |19/09/1947|3/05/2003 |683 10 Street SW           |
|5         |Johnson |Steve    |Sales Support Agent|2        |3/03/1965 |17/10/2003|7727B 41 Ave               |
|6         |Mitchell|Michael  |IT Manager         |1        |1/07/1973 |17/10/2003|5827 Bowness Road NW       |
|7         |King    |Robert   |IT Staff           |6        |29/05/1970|2/01/2004 |590 Columbia Boulevard West|
|8         |Callahan|Laura    |IT Staff           |6        |9/01/1968 |4/03/2004 |923 7 ST NW                |
+----------+--------+---------+-------------------+---------+----------+----------+---------------------------+
```

## Multiple resultsets
DataGrabber supports multi-resultset outputs, which can be displayed horizontally, vertically or as seperate tabs.

![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.MultipleResultSets1.png)
![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.MultipleResultSets2.png)
![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.MultipleResultSets3.png)

## Settings
Connection settings for multiple databases are configured in connection profiles.

![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.Settings.ConnectionProfiles.png)
![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.Settings.DisplaySettings.png)

Settings are stored in ``JSON`` format, and can also be changed by editing the file directly.
![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.Settings.SettingsFiles.png)

## Field inspector

![DataGrabber](https://github.com/beNative/DataGrabber/blob/master/Images/DataGrabber.FieldInspector.png)
