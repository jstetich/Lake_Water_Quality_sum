# Data Preparation

## Geospatial Data on Casco Bay Lakes
Geospatial data on Casco Bay lakes and ponds with associated MIDAS numbers is 
provided in the  [Maine_lakes_MIDAS](https://github.com/ccb60/Maine_Lakes_MIDAS)
GitHub repository.  Details of preparing the geospatial data is also
provided there.

In brief, we extracted MIDAS numbers and geospatial data on Maine lake centroids
from a KML file made available by Maine DEP.  We selected centroids located in
the Casco Bay watershed, and matched centroids with polygon data from the
National Hydrography Database. We conducted careful QA/QC to identify missing or
duplicated lake polygons.

### Generating a CSV File
For our analysis here, we needed to work with a list of MIDAS numbers for lakes 
from the Casco Bay watershed.  We created a CSV file by exporting data from
the GIS file just described. We exported the attribute table of "CB_Lakes" (a
shapefile) to "CB_Lakes.csv" (in the 'Supplementary Lake Data' folder). We then
edited that file (by hand) to remove the FID column, and revised the column
names as follows:

Column Name     | Contents                                      
----------------|----------------------------------------------
ExtName         | Lake Name, combined with MIDAS and Town info      
Longitude       | Longitude, apparently WGS 1984
Latitude        | Latitude, apparently WGS 1984
Name            | Lake Name
Town            | A single Town name, even for large lakes
MIDAS Number    | The lakes' unique MIDAS number
Elevation       | Elevation, in feet
Notes           | If present, usually a comment about lake names
Name Change     | Additional; info on name changes
----------------|----------------------------------------------

We deleted records with duplicate MIDAS numbers (e.g., for Sebago Lake),
selecting The point that corresponds to the largest contiguous pond area and
deleting others.  Where relevant, we edited the Notes field to indicate what we
did.

## Sebago Lake Sampling Locations
We used ArcGIS to transfer positions of sampling Stations from DEP and PWD maps
to GIS, using standard methods for georeferencing images. Once approximate
Station positions were transferred to ArcGIS, we calculated a distance matrix,
in meters and kilometers, describing distances among all sampling locations.  We
reordered that matrix in Excel to identify sampling Regions, where Stations are
all within five kilometers of each other. Distance matrix and assignment
to regions are available here via an excel file:

'Sample Point Distances and Regions.xlsx'.


## Water Quality Data
The downloaded lakes data was originally in six data files, with the following
content:  
*  Chlorophyll
*  Geography and Morphometry
*  pH, Color, Conductivity and alkalinity
*  Phosphorus
*  Secchi Depths
*  Temperature and Dissolved Oxygen profiles

But these data can be reorganized in terms of the temporal and 
spatial extents.  We have essentially three types of data:
1.  Lake identification, location, and Morphometry, which is lake specific and
    time independent.
2.  Secchi depth, which is station specific (within lakes) and date dependent.
    (Raw data includes a TIME field, principally for QA purposes).
3.  All other parameters, which are lake, station, depth, and date/time 
    dependent.
    
We chose to keep dissolved oxygen and temperature data, which was in the
form of vertical profiles, separate from other "grab" sample data that also
depend on location, depth and time. This mirrors the organization of the
source data.

We reorganized the water quality data into four CSV files, as follows:
*  Morphometry and Geography data:  **Lake_Morphometry_Metric.csv**
*  Secchi depth data:  **Secchi.csv** and **Secchi_Sebago.csv**
*  Temperature and Dissolved oxygen: **Supplementary Lake Data/Temp_DO.csv**
*  All Other Observations: *  **Supplementary Lake Data/Sample_Data.csv**

We did not use the last two files in State of Casco Bay, in large measure
because of the difficulty of summarizing multiple parameters with uneven 
sampling histories for a lay audience at basin-wide scale.  

We provide those files in this repository for completeness, but store them 
in the "Supplementary Lakes Data Folder". Those files include data from lakes
statewide.

### File Contents
*  **Lake_Morphometry_Metric.csv**
Data on the morphology of Maine Lakes, drawn from   [here](http://www.gulfofmaine.org/kb/files/9680/MaineLakes_Geography_Morphometry.xls),
converted to metric units, with names somewhat simplified for use in R.
Additional metadata available with the source file.

Column Name     | Contents                                      | Units / Values 
----------------|-----------------------------------------------|-------------- 
MIDAS	          | Unique numerical indicator for Maine Lakes    | Integer 
Lake	          | name of Lake (NOT consistent across data files!) | 
Towns	          | Town or towns adjacent to the lake            | 
Flushes_p_yr	 | Flushing rate                                 | Float
Trop_Cat        | Trophic Category	                            | 'OLIGO', 'MESO', 'EUTRO'
Dam	          | How much does a dam nfluence lake size        | 1 = no dam, 2 = >50%, 3 < 50%
Major_Drainage	 | Name of the major drainage in which the lake occurs | 
Sub_Drainage	 | Name of the sub-drainage in which the lake occurs | 
HUC10_Name      | Name of the HUC-10 'watershed'                | 
HUC10_Code      | Code of the HUC-10 'watershed'                | 
USGS_Quad24	    | USGS topographic map on which the lake appears| 
County          | County in which the lake occurs               | 
UTM_X	          | UTM Easting zone 19 N, NAD 83 (approximate lake center)  | 
UTM_Y	          | UTM Northing zone 19 N, NAD 83 (approximate lake center) | 
Latitude	       | Latitude.  No geioid specified. NAD 1983 / WGS 1984 ?    | 
Longitude		 | Longitude.  No geioid specified. NAD 1983 / WGS 1984 ?    | 
WQ_Statement	 |                                               | Text
Invasives	    | Invasive aquatic plants. Ca 2011?             | 
Fishery         | Fishery management type from IF&W.            | 
Area_sq_m	    | Area, in square )                             | meters (Converted from acres 
Perim_km	       | Perimeter in                                  | kilometers(from miles)  
D_Mean_m	       | Lake mean depth                               | meters (from feet) 
D_Max_m	       | Lake maximum depth in                         |  meters (from feet) 
Volume_m3	    | Lake Volume in cubic                          | meters (from acre-feet)  
DDrain_ha	    | Direct drainage area (omits upstream lake watersheds) | Hectares ( from square miles) 
TDrain_ha	    | Total drainage area in Hectares (from square miles) | Hectares ( from square miles) 
Elev_m          | Lake surface elevation                        | meters (from feet)
----------------|-----------------------------------------------|--------------

*  **Secchi.csv**
Secchi depth data FOR Casco Bay Lakes only, drawn as a subset of the statewide 
data.

Column Name  | Contents                     | Units / Values 
-------------|------------------------------|-------------- 
MIDAS        | MIDAS lake code              | 
Lake         | Lake name                    | 
Town         | Town(s) in which the lake occurs  | 
Date         | Sampling date                | 
Station      | Sampling station             | 
Time         | Sampling time                | 
Secchi Depth | Secchi depth (meters)        | 
Secchi on Bottom? | 	Indicates whether Secchi was on the lake bottom |  Yes (B or Y), or No (N) 
Scope	       | Viewing scope type |1=none; 2=plain; 3=slant glass; 4=slant glass with mask; 5=flat glass with mask. 
Wind Level   | Wind velocity estimate (miles per hour)               | 
Wind Direction | Wind direction estimate    | 1=N; 2=NE; 3=E; 4=SE; 5=S; 6=SW; 7=W; 8=NW 
Cloud cover  |                              | B=clear; C=cloudy/bright; O=heavy overcast. 
-------------|------------------------------|-------------- 

Note the annotation about whether the Secchi disk was on the bottom of the lake.
Secchi data is "right censored", as it is impossible to measure a Secchi depth
greater than the water depth.  This scenario was fairly common at some sampling 
locations, and may need special handling with resistant statistics or other
methods.

*  **Secchi_Sebago.csv**
Additional data on secchi depth derived from data e-mailed to CBEP by staff at 
the Portland Water District (PWD).  PWD data apparently was not incorporated
into the DEP Lakes data files.

Column Name  | Contents                     | Units / Values 
-------------|------------------------------|-------------- 
MIDAS        | 5786                         |
Lake         | Sebago Lake,"Gray, Windham   |
Town         | "Gray, Windham"              |
StationName  | Station name.                | 
Date         | Sample date as datetime with time = 0 | %Y-%m-%dT$H:%M:%SZ"
Year         | Year of sample )from Date)   |  
Secchi_Depth | Observed Secchi depth        |
Station      | Station ID number            | 101 through 107
-------------|------------------------------|-------------- 

Note, this file does not include any indicator for the Secchi Disk sitting on 
the bottom.  Sebago lake is very deep, and that's not an issue at any of their 
sampling locations.

*  **Supplementary Lake Data/Temp_DO.csv**
This is a CSV copy of the data from 
[here](http://www.gulfofmaine.org/kb/record.html?recordid=9214).

Column Name  | Contents                     | Units / Values 
-------------|------------------------------|-------------- 
MIDAS        | MIDAS lake code              | 
Lake         | Lake name                    | 
Town         | Town(s) in which the lake occurs  | 
Station      | Sampling station             | 
Date         | Sample date as datetime with time = 0 | %Y-%m-%dT$H:%M:%SZ" 
Depth        | Depth of data collection     | meters 
Temperature  | Water temperature at depth   | Celsius
Oxygen       | Dissolved oxygen concentration at depth (mg/l or ppm) | 
Oxygen_Method| Indication of methods        | No dictionary available
-------------|------------------------------|-------------- 

*  **Supplementary Lake Data/Sample_Data.csv**
This is a large file, with data from all Maine Lakes, that also contains a large
number of data types. The data were  derived from a data files available at the
following location:
[Transparency, color, chemistry](http://www.gulfofmaine.org/kb/2.0/record.html?recordid=9679)

To make data storage more efficient, we converted the data to "long" 
form. That is, each row represents one measurement, not one sample.


Column Name  | Contents                     | Units / Values 
-------------|------------------------------|-------------- 
MIDAS        | MIDAS lake code              | 
Lake         | Lake name                    | 
Town         | Town(s) in which the lake occurs  | 
Station      | Sampling station             | 
Date         | Sample date as datetime with time = 0 | %Y-%m-%dT$H:%M:%SZ" 
Depth        | Sample depth                 | meters
Type         | Was sample a "grab" sample or "core" sample? | "G", "C"
Color_Method | Method used to measure color |  "C"  "N"  "H"  "S"  "H2"
Parameter    | What parameter?              |"pH", "Conductivity", "Alkalinity", "Color"       
Value        | observation                  | numerical value
Method       | Other method indicators      | See below
Units        | Reported units for the observation | "uS", "mg/l", "SPU" 
-------------|------------------------------|-------------- 

**Color_Method* is not fully documented in our source data, but they correspond 
to the following:
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"C" = colorimetric

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"H" = Hach

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"N" = Nessler

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"H2"= Not documented, but probably refers to a different Hach method.

**Methods** collapsed methods and other indicators for several parameters into 
one data column.  Many indicators were not fully documented in our source data.
Organized by parameter, they are: 

_Alkalinity_:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"C" = Colorimetric

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"M" = Methyl orange

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"G" = Gran Plot (with unit conversion)

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"H" = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"B" = Bromcresol Green / methyl red 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"B-LAMOTTE 4491" = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"O"              = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"[2320 B]"       = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"2320B"          = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"GRANPLOT"       = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"Bromocresol Green/Met"  = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"L"              = Not defined in the source metadata


_Conductivity_:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"E"        = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"L"        = Electronic lab meter

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"F"        = Electronic field meter

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"LEA"      = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"YSI PRO"  = Not defined in the source metadata

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"Lab meter", "LAB METER" and "Lab Meter"

_Color_ 
We pulled out Color Methods separately, but then collapsed a qualifier of the
type of color metric used into our general "Methods" category.  That reflected
our initial uncertainty about the meaning of some of the coded data columns, in
part due to incomplete metadata in our source data. We never bothered to fix it,
since we did not use the color data directly in SoCB.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"T" = True color

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"A" = Apparent Color


**Other state-wide data** is available online:

[Chlorophyll Data](http://www.gulfofmaine.org/kb/2.0/record.html?recordid=9211)

[Phosphorus](http://www.gulfofmaine.org/kb/2.0/record.html?recordid=9212)


## A Caution: 
Names of lakes are not entirely consistent across these data sets.

