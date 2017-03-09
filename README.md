# fishingMaps

Make maps of fishing effort (kw hours) and swept area ratio for all ICES Ecoregions. 

## Notes

* The scripts are designed to run on XYZ data. 
* To facilitate development, several datasets are included (t05, tf05, and tf10). NOTE: these data are randomly generated and have no reflection on actual fishing patterns.

## To run

Run the R script called run.R:
```
source("run.R")
```

You may want to run this file line by line however.  The process is as follows:

### Dowloading the data
1. The ICES ecoregion shapefiles are downloaded from the ICES GIS server
2. Landmass shapefiles are downloaded from openstreetmap
3. The example data is unzipped from the `ftp` folder

### Data setup 
1. ICES ecoregion is added to the example data
2. To save space the landmass shapefile is trimmed to the extent of the ICES Ecoregions

### plotting
A generic plotting function is use to:
1. subset the data for a particular area and year and (set of) year
2. a raster is made for a given resolution
3. this is plotted on top of a basemap showing the european landmass and the 
   relavent ICES ecoregion
4. A map of each year, and on averaged of years us made and saved to the `plots` folder
