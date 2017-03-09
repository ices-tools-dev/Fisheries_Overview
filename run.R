# boiler plate

# required libraries
# sp, rgeos, rgdal, raster, dplyr

# clean -------

unlink("data", recursive = TRUE)
unlink("input", recursive = TRUE)
unlink("plots", recursive = TRUE)


# get data ------

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/shapefiles")) dir.create("data/shapefiles")
source("data_00_download.R")


# process data ------

if (!dir.exists("input")) dir.create("input")
source("data_01_shapefile_preparation.R")
source("data_02_spatial_subsetting.R")

# make plots ------

if (!dir.exists("plots")) dir.create("plots")
source("plots_01_all_plots.R")

# done!
