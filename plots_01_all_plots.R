# Make Maps of the fishing hours -------

rm(list = ls())

# source common
source("plots_00_plotting_functions.R")

####

# shape files
coast <- rgdal::readOGR("data/shapefiles", "coast", verbose = FALSE)
ices_eco <- rgdal::readOGR("data/shapefiles", "ICES_ecoregions_20150113_no_land", verbose = FALSE)

# which ecoregions to plot
area_list <- c("Greater North Sea", "Celtic Seas")

# plot effort data
data <- read.csv("input/tf05.csv") # effort
plotPages(data, "fishing_hours", main = "Fishinghours", area_list = area_list)
plotPages(data, "mw_fishing_hours", main = "mW Fishinghours", area_list = area_list)

# plot abraision data
data <- read.csv("input/t05.csv")  # abraision
breaks <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)/100
plotPages(data, "Surface_SweptAreaRatio", main = "Surface SweptArea Ratio",
          area_list = area_list, breaks = breaks)
plotPages(data, "SubSurface_SweptAreaRatio", main = "Subsurface SweptArea Ratio",
          area_list = area_list, breaks = breaks)

