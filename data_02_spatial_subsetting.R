
# -----------------------------------------

rm(list = ls())

# read eco region shapefiles
ices_eco <- rgdal::readOGR("data/shapefiles", "ICES_ecoregions_20150113_no_land", verbose = FALSE)

# function to spatial join ICES eco region onto data
process_data <- function(file) {
  # read in data
  x <- read.csv(file.path("data", file))

  # create SpatialPointsDataFrames
  sp::coordinates(x) <- ~ Lon + Lat
  sp::proj4string(x) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # tag on eco-region to data
  x$Ecoregion <- sp::over(x, ices_eco)$Ecoregion

  as.data.frame(x)
}

# for the following files '<files>.csv' process and save
files <- c("t05", "tf05", "tf10")

for (i in files) {
  x <- process_data(paste0(i, ".csv"))
  assign(i, value = x)
}

# save input data files for later use (as shapefiles)

for (i in files)
  write.csv(get(i), file = paste0("input/", i, ".csv"), row.names = FALSE)
