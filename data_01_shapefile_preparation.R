# shapefile preparation -----

rm(list = ls())

# read eco region shapefiles
ices_eco <- rgdal::readOGR("data/shapefiles", "ICES_ecoregions_20150113_no_land", verbose = FALSE)

# read coastline shapefiles and transform to wgs84
coast <- rgdal::readOGR("data/shapefiles/land-polygons-generalized-3857", "land_polygons_z5", verbose = FALSE)
coast <- sp::spTransform(coast, sp::CRS(sp::proj4string(ices_eco)))

# trim coastline to eco region extent for ploting
bbox <- as(raster::extent(ices_eco), "SpatialPolygons")
sp::proj4string(bbox) <- sp::proj4string(ices_eco)
coast <- rgeos::gIntersection(coast, bbox, byid = TRUE)
coast <- rgeos::gUnaryUnion(coast)
coast <- as(coast, "SpatialPolygonsDataFrame")

# save coastline to shapefiles folder
rgdal::writeOGR(coast, "data/shapefiles", "coast", driver = "ESRI Shapefile", overwrite_layer = TRUE)
