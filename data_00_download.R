

# download data ----

# get ices eco region shapefiles
download.file("http://gis.ices.dk/shapefiles/ICES_ecoregions.zip",
              "data/ICES_ecoregions.zip")

unzip(zipfile = "data/ICES_ecoregions.zip",
      overwrite = TRUE,
      exdir = "data/shapefiles")

unlink("data/ICES_ecoregions.zip")

# get european coastline shapefiles
download.file("http://data.openstreetmapdata.com/land-polygons-generalized-3857.zip",
              "data/land-polygons-generalized-3857.zip")

unzip(zipfile = "data/land-polygons-generalized-3857.zip",
      overwrite = TRUE,
      exdir = "data/shapefiles")

unlink("data/land-polygons-generalized-3857.zip")


# get datafiles
file.copy("ftp/t.zip", "data/t.zip")

unzip(zipfile = "data/t.zip",
      overwrite = TRUE,
      exdir = "data")

unlink("data/t.zip")
