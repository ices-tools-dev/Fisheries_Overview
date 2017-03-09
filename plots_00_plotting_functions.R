
# common plot functions

# the big mamma - do everything function
plotPages <- function(data, ivalue, years, breaks, palette, res, main, area_list, digits) {

  # clean data
  data <- data[complete.cases(data[c("Lat", "Lon", "Year", "Fishing_category_FO", "Ecoregion")]),]

  # default settings:
  # last four years in data
  if (missing(years)) years <- tail(sort(unique(data$Year)),4)
  # default breaks for effort...
  if (missing(breaks)) breaks <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)
  if (missing(palette)) palette <- RColorBrewer::brewer.pal(9, "YlOrRd")
  if (missing(res)) res <- 0.05
  # remove _ from column name
  if (missing(main)) main <- gsub("_", " ", ivalue)
  # do all eco regions
  if (missing(area_list)) area_list <- unique(data$Ecoregion)
  if (missing(digits)) {
    # how small is the smallest number (in magnitude)
    decimals <- -1 * min(log(abs(breaks)[breaks != 0], 10))
    # set digits to be the 1st sig fig of the smallest if smallest < 1, otherwise 0
    digits <- pmax(0, decimals)
  }

  # derived quantities
  col <- colorRampPalette(palette)(length(breaks)-1)
  gear_list <- tapply(data$Fishing_category_FO, data$Ecoregion, unique)

  legendtitle <- main
  maintitle <- paste("Average", main)


  # do annual plots
  for (iarea in area_list) # for each ecoregion
    for(igear in gear_list[[iarea]]) {  # for each gear present in that ecoregion

      # set up breaks
      maxvalue <- max(data[data$Fishing_category_FO == igear &
                           data$Ecoregion == iarea &
                         data$Year %in% years, ivalue],
                      na.rm = TRUE)
      ibreaks <- c(breaks[breaks < maxvalue], maxvalue)
      icol <- col[1:(length(ibreaks)-1)]

      fname <- paste0("plots/", ivalue, "_", gsub(" ", "_", iarea), "_", igear, "_", res, ".png")
      png(fname, width = 40, height = 40, res = 600, units = "cm", pointsize = 20)
      par(mfrow = c(2,2), oma = c(1,1,1,7.5), mar = c(2,2,2,2))

      # do plots
      for (iyear in paste(years)) {
        pdata <- data[data$Year == as.integer(iyear) &
                      data$Fishing_category_FO == igear &
                      data$Ecoregion == iarea, ]
        if (nrow(pdata) == 0) {
          # use some dummy data to create raster of NAs
          pdata <- data[data$Year %in% years &
                        data$Fishing_category_FO == igear &
                        data$Ecoregion == iarea, ]
          pdata[[ivalue]] <- NA
        }
        rast <- makeRaster(pdata, value = ivalue, resolution = res)
        plotRaster(rast,
                   ecoregion = ices_eco[ices_eco$Ecoregion == iarea,], land = coast,
                   main = paste0(igear, " ", iyear, ", ", iarea),
                   col = icol, breaks = ibreaks)
     }

      # do legend
      plotLegend(ibreaks, icol, legendtitle, digits)

      dev.off()
    }



  # do mean plots
  for (iarea in area_list) { # for each ecoregion
    # set up breaks (this does not get the max mean value...)
    maxvalue <- max(data[data$Ecoregion == iarea &
                         data$Year %in% years, ivalue],
                    na.rm = TRUE)
    ibreaks <- c(breaks[breaks < maxvalue], maxvalue)
    icol <- col[1:(length(ibreaks)-1)]

    fname <- paste0("plots/mean_", ivalue, "_", gsub(" ", "_", iarea), "_", igear, "_", res, ".png")
    png(fname, width = 40, height = 40, res = 600, units = "cm", pointsize = 20)
    par(mfrow = c(3,3), oma = c(1,1,1,7.5), mar = c(2,2,2,2))

    for(igear in gear_list[[iarea]]) {# for each gear present in that ecoregion
      pdata <- data[data$Year %in% years &
                    data$Fishing_category_FO == igear &
                    data$Ecoregion == iarea, ]
      rast <- makeRaster(pdata, value = ivalue, resolution = res, by = "Year")
      plotRaster(rast,
                   ecoregion = ices_eco[ices_eco$Ecoregion == iarea,], land = coast,
                   main = paste0(igear, ", ", iarea),
                  col = icol, breaks = ibreaks)
    }

    # do legend
    plotLegend(ibreaks, icol, legendtitle, digits)
    title(paste(maintitle, paste(range(years), collapse = "-")), outer = TRUE)

    dev.off()
  }
}





makeRaster <- function(data, value, resolution = 0.005, by = NULL) {
  # make a raster of the data
  loc <- as.matrix(data[c("Lon", "Lat")])
  colnames(loc) <- c('X', 'Y')

  # set up an 'empty' raster, here via an extent object derived from your data
  r <- raster::raster(raster::extent(loc),
                      resolution = resolution,
                      crs = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  if (!is.null(by)) {
    byvalue <- unique(data[[by]])
    x <- lapply(byvalue, function(x) {
           raster::rasterize(loc, r, data[data[[by]] == x, value], fun = sum)
         })
    x <- Reduce("+", x) / length(x)
  } else
  {
    x <- raster::rasterize(loc, r, data[[value]], fun = sum)
  }

  x
}

plotRaster <- function(x, value, ecoregion, land, col, breaks, resolution = 0.005, main = "") {

  # define bounding box based on ecoregion
  bbox <- as(raster::extent(ecoregion) + 1, "SpatialPolygons")
  sp::proj4string(bbox) <- sp::proj4string(ecoregion)

  # make base plot of land and ecoregion outline
  sp::plot(rgeos::gIntersection(land, bbox), col = "light grey", axes = TRUE, border = NA)
  sp::plot(ecoregion, add = TRUE, col = "#FF000001", lwd = .5)

  # plot raster
  raster::plot(x, col = col, breaks = breaks, add = TRUE, legend = FALSE)

  # add title
  mtext(main, outer = F, cex = 1, line = 0.5)
}


plotLegend <- function(breaks, col, title, digits = 0) {

  ncol <- length(breaks) - 1
  fbreaks <- formatC(breaks, format = "f", digits = digits)
  labels <- paste0(fbreaks[(ncol):1], " - ", fbreaks[(ncol + 1):2])

  par(fig = c(0, 1, 0, 1), oma = c(0.2, 0.2, 0.2, 0.2), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')

  legend("topright", y = NULL,
         legend = labels,
         title = title,
         col = col[ncol:1], pch = 15, pt.cex = 2.5,
         bg = "white", inset = c(0, 0), xpd = TRUE)
}
