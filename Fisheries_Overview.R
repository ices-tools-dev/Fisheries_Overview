rm(list=ls(all=TRUE)) 
options(digits=5, scipen=10)


#library(sqldf)
#library(plyr)
#library(maps)
#library(mapdata)
#library(mapproj)
library(mapplots)
library(maptools)
library(shapefiles)
library(scales)
library(RColorBrewer)
library(sp)
library(rgdal)
#library(rgeos)
library(data.table)

path <- "V:/USER/JEPOL/16-10-3_Scott_ices_maps/sample/"
path_u <- "V:/USER/JEPOL/16-10-3_Scott_ices_maps/sample/"

path <- "...your_folder.../sample/"

outPath <- "Maps/"
GisPath <- "Gis/"
datapath <- "data/" 

setwd(path)
mkfldrs <-0                                                       
if(mkfldrs ==1){ 
  setwd(path)
  dir.create(outPath) 
  dir.create(GisPath) 
  dir.create(datapath) 
}                                                                 
rm(mkfldrs)                       



nor <- read.shapefile(paste(path, GisPath, "nor", sep=""))
cel <- read.shapefile(paste(path, GisPath, "cel", sep=""))
norm <- read.shapefile(paste(path, GisPath, "norm", sep=""))
celm <- read.shapefile(paste(path, GisPath, "celm", sep=""))

Coast <- read.shapefile(paste(path, GisPath, "europe_wgs84", sep=""))


t05 <- fread(paste(path, datapath, "t05.csv", sep=""), header = T)
tf05 <- fread(paste(path, datapath, "tf05.csv", sep=""), header = T)
tf10 <- fread(paste(path, datapath, "tf10.csv", sep=""), header = T)


####### Turn Dataframes into SpatialPointsDataFrame
latlon <- CRS("+proj=longlat +datum=WGS84")
t05s <- t05
coordinates(t05s) <- ~ Longitude + Latitude
proj4string(t05s) <-  latlon 

tf10s <- tf10
coordinates(tf10s) <- ~ Lon + Lat
proj4string(tf10s) <-  latlon 

tf05s <- tf05
coordinates(tf05s) <- ~ Lon + Lat
proj4string(tf05s) <-  latlon 

###### Extract the points inside North Sea area #######  (This requires a lot of RAM in large datasets, is there a better way?)
NS <- readShapeSpatial(paste(path, GisPath, "nor", sep=""), proj4string=latlon)
NS <- as(NS,"SpatialPolygons")

overNS <- over(t05s, NS)
t05NS <- t05[!is.na(overNS),]
rm(overNS)

overNS <- over(tf10s, NS)
tf10NS <- tf10[!is.na(overNS),]
rm(overNS)

overNS <- over(tf05s, NS)
tf05NS <- tf05[!is.na(overNS),]
rm(overNS)

###### Extract the points inside Celtic Sea area #######
CS <- readShapeSpatial(paste(path, GisPath, "cel", sep=""), proj4string=latlon)
CS <- as(CS,"SpatialPolygons")


overCS <- over(t05s, CS)
t05CS <- t05[!is.na(overCS),]
rm(overCS)

overCS <- over(tf10s, CS)
tf10CS <- tf10[!is.na(overCS),]
rm(overCS)

overCS <- over(tf05s, CS)
tf05CS <- tf05[!is.na(overCS),]
rm(overCS)

###########

rm(t05, tf05, tf10, CS, NS, latlon)



svdat <-1       ## Set to 1 of you want to save the data                                                
if(svdat ==1){ 
save(t05CS,file=paste(path, datapath, "t05CS.Rdata",sep=""))
save(tf05CS,file=paste(path, datapath, "tf05CS.Rdata",sep=""))
save(tf10CS,file=paste(path, datapath, "tf10CS.Rdata",sep=""))
save(t05NS,file=paste(path, datapath, "t05NS.Rdata",sep=""))
save(tf05NS,file=paste(path, datapath, "tf05NS.Rdata",sep=""))
save(tf10NS,file=paste(path, datapath, "tf10NS.Rdata",sep=""))
}


load(paste(path, datapath, "t05CS.Rdata",sep=""))
load(paste(path, datapath, "tf05CS.Rdata",sep=""))
load(paste(path, datapath, "tf10CS.Rdata",sep=""))
load(paste(path, datapath, "t05NS.Rdata",sep=""))   
load(paste(path, datapath, "tf05NS.Rdata",sep=""))     
load(paste(path, datapath, "tf10NS.Rdata",sep=""))
          

########################################################
## Make Maps of the fishing hours
########################################################
intervals <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)
col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(13)

####

CreatePlot <- function(gearp, yearp){
  ty <- t[t$Year==yearp & t$Fishing_category_FO==gearp,]
  
  grd <- make.grid(ty$Lon,ty$Lat,ty$fishing_hours, byx, byy, xlim, ylim)
  basemap(xlim=xlim, ylim=ylim, main = "", bg="white")

  
  if(nrow(ty)!=0) {   # draw.grid will terminate the script if there is no values in the grid
    maxvalue <- max(na.omit(ty$fishing_hours))
    breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    #breaks <- c(0,1,2,5,10,20,50,100,200,500,1000,2000, max(ty$fishing_hours))
    cols <- col[1:(length(breaks)-1)]
  draw.grid(grd, breaks=breaks,col=cols)
  }
  
  draw.shape(around, col="white", border ="transparent")
  draw.shape(Coast, col="light grey", border="transparent", xlim=xlim, ylim=ylim)
  draw.shape(inside,col=alpha("red",0.0001),lwd=.5, border="black")
  mtext(paste(gearp, " ", yearp, ", ", area,  sep = ""), outer=F, cex=1, line=0.5)
}


#####################
## Resolution
#####################

for (r in 2:2){
  ## 0.1 resolution
  if(r=="1"){
    byx = 0.1
    byy = 0.1
    res <- "0.1"
  }  else{
    ## 0.05 resolution
    byx = 0.05
    byy = 0.05
    res <- "0.05"
  }
  
  #####################
  ## Extent
  #####################
  
  for (e in 1:2) {
    if(e=="1" & r =="1"){
      #Greater North Sea 0.1
      t <- tf10NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.95, 13.15)
      ylim <- c(47.95,62.05)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="1"){
      
      ##Celtic Sea 0.1
      t <- tf10CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.15, 1.95)
      ylim <- c(47.95,64.05)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    if(e=="1" & r =="2"){
      #Greater North Sea 0.05
      t <- tf05NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.955, 13.155)
      ylim <- c(47.955,62.055)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="2"){
      
      ##Celtic Sea 0.05
      t <- tf05CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.155, 1.955)
      ylim <- c(47.955,64.055)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    
    #####################
    ## Gear
    #####################
    
    for(f in 1:length(gear_list)) {  # for each gear
      
      gear <- gear_list[f]
    
    names<-paste("Fishing_hours_", area_n, "_", gear, "_", res, ".png",sep = "")
    path <- paste(path_u, outPath, names, sep="")
    png(file=path, width=40, height=40, res=600, units="cm", pointsize=20)
    par(mfrow=c(2,2), oma=c(1,1,1,7.5), mar=c(2,2,2,2))
    
    for(j in 1:4){
      CreatePlot(gear, 2011+j)
    }
    
    par(fig=c(0, 1, 0, 1), oma=c(0.2, 0.2, 0.2, 0.2), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    maxvalue <- max(na.omit(t$fishing_hours[t$Fishing_category_FO==gear]))
    breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    
    cols <- col[1:(length(breaks)-1)]
    
    legend.grid("topright", xpd=T, breaks=breaks, type=2, col=cols, bg="white", title="Fishing hours", 
                inset = c(0, 0))
    
    dev.off()
  }
  
}

}



########################################################
## Make Maps of the mean fishing hours
########################################################
intervals <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)
col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(13)





CreatePlot_calc <- function(gearp){
  
  ty <- t[t$Fishing_category_FO==gearp,]
  
  g1 <- make.grid(ty$Lon,ty$Lat,ty$fishing_hours[t$Year==2012], byx, byy, xlim, ylim)
  #g1[is.na(g1)] <- 0
  
  g2 <- make.grid(ty$Lon,ty$Lat,ty$fishing_hours[t$Year==2013], byx, byy, xlim, ylim)
  #g2[is.na(g2)] <- 0
 
  g3 <- make.grid(ty$Lon,ty$Lat,ty$fishing_hours[t$Year==2014], byx, byy, xlim, ylim)
  #g3[is.na(g3)] <- 0
  
  g4 <- make.grid(ty$Lon,ty$Lat,ty$fishing_hours[t$Year==2015], byx, byy, xlim, ylim)
  #g4[is.na(g4)] <- 0
  

  g <- g1+g2+g3+g4/4  ### Calculate the average for the years 2012-2015
  
  #g[g==0] <- NA
  
  #Make Map
  
  
  maxvalue <- which.max(g)
  
  breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
  cols <- col[1:length(breaks)-1]
  basemap(xlim=xlim, ylim=ylim, main = "", bg="white")
  draw.grid(g, breaks=breaks,col=cols)
  draw.shape(around, col="white", border ="transparent")
  draw.shape(Coast, col="light grey", border="transparent", xlim=xlim, ylim=ylim)
  draw.shape(inside,col=alpha("red",0.0001),lwd=.5, border="black")
  
  mtext(paste(gearp, ", ", area,  sep = ""), outer=F, cex=1, line=0.5)
}


#####################
## Resolution
#####################

for (r in 1:2){
  ## 0.1 resolution
  if(r=="1"){
    byx = 0.1
    byy = 0.1
    res <- "0.1"
  }  else{
    ## 0.05 resolution
    byx = 0.05
    byy = 0.05
    res <- "0.05"
  }
  
  #####################
  ## Extent
  #####################
  
  for (e in 1:2) {
    if(e=="1" & r =="1"){
      #Greater North Sea 0.1
      t <- tf10NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
    
      xlim <- c(-4.95, 13.15)
      ylim <- c(47.95,62.05)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="1"){
      
      ##Celtic Sea 0.1
      t <- tf10CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.15, 1.95)
      ylim <- c(47.95,64.05)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    if(e=="1" & r =="2"){
      #Greater North Sea 0.05
      t <- tf05NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.955, 13.155)
      ylim <- c(47.955,62.055)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="2"){
      
      ##Celtic Sea 0.05
      t <- tf05CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.155, 1.955)
      ylim <- c(47.955,64.055)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    
    #####################
    ## Gear
    #####################
    
    
    maxvalue <- max(na.omit(t$fishing_hours))
    breaks1 <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    cols1 <- col[1:(length(breaks1)-1)]
    
    
    names<-paste("Mean_Fishing_hours_", area_n, "_", res, ".png",sep = "")
    path <- file.path(paste(path_u, outPath, sep=""),paste(names, sep = ""))
    png(file=path, width=40, height=40, res=600, units="cm", pointsize=20)
    par(mfrow=c(3,3), oma=c(1,1,1,7.5), mar=c(2,2,2,2))
    
    for(j in 1:length(gear_list)){
      gear <- gear_list[j]
      CreatePlot_calc(gear)
    }
    
    par(fig=c(0, 1, 0, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    title("Average fishinghours 2012-2015", outer=TRUE)
    legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols1, bg="white", title="Fishing hours", 
                inset = c(0, 0))
    
    dev.off()
  }
  
}



########################################################
## Make Maps of the kW fishing Hours
########################################################
intervals <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)
col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(13)




CreatePlot <- function(gearp, yearp){
  
  ty <- t[t$Year==yearp & t$Fishing_category_FO==gearp,]
  
  grd <- make.grid(ty$Lon,ty$Lat,ty$fishing_hours, byx, byy, xlim, ylim)
  basemap(xlim=xlim, ylim=ylim, main = "", bg="white")

  if(nrow(ty)!=0) {   # draw.grid will terminate the script if there is no values in the grid
    maxvalue <- max(na.omit(ty$fishing_hours))
    breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    #breaks <- c(0,1,2,5,10,20,50,100,200,500,1000,2000, max(ty$fishing_hours))
    cols <- col[1:(length(breaks)-1)]
    draw.grid(grd, breaks=breaks,col=cols)
  }
  
  draw.shape(around, col="white", border ="transparent")
  draw.shape(Coast, col="light grey", border="transparent", xlim=xlim, ylim=ylim)
  draw.shape(inside,col=alpha("red",0.0001),lwd=.5, border="black")
  
  mtext(paste(gearp, " ", yearp, ", ", area,  sep = ""), outer=F, cex=1, line=0.5)
}



#####################
## Resolution
#####################

for (r in 1:2){
  ## 0.1 resolution
  if(r=="1"){
    byx = 0.1
    byy = 0.1
    res <- "0.1"
  }  else{
    ## 0.05 resolution
    byx = 0.05
    byy = 0.05
    res <- "0.05"
  }
  
  #####################
  ## Extent
  #####################
  
  for (e in 1:2) {
    if(e=="1" & r =="1"){
      #Greater North Sea 0.1
      t <- tf10NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.95, 13.15)
      ylim <- c(47.95,62.05)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="1"){
      
      ##Celtic Sea 0.1
      t <- tf10CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.15, 1.95)
      ylim <- c(47.95,64.05)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    if(e=="1" & r =="2"){
      #Greater North Sea 0.05
      t <- tf05NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.955, 13.155)
      ylim <- c(47.955,62.055)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="2"){
      
      ##Celtic Sea 0.05
      t <- tf05CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.155, 1.955)
      ylim <- c(47.955,64.055)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    
    #####################
    ## Gear
    #####################
    
    for(f in 1:length(gear_list)) {  # for each gear
      
      gear <- gear_list[f]
      
      maxvalue <- max(na.omit(t$mw_fishinghours[t$Fishing_category_FO==gear]))
      breaks1 <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
      cols <- col[1:(length(breaks1)-1)]
      
      names<-paste("mw_fishinghours_", area_n, "_", gear, "_", res, ".png",sep = "")
      path <- file.path(paste(path_u, outPath, sep=""),paste(names, sep = ""))
      png(file=path, width=40, height=40, res=600, units="cm", pointsize=20)
      par(mfrow=c(2,2), oma=c(1,1,1,7.5), mar=c(2,2,2,2))
      
      for(j in 1:4){
        CreatePlot(gear, 2011+j)
      }
      
      par(fig=c(0, 1, 0, 1), oma=c(0.2, 0.2, 0.2, 0.2), mar=c(0, 0, 0, 0), new=TRUE)
      plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
      
      
      legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols, bg="white", title="mW fishinghours", 
                  inset = c(0, 0))
      
      dev.off()
    }
    
  }
  
}




########################################################
## Make Maps of the mean kW fishing Hours
########################################################
intervals <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)
col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(13)



CreatePlot_calc <- function(gearp){
  
  ty <- t[t$Fishing_category_FO==gearp,]
  
  g1 <- make.grid(ty$Lon,ty$Lat,ty$mw_fishinghours[t$Year==2012], byx, byy, xlim, ylim)
  #g1[is.na(g1)] <- 0
  
  g2 <- make.grid(ty$Lon,ty$Lat,ty$mw_fishinghours[t$Year==2013], byx, byy, xlim, ylim)
  #g2[is.na(g2)] <- 0
  
  g3 <- make.grid(ty$Lon,ty$Lat,ty$mw_fishinghours[t$Year==2014], byx, byy, xlim, ylim)
  #g3[is.na(g3)] <- 0
  
  g4 <- make.grid(ty$Lon,ty$Lat,ty$mw_fishinghours[t$Year==2015], byx, byy, xlim, ylim)
  #g4[is.na(g4)] <- 0
  
  
  g <- g1+g2+g3+g4/4  ### Calculate the average for the years 2012-2015
  
  #g[g==0] <- NA
  
  #Make Map
  
  
  maxvalue <- which.max(g)
  
  breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
  cols <- col[1:length(breaks)-1]
  
  #Make Map
  
  basemap(xlim=xlim, ylim=ylim, main = "", bg="white")
  draw.grid(g, breaks=breaks,col=cols)
  draw.shape(around, col="white", border ="transparent")
  draw.shape(Coast, col="light grey", border="transparent", xlim=xlim, ylim=ylim)
  draw.shape(inside,col=alpha("red",0.0001),lwd=.5, border="black")
  
  mtext(paste(gearp, ", ", area,  sep = ""), outer=F, cex=1, line=0.5)
}



#####################
## Resolution
#####################

for (r in 1:2){
  ## 0.1 resolution
  if(r=="1"){
    byx = 0.1
    byy = 0.1
    res <- "0.1"
  }  else{
    ## 0.05 resolution
    byx = 0.05
    byy = 0.05
    res <- "0.05"
  }
  
  #####################
  ## Extent
  #####################
  
  for (e in 1:2) {
    if(e=="1" & r =="1"){
      #Greater North Sea 0.1
      t <- tf10NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.95, 13.15)
      ylim <- c(47.95,62.05)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="1"){
      
      ##Celtic Sea 0.1
      t <- tf10CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.15, 1.95)
      ylim <- c(47.95,64.05)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    if(e=="1" & r =="2"){
      #Greater North Sea 0.05
      t <- tf05NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-4.955, 13.155)
      ylim <- c(47.955,62.055)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    }  
    if(e=="2" & r =="2"){
      
      ##Celtic Sea 0.05
      t <- tf05CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      t <- t[t$Year>2011,]
      gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
      xlim <- c(-16.155, 1.955)
      ylim <- c(47.955,64.055)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    
    #####################
    ## Gear
    #####################
    
    
    maxvalue <- max(na.omit(t$mw_fishinghours))
    breaks1 <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    cols <- col[1:(length(breaks1)-1)]
    
    
    
    names<-paste("Mean_mw_fishinghours_", area_n, "_", res, ".png",sep = "")
    path <- file.path(paste(path_u, outPath, sep=""),paste(names, sep = ""))
    png(file=path, width=40, height=40, res=600, units="cm", pointsize=20)
    par(mfrow=c(3,3), oma=c(1,1,1,7.5), mar=c(2,2,2,2))
    
    for(j in 1:length(gear_list)){
      
      gear <- gear_list[j]
      CreatePlot_calc(gear)
    }
    
    par(fig=c(0, 1, 0, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    title("Average mW fishinghours 2012-2015", outer=TRUE)
    legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols, bg="white", title="mW Fishinghours", 
                inset = c(0, 0))
    
    dev.off()
  }
  
}





#############################################################################################################################################






########################################################
## Make Maps of the abrasion levels
########################################################

intervals <- (c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)/100)

col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(13)


CreatePlot <- function(gearp, yearp){
  ty <- t[t$Year==yearp&t$Fishing_category_FO==gearp,]
  
  if(depth=="sur") {
    grd <- make.grid(ty$Lon,ty$Lat,ty$Surface_SweptAreaRatio, byx, byy, xlim, ylim)
    maxvalue <- max(na.omit(ty$Surface_SweptAreaRatio))
  }else{
    grd <- make.grid(ty$Lon,ty$Lat,ty$SubSurface_SweptAreaRatio, byx, byy, xlim, ylim)
    maxvalue <- max(na.omit(ty$SubSurface_SweptAreaRatio))
  }
  
  
  basemap(xlim=xlim, ylim=ylim, main = "", bg="white")
  
  if(nrow(ty)!=0) {   # draw.grid will terminate the script if there is no values in the grid
    breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    #breaks <- c(0,1,2,5,10,20,50,100,200,500,1000,2000, max(ty$fishing_hours))
    cols <- col[1:(length(breaks)-1)]
    draw.grid(grd, breaks=breaks,col=cols)
  }
  
  draw.shape(around, col="white", border ="transparent")
  draw.shape(Coast, col="light grey", border="transparent", xlim=xlim, ylim=ylim)
  draw.shape(inside,col=alpha("red",0.0001),lwd=.5, border="black")
  
  mtext(paste(gearp, " ", yearp, ", ", area,  sep = ""), outer=F, cex=1, line=0.5)
}




byx = 0.05
byy = 0.05


#####################
## Extent
#####################

for (e in 1:2) {

  if(e=="1") {
    #Greater North Sea 0.05
    t <- t05NS
    t <- t[complete.cases(t[,"Fishing_category_FO"]),]
    xlim <- c(-4.955, 13.155)
    ylim <- c(47.955,62.055)
    area <- "Greater North Sea"
    area_n <- "NS"
    around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
    inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
  } else {
    
    ##Celtic Sea 0.05
    t <- t05CS
    t <- t[complete.cases(t[,"Fishing_category_FO"]),]
    xlim <- c(-16.155, 1.955)
    ylim <- c(47.955,64.055)
    area <- "Celtic Sea"
    area_n <- "CS"
    around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
    inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
  }
  
  gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
  #####################
  ## Gear
  #####################
  for(g in 1:2) {
    if (g==1) {
      depth <- "sur"
    }else{
      depth <- "sub"
    }
    
    for(f in 1:length(gear_list)) {  # for each gear
      
      gear <- gear_list[f]
      if(depth=="sur") {
        maxvalue <- max(na.omit(t$Surface_SweptAreaRatio[t$Fishing_category_FO==gear]))
        names<-paste("Surface_SweptAreaRatio_", area_n, "_", gear, ".png",sep = "")
      }else{
        maxvalue <- max(na.omit(t$SubSurface_SweptAreaRatio[t$Fishing_category_FO==gear]))
        names<-paste("Subsurface_SweptAreaRatio_", area_n, "_", gear, ".png",sep = "")
      }
      
      breaks1 <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
      cols1 <- col[1:(length(breaks1)-1)]
      
      
      path <- file.path(paste(path_u, outPath, sep=""),paste(names, sep = ""))
      png(file=path, width=40, height=40, res=600, units="cm", pointsize=20)
      par(mfrow=c(2,2), oma=c(1,1,1,7.5), mar=c(2,2,2,2))
      
      for(j in 1:4){
        CreatePlot(gear, 2011+j)
      }
      
      par(fig=c(0, 1, 0, 1), oma=c(0.2, 0.2, 0.2, 0.2), mar=c(0, 0, 0, 0), new=TRUE)
      plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
      
      if(depth=="sur") {
        legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols1, bg="white", title="Surface SweptArea Ratio", 
                    inset = c(0, 0))
      }else{
        
        
        legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols1, bg="white", title="Subsurface SweptArea Ratio", 
                    inset = c(0, 0))
      }
      
      dev.off()
    }
    
  }
  
  
}



########################################################
## Make Maps of the mean abrasion levels
########################################################


intervals <- (c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000, 10000)/100)

col <- colorRampPalette(brewer.pal(9, "YlOrRd"))(13)


CreatePlot_calc <- function(gearp){
  
  ty <- t[t$Fishing_category_FO==gearp,]
if(depth=="sur") {
  g1 <- make.grid(ty$Lon,ty$Lat,ty$Surface_SweptAreaRatio[t$Year==2012], byx, byy, xlim, ylim)
  g2 <- make.grid(ty$Lon,ty$Lat,ty$Surface_SweptAreaRatio[t$Year==2013], byx, byy, xlim, ylim)
  g3 <- make.grid(ty$Lon,ty$Lat,ty$Surface_SweptAreaRatio[t$Year==2014], byx, byy, xlim, ylim)
  g4 <- make.grid(ty$Lon,ty$Lat,ty$Surface_SweptAreaRatio[t$Year==2015], byx, byy, xlim, ylim)
}else{
  g1 <- make.grid(ty$Lon,ty$Lat,ty$SubSurface_SweptAreaRatio[t$Year==2012], byx, byy, xlim, ylim)
  g2 <- make.grid(ty$Lon,ty$Lat,ty$SubSurface_SweptAreaRatio[t$Year==2013], byx, byy, xlim, ylim)
  g3 <- make.grid(ty$Lon,ty$Lat,ty$SubSurface_SweptAreaRatio[t$Year==2014], byx, byy, xlim, ylim)
  g4 <- make.grid(ty$Lon,ty$Lat,ty$SubSurface_SweptAreaRatio[t$Year==2015], byx, byy, xlim, ylim) 
}  
  
  
  g <- g1+g2+g3+g4/4  ### Calculate the average for the years 2012-2015
  
  #g[g==0] <- NA
  
  #Make Map
  
  
  maxvalue <- which.max(g)
  breaks <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
  cols <- col[1:length(breaks)-1]
  
  #Make Map
  
  basemap(xlim=xlim, ylim=ylim, main = "", bg="white")
  if(maxvalue!=-Inf)
  draw.grid(g, breaks=breaks,col=cols)
  draw.shape(around, col="white", border ="transparent")
  draw.shape(Coast, col="light grey", border="transparent", xlim=xlim, ylim=ylim)
  draw.shape(inside,col=alpha("red",0.0001),lwd=.5, border="black")
  
  mtext(paste(gearp, ", ", area,  sep = ""), outer=F, cex=1, line=0.5)
}


byx = 0.05
byy = 0.05

#####################
## depth
#####################

for (depth in c("sur", "sub")) { 
  
  
  #####################
  ## Extent
  #####################
  levels(factor(t$Fishing_category_FO))
  
  
  for (e in 1:2)  {
    
    
    if(e=="1") {
      #Greater North Sea 0.05
      t <- t05NS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      xlim <- c(-4.955, 13.155)
      ylim <- c(47.955,62.055)
      area <- "Greater North Sea"
      area_n <- "NS"
      around <- norm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/norm')
      inside <- nor #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/nor')
    } else {
      
      ##Celtic Sea 0.05
      t <- t05CS
      t <- t[complete.cases(t[,"Fishing_category_FO"]),]
      xlim <- c(-16.155, 1.955)
      ylim <- c(47.955,64.055)
      area <- "Celtic Sea"
      area_n <- "CS"
      around <- celm #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/celm')
      inside <- cel #read.shapefile('V:/USER/JEPOL/16-10-3_Scott_ices_maps/Gis/cel')
    }
    
    
    
    #####################
    ## Gear
    #####################
    
    if(depth=="sur") {
      maxvalue <- max(na.omit(t$Surface_SweptAreaRatio))
      names<-paste("Mean_Surface_SweptAreaRatio_", area_n, ".png",sep = "")
    }else{
      maxvalue <- max(na.omit(t$SubSurface_SweptAreaRatio))
      names<-paste("Mean_SubSurface_SweptAreaRatio_", area_n, ".png",sep = "")
    }
    
    breaks1 <- unlist(lapply(maxvalue,function(x) c(intervals[intervals< x], x)))
    cols1 <- col[1:(length(breaks1)-1)]
    
    
    path <- file.path(paste(path_u, outPath, sep=""),paste(names, sep = ""))
    png(file=path, width=40, height=40, res=600, units="cm", pointsize=20)
    par(mfrow=c(2,2), oma=c(1,1,1,7.5), mar=c(2,2,2,2))
    
    
    gear_list<-as.character(unique(na.omit(t$Fishing_category_FO)))
    
    
    for(j in 1:length(gear_list)){
      gear <- gear_list[j]
      CreatePlot_calc(gear)
    }
    
    par(fig=c(0, 1, 0, 1), oma=c(1, 1, 1, 1), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    if(depth=="sur") {
      title("Average Surface SweptArea Ratio 2012-2015", outer=TRUE)
      legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols1, bg="white", title="Surface SweptArea Ratio", 
                  inset = c(0, 0))
      
    }else{
      title("Average Subsurface SweptArea Ratio 2012-2015", outer=TRUE)
      legend.grid("topright", xpd=T, breaks=breaks1, type=2, col=cols1, bg="white", title="Subsurface SweptArea Ratio", 
                  inset = c(0, 0))
      
    }
    
    
    dev.off()
  }}


