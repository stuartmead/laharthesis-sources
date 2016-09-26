rm(list=ls())
library(raster)
library(rgdal)
setwd("DIRECTORY WITH PILEHIEGHT FILES")

#Get exceedence probability from ecdf (currently 0.1 m)
exceedenceRaster <- function(x){
  1-ecdf(x)(0.1)
}
#Read in and flip raster - Pileheights are reversed in y axis.
processRasters <- function(rasterFile)
{
  r <- flip(raster(rasterFile), direction = 'y')
  r[is.na(r)] <- 0
  return(r)
}

#Get list of directories
dList <- list.dirs(recursive = FALSE)
Hexceed <- list() #Create list of exceedences

#Loop through directories (could run this as an lapply as well, but that seems a bit crazy)
for (i in 1:length(dList))
{
  #Get list of raster files
  fList <- list.files(path = dList[i], pattern="*.asc", full.names = TRUE, recursive = TRUE)
  #Process raster (flip, turn NA into 0)
  rList <- lapply(fList, processRasters)
  #Create raster stack
  heights <- stack(rList)
  
  #Create new raster (exceedence prob)
  r <- raster(heights)
  r <- overlay(heights, fun=exceedenceRaster)
  #Extract AEP (1/ARI)
  aep <- 1/as.numeric(strsplit(dList[i], split = "./ARI")[[1]][2])
  r <- r*aep
  Hexceed[i] <- r
}

#Now get exceedences
Exceedence <- overlay(stack(Hexceed), fun=sum)

writeRaster(Exceedence, "Exceedence0_1m", format="GTiff", overwrite=TRUE)