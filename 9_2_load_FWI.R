rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(rgdal)
library(maptools) # loads sp library too
library(fields)
library(maps)
library(Hmisc)
source("/Users/marco/Dropbox/estcena/scripts/Murcia_FIRE/scripts/t2sti.R")

data(wrld_simpl)

## fix parameters
dir_oss = '~/Dropbox/estcena/scripts/Murcia_FIRE/data/'
dir_out = dir_oss
dir_shp = '/Users/marco/Documents/virtualBox/temiav/Murcia_Region/'
anni = 1991:2023
mesi = rep(1:12, length(anni))

CRS.new <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#===============================================================================
# 1). shapefile -----
#===============================================================================

file_shp = paste0(dir_shp,'Murcia_Region.shp')
shp <- readOGR(file_shp)
# shp=shp[which(shp$NUTS_ID == 'ES51'),]
# plot(shp)
# shp = spTransform(shp,
#                   CRS(
#                     "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#                   ))
proj4string(shp) <- CRS.new

#===============================================================================
# 1). climate data -----
#===============================================================================

## load PRECIPITATION
fname <- file.path(dir_oss, 'Murcia_FWI_1991_2023_monthly.nc')
obs.nc <- nc_open(fname)

obs <- ncvar_get(obs.nc, "fwinx")
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat
lat <- rev(lat)
obs=obs[,ncol(obs):1,]
ni = dim(obs)[1]
nj = dim(obs)[2]
# Convert longitude values greater than 180°

image.plot(lon, lat, apply(obs, c(1, 2), mean))
plot(wrld_simpl, add = TRUE)

image.plot(lon, lat, obs[,,1])
plot(wrld_simpl, add = TRUE)

points <- expand.grid(lon, lat)
pts = SpatialPoints(points)
proj4string(pts) <- CRS.new

## pixel to regions
sm_reg=rep(NA, length(anni) * 12)

ii <- !is.na(over(pts, shp))
inout = ii[, 1]
dim(inout) <- c(length(lon), length(lat))
inout[inout == 0] = NA
image.plot(lon, lat, inout)
plot(wrld_simpl, add = TRUE)
for (itime in 1:dim(obs)[3]) {
  sm_reg[ itime] = mean(obs[, , itime] * inout, na.rm = T)
}

## forecast ave and low conditions

## calculate SSI
ssi3_ave = sm_reg * NA
ssi6_ave = sm_reg * NA
ssi12_ave = sm_reg * NA

fwi3_ave = STI(sm_reg, 3)
fwi6_ave = STI(sm_reg, 6)
fwi12_ave = STI(sm_reg, 12)
plot(fwi12_ave)


# plot.ts(spei3_ave[1,])
save(fwi3_ave, file = file.path(dir_out, "FWI3_ERA5_1991_2023.RData"))
save(fwi6_ave, file = file.path(dir_out, "FWI6_ERA5_1991_2023.RData"))
save(fwi12_ave, file = file.path(dir_out, "FWI12_ERA5_1991_2023.RData"))
