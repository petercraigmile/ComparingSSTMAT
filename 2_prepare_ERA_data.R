
library("ncdf4")

source('functions/map_functions.R')
source('functions/area_weighted_average.R')
source('functions/climate_models.R')



ERA <- nc_open("Data/ERA_SST_SAT.nc")

ERAlon  <- ncvar_get(ERA,"longitude")
ERAlat  <- ncvar_get(ERA,"latitude")

ERASST  <- Kelvin.to.Celsius(ncvar_get(ERA,"sst"))
ERASAT  <- Kelvin.to.Celsius(ncvar_get(ERA,"t2m"))

ERA.years <- hours.to.years(ncvar_get(ERA,"time"), first.year=1996)

## Map to the study region of the buoy data
lat.lon.inds <- read.table("Data/lat_long_study_region_indexes.txt", header=TRUE)

ERA.lat <- ERAlat[lat.lon.inds$lat.inds]
ERA.lon <- ERAlon[lat.lon.inds$lon.inds]

ERA.sites <- cbind(ERA.lon, ERA.lat)


ERA.SST <- t(sapply(1:nrow(lat.lon.inds), function (k)
    ERASST[lat.lon.inds$lon.inds[k], lat.lon.inds$lat.inds[k],]))

ERA.MAT <- t(sapply(1:nrow(lat.lon.inds), function (k)
    ERASAT[lat.lon.inds$lon.inds[k], lat.lon.inds$lat.inds[k],]))




## Sort the locations into an "approximate" grid, using the locations

## Move the latitude(s) at 9N to 8N
lats <- ifelse(ERA.lat==9, 8, ERA.lat)

## Leave the longitudes alone
lons <- ERA.lon

ulons <- sort(unique(lons))
ulats <- rev(sort(unique(lats)))

ERA.grid <- as.matrix(expand.grid(ulons, ulats), 2)

ERA.grid.mapping <- sapply(1:nrow(ERA.grid), function (k) {

    sel <- which((ERA.grid[k,1] == lons & ERA.grid[k,2] == lats))

    if (length(sel)==0) {
        NA
    } else {
        sel
    }})





save(list = c("ERA.lat", "ERA.lon", "ERA.sites", "ERA.years",
              "ERA.SST", "ERA.MAT",
              "ERA.grid",
              "ERA.grid.mapping"),
     file = "Derived_Data/ERA_2024_12_03.RData")
