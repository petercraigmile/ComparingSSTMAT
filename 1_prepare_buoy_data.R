
library(parallel)

source("functions/read_TAO.R")
source("functions/date_functions.R")

## Read in the Marine Average Temperature (MAT)
buoy.locs.MAT <- get.TAO.locations("AIRT")

## Read in the Sea Surface Temperature (SST)
buoy.locs.SST <- get.TAO.locations("SST")

## Define te sites
buoy.sites <- cbind(lon=buoy.locs.MAT$lon, lat=buoy.locs.MAT$lat)




## Try to sort the locations into an approximate grid, using the MAT locations

## Move the latitude(s) at 9N to 8N
lats <- ifelse(buoy.locs.MAT$lat==9, 8, buoy.locs.MAT$lat)

## Leave the longitudes alone
lons <- buoy.locs.MAT$lon

ulons <- sort(unique(lons))
ulats <- rev(sort(unique(lats)))

buoy.grid <- as.matrix(expand.grid(ulons, ulats), 2)

buoy.grid.mapping <- sapply(1:nrow(buoy.grid), function (k) {

    sel <- which((buoy.grid[k,1] == lons & buoy.grid[k,2] == lats))

    if (length(sel)==0) {
        NA
    } else {
        sel
    }})

rm(lats, lons, ulons, ulats)




## Create a list of datasets, one for each buoy location
## 

dfs <- mclapply(1:length(buoy.locs.MAT$latlon), function (k) {

    if (k%%10==0) cat(k, " ")
    
    ll <- buoy.locs.MAT$latlon[k]
    
    MAT <- read.TAO("AIRT", ll)
    SST <- read.TAO("SST",  ll)
    
    ctime <- common.dates(MAT$date, SST$date)
    
    tsel <- ctime.index(SST$date, ctime)
    asel <- ctime.index(MAT$date, ctime)

    SST.dup.dates <- dup.time.indexes(SST$date)
    MAT.dup.dates <- dup.time.indexes(MAT$date)
    
    diffs <- SST$value[tsel] - MAT$value[asel]

    my <- months.and.years(ctime)
    
    list(ll=ll, MAT=MAT, SST=SST, diffs=diffs,
         ctime=ctime, tsel=tsel, asel=asel,
         SST.dup.dates=SST.dup.dates, MAT.dup.dates=MAT.dup.dates,
         months=my$months, years=my$years)
    
}, mc.cores=7)


buoy.dates <- as.Date( julian(as.Date("1992-01-01")):julian(as.Date("2020-12-31")) )

N <- length(buoy.dates)

num.buoy.sites <- length(dfs)


## Summarize the data quality codes

# SST: Ignore 9 that is missing!
tt <- table(unlist(lapply(1:length(dfs), function (k) {

    a <- dfs[[k]]$SST$quality

    a[a!="9"]})))
    
print(round(tt/sum(tt)*100 ,1))

#1    2    3    5 
#15.0 80.1  4.8  0.1 

# MAT: Ignore 9 that is missing, also 0 and 0.08
tt <- table(unlist(lapply(1:length(dfs), function (k) {

    a <- dfs[[k]]$MAT$quality

    a[a!="9" & a!="0" & a!="0.08"]})))
    
print(round(tt/sum(tt)*100 ,1))

#   0 0.08    1    2    3    5 
# 0.0  0.0 13.3 80.8  5.8  0.0 

rm(tt, a)







## ======================================================================
## Create the MAT data variables
## 'MAT', 'MAT.quality', 'MAT.data.mode'
##
## Anything with a quality of 0.08, 0, 3, 4, or 9 is marked as missing.
## The "5" version also makes a quality of 5 missing.
## ======================================================================

MAT <- MAT5 <-
    MAT.quality <- MAT.quality5 <-
        MAT.data.mode <-MAT.data.mode5 <-  matrix(NA, num.buoy.sites, N)

for (k in 1:num.buoy.sites) {

    cat(k, " ")
    
    zz <- dfs[[k]]
    
    ind <- sapply(buoy.dates, function (x) which(x==zz$MAT$date)[1])
    
    qual <- zz$MAT$quality[ind]
    
    MAT[k,]           <- quality.recode(zz$MAT$value[ind], qual)
    MAT5[k,]          <- quality.recode5(zz$MAT$value[ind], qual)
    
    MAT.quality[k,]   <- quality.recode(qual, qual)
    MAT.quality5[k,]  <- quality.recode5(qual, qual)

    MAT.data.mode[k,] <- quality.recode(zz$MAT$data.mode[ind], qual)
    MAT.data.mode5[k,] <- quality.recode5(zz$MAT$data.mode[ind], qual)
}

rm(k, ind, qual)




## ======================================================================
## Create the SST data variables
## 'SST', 'SST.quality', 'SST.data.mode'
##
## Anything with a quality of 0.08, 0, 3, 4, or 9 is marked as missing.
## The "5" version also makes a quality of 5 missing.
## ======================================================================

SST <- SST5 <-
    SST.quality <- SST.quality5 <-
        SST.data.mode <- SST.data.mode5 <-  matrix(NA, num.buoy.sites, N)

for (k in 1:num.buoy.sites) {

    cat(k, " ")
    
    zz <- dfs[[k]]
  
    ind <- sapply(buoy.dates, function (x) which(x==zz$SST$date)[1])
    
    qual <- zz$SST$quality[ind]
    
    SST[k,]           <- quality.recode(zz$SST$value[ind], qual)
    SST5[k,]          <- quality.recode5(zz$SST$value[ind], qual)
    
    SST.quality[k,]   <- quality.recode(qual, qual)
    SST.quality5[k,]  <- quality.recode5(qual, qual)

    SST.data.mode[k,]  <- quality.recode(zz$SST$data.mode[ind], qual)
    SST.data.mode5[k,] <- quality.recode5(zz$SST$data.mode[ind], qual)
}


rm(k, ind, qual)



buoy.daily.MAT <- list(MAT=MAT,
                       MAT5=MAT5,
                       MAT.quality=MAT.quality,
                       MAT.quality5=MAT.quality5,
                       MAT.data.mode=MAT.data.mode,
                       MAT.data.mode5=MAT.data.mode5)


buoy.daily.SST <- list(SST=SST,
                       SST5=SST5,
                       SST.quality=SST.quality,
                       SST.quality5=SST.quality5,
                       SST.data.mode=SST.data.mode,
                       SST.data.mode5=SST.data.mode5)





## Convert to monthly time series, between 'start' and 'end'

start <- "1996-01-01"
end   <- "2018-12-31"

sel <- buoy.dates>=start & buoy.dates<=end

year.mon <- substr(as.character(buoy.dates[sel]), 1, 7)

buoy.SST <- t(sapply(1:nrow(buoy.daily.SST$SST), function (k)
    monthly.mean(buoy.daily.SST$SST[k,sel], year.mon)))

buoy.MAT <- t(sapply(1:nrow(buoy.daily.MAT$MAT), function (k)
    monthly.mean(buoy.daily.MAT$MAT[k,sel], year.mon)))

buoy.SST5 <- t(sapply(1:nrow(buoy.daily.SST$SST), function (k)
    monthly.mean(buoy.daily.SST$SST5[k,sel], year.mon)))

buoy.MAT5 <- t(sapply(1:nrow(buoy.daily.MAT$MAT), function (k)
    monthly.mean(buoy.daily.MAT$MAT5[k,sel], year.mon)))

buoy.SST.count <- t(sapply(1:nrow(buoy.daily.SST$SST), function (k)
    monthly.count(buoy.daily.SST$SST[k,sel], year.mon)))

buoy.MAT.count <- t(sapply(1:nrow(buoy.daily.MAT$MAT), function (k)
    monthly.count(buoy.daily.MAT$MAT[k,sel], year.mon)))

buoy.SST5.count <- t(sapply(1:nrow(SST), function (k)
    monthly.count(buoy.daily.SST$SST5[k,sel], year.mon)))

buoy.MAT5.count <- t(sapply(1:nrow(buoy.daily.MAT$MAT), function (k)
    monthly.count(buoy.daily.MAT$MAT5[k,sel], year.mon)))

buoy.years <- seq(from=1996, by=1/12, length=ncol(buoy.SST))





save(list = c("buoy.dates",
              "buoy.sites",
              "buoy.grid",
              "buoy.grid.mapping",
              "buoy.daily.MAT",
              "buoy.daily.SST",
              "buoy.SST", "buoy.MAT",
              "buoy.SST5", "buoy.MAT5",
              "buoy.SST.count", "buoy.MAT.count",
              "buoy.SST5.count", "buoy.MAT5.count",
              "buoy.years"),
     file = "Derived_Data/buoy_2024_12_03.RData")


