
source("functions/chordal.R")
source("functions/deseasonalize_monthly.R")


## Load the data
load("Derived_Data/buoy_2024_12_03.RData")
load("Derived_Data/ERA_2024_12_03.RData")

## Calculate the differences
buoy.SST.minus.MAT <- buoy.SST - buoy.MAT
ERA.SST.minus.MAT  <- ERA.SST - ERA.MAT


## Deseasonalize
ds.buoy.SST <- buoy.SST
ds.buoy.MAT <- buoy.MAT

ds.ERA.SST <- ERA.SST
ds.ERA.MAT <- ERA.MAT

for (k in 1:nrow(buoy.SST)) {

    ds.buoy.SST[k,] <- deseasonalize.monthly(buoy.SST[k,], buoy.years)$ds
    ds.buoy.MAT[k,] <- deseasonalize.monthly(buoy.MAT[k,], buoy.years)$ds

    ds.ERA.SST[k,] <- deseasonalize.monthly(ERA.SST[k,], buoy.years)$ds
    ds.ERA.MAT[k,] <- deseasonalize.monthly(ERA.MAT[k,], buoy.years)$ds
}

rm(k)

ds.buoy.SST.minus.MAT <- ds.buoy.SST - ds.buoy.MAT
ds.ERA.SST.minus.MAT  <- ds.ERA.SST - ds.ERA.MAT




us <- buoy.years - min(buoy.years)

N <- length(us)

#DE <- Edist(buoy.sites)
D  <- distance.chord.matrix(buoy.sites[,1], buoy.sites[,2])/1000

## Number of locations
nlocs <- nrow(D)

## Design matrix for time-varying covariates
X.time <- cbind(1, us)

ord.lon.lat <- order(buoy.sites[,2], -buoy.sites[,1])

SST.col <- "darkblue"
MAT.col <- "darkgreen"

month.number <- round((buoy.years-floor(buoy.years))*12)+1

ws <- 2 * pi * us

sin.term <- sin(ws)
cos.term <- cos(ws)

rm(ws)
