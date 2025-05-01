

## Please run after read_data.R


## Standardize
std.buoy.SST <- buoy.SST
std.buoy.MAT <- buoy.MAT

std.ERA.SST <- ERA.SST
std.ERA.MAT <- ERA.MAT


## Standardize
buoy.SST.SDs <- buoy.MAT.SDs <- ERA.SST.SDs <- ERA.MAT.SDs <- list()



for (k in 1:nrow(buoy.SST)) {

    sm <- standardize.monthly(buoy.SST[k,], buoy.years)

    std.buoy.SST[k,] <- sm$std
    buoy.SST.SDs[[k]] <- sm$est.sds

    sm <- standardize.monthly(buoy.MAT[k,], buoy.years)
    std.buoy.MAT[k,] <- sm$std
    buoy.MAT.SDs[[k]] <- sm$est.sds
    
    sm <- standardize.monthly(ERA.SST[k,], buoy.years)
    std.ERA.SST[k,] <- sm$std
    ERA.SST.SDs[[k]] <- sm$est.sds

    sm <- standardize.monthly(ERA.MAT[k,], buoy.years)
    std.ERA.MAT[k,] <- sm$std
    ERA.MAT.SDs[[k]] <- sm$est.sds
}

rm(k)

std.buoy.SST.minus.MAT <- std.buoy.SST - std.buoy.MAT
std.ERA.SST.minus.MAT  <- std.ERA.SST - std.ERA.MAT

