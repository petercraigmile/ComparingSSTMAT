
## ======================================================================
## To create ERA_SST_SAT.nc.bz2 run this file in R
##
## Then you can create ERA_SST_SAT.nc by running
## 'bunzip2 ERA_SST_SAT.nc.bz2'
## ======================================================================

stubs <- paste("a", c("a", "b", "c", "d", "e", "f"), sep="")

xs <- sapply(stubs,
       function (x) readBin(paste("ERA_SST_SAT.nc.bz2", x, sep=""),
                            "raw", 200000000))

writeBin(c(xs[[1]], xs[[2]], xs[[3]],
           xs[[4]], xs[[5]], xs[[6]]), "ERA_SST_SAT.nc.bz2")
