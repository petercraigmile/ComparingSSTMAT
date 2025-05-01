
library(maps)
library(fields)
library(scales)

source("read_data.R")

source("functions/paper_functions.R")
source('functions/deseasonalize_monthly.R')
source('functions/standardize_monthly.R')

source("read_data_std.R")





par(mfrow=c(2,2), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

matplot(simplify2array(buoy.SST.SDs), type="l", ylim=c(0, 2.5))
matplot(simplify2array(buoy.MAT.SDs), type="l", ylim=c(0, 2.5))
matplot(simplify2array(ERA.SST.SDs), type="l", ylim=c(0, 2.5))
matplot(simplify2array(ERA.MAT.SDs), type="l", ylim=c(0, 2.5))

sel <- c(1, 4, 7, 10, 12)

txt <- substr(month.name, 1, 3)
txt[12] <- ""

pdf(file="figures/FigS3_std_SDs.pdf", width=6.8, height=6)
par(mfrow=c(7,8), cex=0.45, mar=c(1.5,1.5,1,0.4), oma=c(1.4,1.4,0,0), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(buoy.grid.mapping)) {
    
    gj <- buoy.grid.mapping[j]
   
    if (handle.empty.plot(gj, buoy.ERA.label=FALSE, orig.label=TRUE)) {

        plot(1:12, ERA.SST.SDs[[gj]], type="l", col="green", ylim=c(0, 2), xaxt="n")
        lines(1:12, buoy.SST.SDs[[gj]], type="l", col="red")

        axis(side=1, at=(1:12)[sel], txt[sel], line=0.15, cex=0.5, tick=FALSE)

        axis(side=1, at=sel[-5], labels=FALSE)
        
        lines(1:12, ERA.MAT.SDs[[gj]], type="l", col="gray40", ylim=c(0, 2))
        lines(1:12, buoy.MAT.SDs[[gj]], type="l", col="blue")

    }
    
}

grid.outside.labels()

dev.off()




czlim <- c(-6,6)
czlim2 <- c(-6, 6)

pdf(file="figures/FigS4_heat_maps_std.pdf", width=7, height=6.5)
par(mfrow=c(3,2), cex=0.75, mar=c(2.5,2.5,1,0.5), mgp=c(1.8,0.5,0), bty="L")

image.plot(buoy.years, 1:nlocs, t(std.buoy.SST[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n", yaxt="n")
par(cex=0.75)
mtext("TAO buoys: SST", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(std.ERA.SST[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA5: SST", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(std.buoy.MAT[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("TAO buoys: MAT", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(std.ERA.MAT[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA5: MAT", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t((std.buoy.SST-std.buoy.MAT)[ord.lon.lat,]),
           xlab="", ylab="",yaxt='n',zlim=czlim2, ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("TAO buoy: SST - MAT", side=3, line=0, cex=0.8)
ax()

image.plot(buoy.years, 1:nlocs, t((std.ERA.SST-std.ERA.MAT)[ord.lon.lat,]),
           xlab="", ylab="",yaxt='n',zlim=czlim2, ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA: SST - MAT", side=3, line=0, cex=0.8)
ax()

dev.off()




cylim2 <- c(-4, 3.2)

pdf(file="figures/FigS5_std_differences.pdf", width=6.8, height=6)
par(mfrow=c(7,8), cex=0.45, mar=c(1.5,1.5,1,0.2), oma=c(1.4,1.4,0,0), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(buoy.grid.mapping)) {
    
    gj <- buoy.grid.mapping[j]
   
    if (handle.empty.plot(gj, buoy.ERA.label=TRUE)) {
                
        plot(buoy.years, std.ERA.SST.minus.MAT[gj,],
             type="l", xlab="", ylab="", ylim=cylim2, col=alpha("gray40",0.95), xaxt="n")

        lines(buoy.years, std.buoy.SST.minus.MAT[gj,], col=alpha("blue", 0.95))

        axis(side=1, at=seq(1996, 2016, 10), labels=paste(seq(1996, 2016, 10)), cex=0.45)
    }
    
}

grid.outside.labels()

dev.off()

