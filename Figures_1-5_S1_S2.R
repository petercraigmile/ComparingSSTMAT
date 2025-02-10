
library(maps)
library(fields)
library(scales)

source("read_data.R")

source("functions/paper_functions.R")
source('functions/deseasonalize_monthly.R')


SST.pct.missing <- apply(buoy.SST, 1, function (x) mean(is.na(x))*100)
MAT.pct.missing <- apply(buoy.MAT, 1, function (x) mean(is.na(x))*100)
diff.pct.missing <- apply(buoy.SST-buoy.MAT, 1, function (x) mean(is.na(x))*100)

daily.SST.pct.missing <- apply(buoy.daily.SST$SST, 1, function (x) mean(is.na(x))*100)
daily.MAT.pct.missing <- apply(buoy.daily.MAT$MAT, 1, function (x) mean(is.na(x))*100)
daily.diff.pct.missing <- apply(buoy.daily.SST$SST-buoy.daily.MAT$MAT, 1, function (x) mean(is.na(x))*100)


range(c(SST.pct.missing, MAT.pct.missing,diff.pct.missing))




pdf(file="figures/Fig1_map.pdf", width=6.3, height=5/2)
par(mfrow=c(1,1), cex=0.7, mar=c(2.9,2.9,0.5,0.5), mgp=c(1.6,0.5,0), bty="l")

show.map("")

points(buoy.sites, pch=19, cex=1.3, col="gray20")

dev.off()



pdf(file="figures/FigS1_missing_daily_data.pdf", width=6.3, height=7)
par(mfrow=c(3,1), cex=0.7, mar=c(2.9,2.9,1.4,0.5), mgp=c(1.6,0.5,0), bty="l")

show.map("SST: percentage missing")

grid.text(fmt(daily.SST.pct.missing))

show.map("MAT: percentage missing")

grid.text(fmt(daily.MAT.pct.missing))

show.map("SST-MAT: percentage missing")

grid.text(fmt(daily.diff.pct.missing))

dev.off()



pdf(file="figures/FigS2_missing_monthly_data.pdf", width=6.3, height=7)
par(mfrow=c(3,1), cex=0.7, mar=c(2.9,2.9,1.4,0.5), mgp=c(1.6,0.5,0), bty="l")

show.map("SST: percentage missing")

grid.text(fmt(SST.pct.missing))

show.map("MAT: percentage missing")

grid.text(fmt(MAT.pct.missing))

show.map("SST-MAT: percentage missing")

grid.text(fmt(diff.pct.missing))

dev.off()





czlim <- c(19, 31)
czlim2 <- c(-2, 4)

pdf(file="figures/Fig2_heat_maps.pdf", width=7, height=6.5)
par(mfrow=c(3,2), cex=0.75, mar=c(2.5,2.5,1,0.5), mgp=c(1.8,0.5,0), bty="L")

image.plot(buoy.years, 1:nlocs, t(buoy.SST[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n", yaxt="n")
par(cex=0.75)
mtext("TAO buoys: SST", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(ERA.SST[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA5: SST", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(buoy.MAT[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("TAO buoys: MAT", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(ERA.MAT[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA5: MAT", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t((buoy.SST-buoy.MAT)[ord.lon.lat,]),
           xlab="", ylab="",yaxt='n',zlim=czlim2, ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("TAO buoy: SST - MAT", side=3, line=0, cex=0.8)
ax()

image.plot(buoy.years, 1:nlocs, t((ERA.SST-ERA.MAT)[ord.lon.lat,]),
           xlab="", ylab="",yaxt='n',zlim=czlim2, ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA: SST - MAT", side=3, line=0, cex=0.8)
ax()

dev.off()

    
    


czlim <- c(-6,6)
czlim2 <- c(-6, 6)

pdf(file="figures/Fig3_heat_maps_ds.pdf", width=7, height=6.5)
par(mfrow=c(3,2), cex=0.75, mar=c(2.5,2.5,1,0.5), mgp=c(1.8,0.5,0), bty="L")

image.plot(buoy.years, 1:nlocs, t(ds.buoy.SST[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n", yaxt="n")
par(cex=0.75)
mtext("TAO buoys: SST", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(ds.ERA.SST[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA5: SST", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(ds.buoy.MAT[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("TAO buoys: MAT", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t(ds.ERA.MAT[ord.lon.lat,]), zlim=czlim,
           xlab="", ylab="",yaxt='n', ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA5: MAT", side=3, line=0, cex=0.9)
ax()

image.plot(buoy.years, 1:nlocs, t((ds.buoy.SST-ds.buoy.MAT)[ord.lon.lat,]),
           xlab="", ylab="",yaxt='n',zlim=czlim2, ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("TAO buoy: SST - MAT", side=3, line=0, cex=0.8)
ax()

image.plot(buoy.years, 1:nlocs, t((ds.ERA.SST-ds.ERA.MAT)[ord.lon.lat,]),
           xlab="", ylab="",yaxt='n',zlim=czlim2, ann=FALSE, xaxt="n")
par(cex=0.75)
mtext("ERA: SST - MAT", side=3, line=0, cex=0.8)
ax()

dev.off()



cylim2 <- c(-1.8, 3.4)

pdf(file="figures/Fig4_differences.pdf", width=6.8, height=6)
par(mfrow=c(7,8), cex=0.45, mar=c(1.5,1.5,1,0.2), oma=c(1.4,1.4,0,0), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(buoy.grid.mapping)) {
    
    gj <- buoy.grid.mapping[j]
   
    if (handle.empty.plot(gj, buoy.ERA.label=TRUE)) {
                
        plot(buoy.years, ERA.SST.minus.MAT[gj,],
             type="l", xlab="", ylab="", ylim=cylim2, col=alpha("gray40", 0.95), xaxt="n")

        lines(buoy.years, buoy.SST.minus.MAT[gj,], col=alpha("blue", 0.95))
        
        axis(side=1, at=seq(1996, 2016, 10), labels=paste(seq(1996, 2016, 10)), cex=0.45)
    }
    
}

grid.outside.labels()

dev.off()




pdf(file="figures/Fig5_deseasonalized_differences.pdf", width=6.8, height=6)
par(mfrow=c(7,8), cex=0.45, mar=c(1.5,1.5,1,0.2), oma=c(1.4,1.4,0,0), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(buoy.grid.mapping)) {
    
    gj <- buoy.grid.mapping[j]
   
    if (handle.empty.plot(gj, buoy.ERA.label=TRUE)) {
                
        plot(buoy.years, ds.ERA.SST.minus.MAT[gj,],
             type="l", xlab="", ylab="", ylim=cylim2, col=alpha("gray40",0.95), xaxt="n")

        lines(buoy.years, ds.buoy.SST.minus.MAT[gj,], col=alpha("blue", 0.95))

        axis(side=1, at=seq(1996, 2016, 10), labels=paste(seq(1996, 2016, 10)), cex=0.45)
    }
    
}

grid.outside.labels()

dev.off()


