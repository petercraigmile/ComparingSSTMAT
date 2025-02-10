


grid.outside.labels <- function () {

    ll  <- buoy.sites[ord.lon.lat,2]
    ull <- unique(ll)
    ull <- ull[ull < 9]
    
    txt <- ifelse(ull<0,
                  paste(abs(ull), "°S", sep=""),
           ifelse(ull>0, 
                  paste(ull, "°N", sep=""),
                  "0° "))
    
    mtext(txt, at=seq(0.1, 0.955, len=length(txt))-0.03, outer=TRUE,
          side=2, cex=0.7, line=0)
    
    ll  <- buoy.sites[ord.lon.lat,1]
    ull <- unique(ll)
    
    txt <- c("165°E",
             paste(c(180, 170, 155, 140, 125, 110, 95), "°W", sep=""))
    
    mtext(txt, at=seq(0.1, 0.976, len=length(txt))-0.03, outer=TRUE,
          side=1, cex=0.7, line=0.2)
}


ax <- function () {

    axis(side=1, at=seq(1996, 2020, 2))
    
    ll  <- buoy.sites[ord.lon.lat,2]
    ull <- unique(ll)
    ull <- ull[ull < 9]

    txt <- ifelse(ull<0,
                  paste(abs(ull), "°S", sep=""),
           ifelse(ull>0, 
                  paste(ull, "°N", sep=""),
                  "0° "))
    
    axis(side=2, at=match(ull, ll), txt, las=1)
}


fmt <- function (x, digits=1) {

    prettyNum(round(x, digits), nsmall=digits, width=3+digits)
}


clabel <- function (x, y, text1, text2, col="black") {

    text(x, y, text1, col=col, cex=0.75)

    if (!missing(text2)) {

        text(x, y-1, text2, col=col, cex=0.75)
    }
    invisible()
}


show.map <- function (title, col="black",
                      country.labels=TRUE) {

    xlim <- c(-210, -79.5)
    ylim <- c(-11, 12)
    
    plot(0, 0, xlim=xlim, ylim=ylim, type="n", 
         xlab="Longitude",ylab="Latitude", yaxt="n", xaxt="n")

    axis(at=c(-10, -5, 0, 5, 10),
         label=c("10°S", "5°S", "0°", "5°N", "10°N"),
         side=2)

    axis(at=c(-200, -180, -160, -140, -120, -100),
         label=c("160°E", "180°W", "160°W", "140°W",
                 "120°W", "100°W"),
         side=1)
    
    map("world", add=TRUE, wrap = c(-260, -260+360),
        col="gray80", fill=TRUE, border="gray60")

    if (!missing(title)) {

        mtext(title, col=col, side=3, line=0, cex=1)
    }
    
    if (country.labels) {
        
        clabel(-210, -7, "Papua", "New Guinea")
        clabel(-203, -6, "Solomon", "Islands")
        
        clabel(-82,  12, "Nicaragua")
        clabel(-84,  10, "Costa Rica")
        clabel(-79,  8.5, "Panama")
        clabel(-78.5,  4, "Colombia")
        clabel(-79.5, -1, "Ecuador")
        clabel(-77, -6, "Peru")    
    }
}


mtitle <- function (text, col="black", cex=0.6) {

    mtext(text, side=3, cex=cex, line=0, col=col)
}


grid.text <- function (text, col="black", cex=1.2) {

    text(buoy.sites[,1], buoy.sites[,2], text,
         cex=cex, col=col, pos=2)
}



add.time.series.to.map <- function (x, y, w, h, times, series,
                                    ylim, ...) {

    x1 <- x-0.5*w
    x2 <- x+0.5*w

    y1 <- y-0.5*h
    y2 <- y+0.5*h

    us <- (times - min(times)) / (max(times) - min(times))

    zs <- (series - ylim[1]) / (ylim[2]- ylim[1])

    the.zero <- (0 - ylim[1]) / (ylim[2]- ylim[1])
    
#    points(sites[1,1], sites[1,2])
##    rect(x1, y1, x2, y2, border="gray50", col="gray90", lwd=0.5)
#    rect(x1, y1, x2, y2, border=NA, col="gray90", lwd=0.5)
#    lines(x1 + us * w, rep(y1 + the.zero * h, length(times)), col="gray50", lwd=0.5)
    
    lines(x1 + us * w, y1 + zs * h, lwd=1, ...)
}



handle.empty.plot <- function (gj, SST.MAT.label=FALSE, buoy.ERA.label=FALSE) {

    if (is.na(gj)) {
        
        plot(0, 0, type="n", xlab="", ylab="", axes=F)
        
        if ((j==6) & (SST.MAT.label)) {
            
            text(0,  0.5+0.3, "SST", cex=2, col=SST.col)
            text(0, -0.5+0.3, "MAT", cex=2, col=MAT.col)
        }

        if ((j==6) & (buoy.ERA.label)) {
            
            text(-0.1,  0.5+0.1, "ERA5", cex=2, col="gray40")
            text(-0.1, -0.5+0.2, "Buoy", cex=2, col="blue")
        }

    }

    !is.na(gj)
}



grid.par <- function () {

    par(mfrow=c(7,8), cex=0.45, mar=c(1.7,1.7,1.2,0.4), mgp=c(1.8,0.5,0), bty="L")
}



month.labels <- function () {

    axis(side=1, at=1:12, labels=substr(month.name, 1, 1), cex=0.6)
}






monthly.estimate <- function (z) {

    st.hat <- tapply(z, month.number, mean, na.rm=TRUE)

    st.hat - mean(st.hat)
     
}

includes.zero <- function (L, U) {

    L < 0 & U > 0
}
