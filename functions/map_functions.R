
empty.map <- function (label, xlab="", ylab="") {

    plot(-220, -12, xlab=xlab, ylab=ylab, type="n",
         xlim=c(-220, -70), ylim=c(-12, 12), cex=0.5)##, xaxt="n", yaxt="n")

    if (!missing(label)) {
        text(-220, 11, label, cex=2, col="gray", pos=4)
    }
}



add.map <- function () {

    library(maps)
    
    map("world", add=TRUE, wrap = c(-260, -260+360),
        fill=TRUE, col="gray90", border="gray65")
}



add.time.series.to.map <- function (x, y, w, h, times, series,
                                    ylim) {

    x1 <- x-0.5*w
    x2 <- x+0.5*w

    y1 <- y-0.5*h
    y2 <- y+0.5*h

    us <- (times - min(times)) / (max(times) - min(times))

    zs <- (series - ylim[1]) / (ylim[2]- ylim[1])

    the.zero <- (0 - ylim[1]) / (ylim[2]- ylim[1])
    
#    points(sites[1,1], sites[1,2])
##    rect(x1, y1, x2, y2, border="gray50", col="gray90", lwd=0.5)
    rect(x1, y1, x2, y2, border=NA, col="gray90", lwd=0.5)
    lines(x1 + us * w, rep(y1 + the.zero * h, length(times)), col="gray50", lwd=0.5)
    
    lines(x1 + us * w, y1 + zs * h, lwd=0.25)
}




##empty.map()
##add.map()
