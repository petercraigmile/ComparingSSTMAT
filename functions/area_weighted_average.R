area.weighted.average <- function (temp.array, areas) {

    sapply(1:dim(temp.array)[[3]], function (l) {

        y <- as.numeric(temp.array[,,l])

        areas.vec <- as.numeric(areas)

        sel <- !is.na(y) & (areas.vec>0)
        
        a <- areas.vec[sel]        
        z <- y[sel]
        
        wt <- a/sum(a)

        wmean     <- sum(wt * z)
        wmean.var <- sum(wt * (z - wmean)^2) * sum(wt^2)
        
        c(wmean, wmean.var)
    })
}

calc.area.by.lat <- function (lat, w, h) {
    ## ======================================================================
    ## Calculate the area of a grid box centered at latitude 'lat' of
    ## width 'w' and height, 'h', in degrees.
    ## See http://mathforum.org/library/drmath/view/63767.html
    ## ======================================================================

    ## Equatorial radius 
    ## R <- 6378.137

    ## Use the mean radius instead
    R <- 6371.0088
    
    R^2 * abs(sin((lat-h/2)*pi/180)-sin((lat+h/2)*pi/180)) * abs(w) * pi / 180
}



calc.area.matrix <- function (nc) {

    ## Calculate the areas    
    ww <- diff(nc$long)[1]
    hh <- diff(nc$lat)[1]

    print(c(ww, hh))
    
    areas <- matrix(NA, length(nc$long), length(nc$lat))
    
    for (j in 1:length(nc$lat)) {
        
        areas[,j] <-  calc.area.by.lat(nc$lat[j], ww, hh)
    }

    areas
}

