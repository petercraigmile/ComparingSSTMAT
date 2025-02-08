

common.dates <- function (x, y) {
    ## For two sets of dates 'x' and 'y' calculate the common dates
    ## sorted by increasing date.

    as.Date(sort(unique(c(x,y))))
    ## as.Date(sort(intersect(x, y)))
}


ctime.index <- function (the.date, ctime) {
    ## For each of the common time dates in 'ctime' find the first
    ## matching index in 'the.date' that matches.

    sapply(ctime, function (tt) which(the.date==tt)[1])
}



dup.time.indexes <- function (the.date) {

    tab <- table(the.date)
    
    as.Date(names(tab[tab > 1]))
}




months.and.years <- function (ctime) {
    
    months <- factor(as.numeric(substr(ctime, 6, 7)))
    
    years <- (julian(ctime) - julian(as.Date("1970-01-01"))) / 365.25

    list(months=months, years=years)
}




calculate.months.years <- function (the.dates, from="1996-01-01", to="2018-12-31") {

    sel <- the.dates >= from & the.dates <= to

    dts <- the.dates[sel]

    year.mon <- substr(dts, 1, 7)
    
    uyear.mon   <- unique(year.mon)
    uyr   <- as.numeric(substr(uyear.mon, 1, 4))
    umon  <- as.numeric(substr(uyear.mon, 6, 7))
    years <- uyr + (umon-1)/12
    
    list(sel=sel, years=years, year.mon=year.mon)
}



monthly.count <- function (x, year.mon, min.days=15) {

    fill.in <- function (x, min.days) {
        
        mm <- sum(!is.na(x))
        
        if (mm >= min.days) {
            
            mm
        } else {
            
            NA
        }
    }
    
    y <- tapply(x, year.mon, function (w) w)
    
    as.numeric(sapply(y, fill.in, min.days=min.days))
}



monthly.mean <- function (x, year.mon, min.days=15) {
    
    fill.in <- function (x, min.days) {
        
        mm <- sum(!is.na(x))
        
        if (mm >= min.days) {
            
            mean(x, na.rm=TRUE)
        } else {
            
            NA
        }
    }
    
    y <- tapply(x, year.mon, function (w) w)
    
    as.numeric(sapply(y, fill.in, min.days=min.days))
}





