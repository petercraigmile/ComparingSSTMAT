

read.TAO <- function (var, latlon, path="Data/") {

    fname <- paste(path, "TAO/TAO_", var, "/TAO_T",
                   latlon, "_M_", var, "_daily.ascii", sep="")
    
    txt <- readLines(fname)[-(1:5)]
    
    deployment.lines <- grep("Deployment", txt)
    YYYYMMDD.lines   <- grep("YYYYMMDD",   txt)
    Height.lines     <- grep("Height",     txt)
    Depth.lines      <- grep("Depth",      txt)

    txt.subset <- txt[-c(deployment.lines, YYYYMMDD.lines, Height.lines, Depth.lines)]
    
    txt.items <- strsplit(txt.subset, " ")
    
    dt0 <- sapply(txt.items, function (x) x[1])
    
    yy <- substr(dt0, 1, 4)
    mm <- substr(dt0, 5, 6)
    dd <- substr(dt0, 7, 8)
    
    date <- as.Date(paste(yy, "-", mm, "-", dd, sep=""))
    
    value0 <- as.numeric(sapply(txt.items, function (x) x[3]))

    value <- ifelse(value0 < -9, NA, value0)

    quality   <- sapply(txt.items, function (x) x[4])

    data.mode <- sapply(txt.items, function (x) x[5])

    list(date=date,
         value=value,
         quality=quality,
         data.mode=data.mode)
}



get.TAO.locations <- function (var, path="Data") {

    ff <- dir(paste(path, "/TAO/TAO_", var, "/", sep=""), "*daily.ascii")
    
    ll.raw <- gsub(paste("_M_", var, "_daily.ascii", sep=""), "",
                   sub("TAO_T", "", ff))

    ll.raw2 <- gsub("N", "N ",
                   gsub("S", "S ", ll.raw))
    
    ll <- strsplit(ll.raw2, " ")
    
    lat <- sapply(ll, function (x) NS.to.degrees(x[1]))
    
    lon <- sapply(ll, function (x) EW.to.degrees(x[2]))
    
    list(lat=lat, lon=lon, latlon=ll.raw)
}




quality.recode <- function (y, quality) {
    ## Anything in 'y' with a quality of 0.08, 0, 3, 4, or 9 is marked
    ## as missing.

    ifelse(quality=="0.08", NA,
    ifelse(quality=="0", NA,
    ifelse(quality=="3", NA,
    ifelse(quality=="4", NA,
    ifelse(quality=="9", NA, y)))))
}



quality.recode5 <- function (y, quality) {
    ## Anything in 'y' with a quality of 0.08, 0, 3, 4, 5, or 9 is
    ## marked as missing.

    ifelse(quality=="0.08", NA,
    ifelse(quality=="0", NA,
    ifelse(quality=="3", NA,
    ifelse(quality=="4", NA,
    ifelse(quality=="5", NA,
    ifelse(quality=="9", NA, y))))))
}



NS.to.degrees <- function (lat) {
    ## Convert North-South to degrees latitude

    dir     <- substr(lat, nchar(lat), nchar(lat))
    abs.val <- as.numeric(substr(lat, 1, nchar(lat)-1))
    abs.val * ifelse(dir=="N", 1, -1)
}

    
EW.to.degrees <- function (lon) {
    ## Convert East-West to degrees latitude

    dir     <- substr(lon, nchar(lon), nchar(lon))
    abs.val <- as.numeric(substr(lon, 1, nchar(lon)-1))
    ifelse(dir=="E", abs.val-360, -abs.val)
}


