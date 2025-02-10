

summarize.posteriors <- function (theta, alpha=0.05) {
    ## Makes simultaneous intervals over all locations!
    
    post.mean <- rowMeans(theta)

    exc <- simconf.mc(theta, alpha)
    
    includes.zero <- includes.zero(exc$a, exc$b)

    list(post.mean=post.mean,
         P025=exc$a,
         P975=exc$b,
         includes.zero=includes.zero)
}


summ <- function (x) {

    mn <- round(mean(x), 3)
    CI <- round(quantile(x, c(0.025, 0.975)), 3)
    
    cat(sprintf("%5.3f (%5.3f, %5.3f) ", mn, CI[1], CI[2]),
        file=fname, append=TRUE)
}


posterior.line <- function (latex, name, fname,
                            transform=function (x) x, index) {

    cat(latex, " & ", file=fname, append=TRUE)

    if (missing(index)) {
    
        x <- transform(c(buoy.model1[name], buoy.model2[name]))
    } else {

        x <- transform(c(buoy.model1[name][index,],
                         buoy.model2[name][index,]))
    }

    summ(x)
    cat( " & ", file=fname, append=TRUE)

    if (missing(index)) {
    
        x <- transform(c(ERA.model1[name], ERA.model2[name]))
    } else {

        x <- transform(c(ERA.model1[name][index,],
                         ERA.model2[name][index,]))
    }

    summ(x)
    cat( " & ", file=fname, append=TRUE)

    if (missing(index)) {
    
        x <- transform(c(buoy.anom.model1[name], buoy.anom.model2[name]))
    } else {

        x <- transform(c(buoy.anom.model1[name][index,],
                         buoy.anom.model2[name][index,]))
    }

    summ(x)
    cat( " & ", file=fname, append=TRUE)

    if (missing(index)) {
    
        x <- transform(c(ERA.anom.model1[name], ERA.anom.model2[name]))
    } else {

        x <- transform(c(ERA.anom.model1[name][index,],
                         ERA.anom.model2[name][index,]))
    }

    summ(x)
    cat( "\\\\\n", file=fname, append=TRUE)
}



posterior.line2 <- function (latex, name, fname,
                            transform=function (x) x, index) {

    cat(latex, " & ", file=fname, append=TRUE)

    if (missing(index)) {
    
        x <- transform(c(buoy.model1[name], buoy.model2[name]))
    } else {

        x <- transform(c(buoy.model1[name][index,],
                         buoy.model2[name][index,]))
    }

    summ(x)
    cat( " & ", file=fname, append=TRUE)

    if (missing(index)) {
    
        x <- transform(c(ERA.model1[name], ERA.model2[name]))
    } else {

        x <- transform(c(ERA.model1[name][index,],
                         ERA.model2[name][index,]))
    }

    summ(x)
    cat( " & -- & -- ", file=fname, append=TRUE)
    cat( "\\\\\n", file=fname, append=TRUE)
}

hline <- function (fname) {

    cat("\\hline\n", file=fname, append=TRUE)
}

