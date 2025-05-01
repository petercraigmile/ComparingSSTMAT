

standardize.monthly <- function (z, years, anom=TRUE) {

    not.miss.z <- which(!is.na(z))

    yrs <- years - mean(years[not.miss.z])
    
    month.number <- round((years-floor(years))*12)
    month.factor <- factor(month.number)

    seas.model <- lm(z ~ yrs + month.factor)
    st.hat.star <- c(0, coef(seas.model)[-(1:2)])
    
    ##    seas.model <- lm(z ~ month.factor)
    ##    st.hat.star <- c(0, coef(seas.model)[-(1)])

    st.hat <- st.hat.star - mean(st.hat.star)

    st.hat.long <- st.hat[month.number+1]

    if (anom) {        
        ds <- as.numeric(z - st.hat.long - coef(seas.model)[1] - mean(st.hat.star))
    } else {
        ds <- as.numeric(z - st.hat.long)
    }

    res <- resid(seas.model)

    est.sds <- tapply(res, month.number[not.miss.z], sd, na.rm=TRUE)

    if (length(est.sds)!=12) { stop("Error in estimating the SDs") }

    std <- ds / est.sds[month.number+1]
    
    list(std=std,
         ds=ds,
         est.sds=est.sds,
         st.hat=st.hat)
}
