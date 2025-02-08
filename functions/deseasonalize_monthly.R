
deseasonalize.monthly <- function (z, years, anom=TRUE) {

    not.miss.z <- which(!is.na(z))

    yrs <- years - mean(years[not.miss.z])
    
    month.number <- round((years-floor(years))*12)
    month.factor <- factor(month.number)

    seas.model <- lm(z ~ yrs + month.factor)
    st.hat.star <- c(0, coef(seas.model)[-(1:2)])
    
#    seas.model <- lm(z ~ month.factor)
#    st.hat.star <- c(0, coef(seas.model)[-(1)])

    st.hat <- st.hat.star - mean(st.hat.star)

    st.hat.long <- st.hat[month.number+1]

    if (anom) {        
        ds <- as.numeric(z - st.hat.long - coef(seas.model)[1] - mean(st.hat.star))
    } else {
        ds <- as.numeric(z - st.hat.long)
    }
    
    list(ds=ds,
         st.hat=st.hat)
}


deseasonalize.monthly.sin <- function (z, years, anom=TRUE) {

    ws <- 2*pi*years

    sin.term <- sin(ws)
    cos.term <- cos(ws)

    yrs <- years - mean(years)

    seas.model <- lm(z ~ yrs + sin.term + cos.term)

    ds <- resid(seas.model) + yrs * coef(seas.model)[2]

    list(ds=ds,
         seas.model=seas.model)
}

