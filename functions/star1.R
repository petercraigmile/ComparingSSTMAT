
## Spatially varying AR(1) processes


star1.phi.to.eta <- function (phi) {

    log( (1.0 + phi) / (1.0 - phi) )
}


star1.eta.to.phi <- function (eta) {

    (exp(eta) - 1.0) / (exp(eta) + 1.0)
}


star1.sim <- function (N, mu=0, phi, Sigma) {

    m <- nrow(Sigma)   
    y <- matrix(NA, N, m)
    
    y[1,] <- mvtnorm::rmvnorm(1, sigma=Sigma)

    for (t in 2:N) {

        y[t,] <- phi * y[t-1,] + mvtnorm::rmvnorm(1, sigma=Sigma)
    }

    y + mu
}


star1.Sigma0.inv <- function (phi, Sigma.inv) {

    if (length(phi)==1) {
        
        Sigma0.inv <- Sigma.inv * (1.0 - phi^2)
        
    } else {
        
        Sigma0.inv <- Sigma.inv * (-phi %*% t(phi) + 1.0)
    }

    return(Sigma0.inv)
}


star1.Sigma0 <- function (phi, Sigma) {

    if (length(phi)==1) {
        
        Sigma0 <- Sigma / (1.0 - phi^2)
        
    } else {
        
        Sigma0 <- Sigma / (-phi %*% t(phi) + 1.0)
    }

    return(Sigma0)
}



star1.loglik <- function (y, mu, phi, Sigma) {

    z  <- y - mu
    ts <- 2:nrow(y)

    log.dvnorm(z[1,], sigma=star1.Sigma0(phi, Sigma)) +
        sum(log.dmvnorm(star1.U(z, phi), sigma=Sigma))
}



star1.mu <- function (X, omega, use.C=TRUE) {
    ## ======================================================================
    ## X is N x q
    ## omega is nlocs x q
    ## ======================================================================

    if (use.C) {

        cppStar1_mu(X, omega)
    } else {
    
        t.omega <- t(omega)        
        t(sapply(1:nrow(X), function (k) colSums(t.omega * X[k,])))
    }
}



star1.U <- function (y, phi, use.C=TRUE) {
    ## ======================================================================
    ## y is N x nlocs
    ## phi is a vector of length nlocs
    ## ======================================================================

    if (use.C) {

        cppStar1_U(y,  phi)
    } else {
        
        ts <- 2:nrow(y)
        ty <- t(y)
        
        t(ty[,ts] - phi * ty[,ts-1])
    }
}





star1.phi0 <- function (y) {

    as.numeric(sapply(1:ncol(y), function (k)
        arima(y[,k], c(1,0,0), include.mean=FALSE)$coef))
}



star1.omega.OLS <- function (y, X.time) {

    t(sapply(1:nrow(y), function (k) {
        
        yy <- y[k,]
        obs <- !is.na(yy)
        coef(lm.fit(rbind(X.time[obs,]), yy[obs]))
    }))    
}
