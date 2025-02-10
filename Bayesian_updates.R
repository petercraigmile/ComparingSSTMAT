
init.star1.Bayesian.model <- function (model.name,
                                       zmat,
                                       X.mu, X.eta,
                                       eta.block.length,
                                       eta.prop.sd             = 0.1,
                                       log.eta.lambda.prop.sd  = 0.2,
                                       log.omega.lambda.prop.sd = rep(0.3, n.omega),
                                       log.U.lambda.prop.sd     = 0.2) {

    x <- MCMC()

    x$model.name <- model.name

    nlocs <- nrow(D)

    ## zeta indicates where the missing values are
    ## z0 is the data matrix with zeros when we have missing values

    z0 <- zeta <- matrix(NA, nrow(zmat), ncol(zmat))
    
    for (j in 1:nlocs) {

        z0[,j]    <- ifelse(is.na(zmat[,j]), 0, zmat[,j])
        zeta[,j]  <- ifelse(is.na(zmat[,j]), 0, 1)
    }

    x$zmat <- zmat
    x$z0   <- z0
    x$zeta <- zeta

    ## Measurement error variance

    create(x, "sigma2", 0.01)

    ## Parameters for the autocorrelation parameters

    create(x, "eta.mu", 0)
    create(x, "eta.tau2", 0.1)
    create(x, "eta.lambda", 4)
    create(x, "eta.lambda.jumps", FALSE)
    
    create(x, "eta", rnorm(nlocs, x$eta.mu, sqrt(x$eta.tau2)))

    create(x, "eta.jumps", FALSE)

    x$X.eta      <- X.eta
    x$R.eta.chol <- chol(GSP.exp(D, c(1, x$eta.lambda)))

    x$eta.block.length <- eta.block.length

    ## Parameters for the mean
    
    x$X.mu <- X.mu
    n.omega <- ncol(X.mu)

    create(x, "omega.mu", rep(1, n.omega))
    create(x, "omega.tau2", rep(1, n.omega))
    create(x, "omega.lambda", rep(1, n.omega))
    create(x, "omega.lambda.jumps", rep(FALSE, n.omega))
    
    x$R.omega.chol <- lapply(1:n.omega, function (k)
        chol(GSP.exp(D, c(1, x$omega.lambda[k]))))

    omega0 <- sapply(1:n.omega, function (k)
        rnorm(nlocs, x$omega.mu[k], sqrt(x$omega.tau2[k])))

    x$X.omega <- lapply(1:n.omega, function (k) cbind(rep(1, nlocs)))

    create(x, "omega", omega0)
    x$mu.Y <- star1.mu(X.mu, x$omega)
    
    ## Parameters for the innovations

    create(x, "U.tau2", 1)
    create(x, "U.lambda", 0.5)
    create(x, "U.lambda.jumps", FALSE)

    x$R.U.chol <- chol(GSP.exp(D, c(1, x$U.lambda)))

    ## Latent y process
    
    create(x, "y", x$z0)

    ## Proposal standard deviations

    x$eta.prop.sd              <- eta.prop.sd
    x$log.eta.lambda.prop.sd   <- log.eta.lambda.prop.sd
    x$log.omega.lambda.prop.sd <- log.omega.lambda.prop.sd
    x$log.U.lambda.prop.sd     <- log.U.lambda.prop.sd 

    x
}



star1.Bayesian.adapt.proposals <- function (x) {
    
    ol <- log(x["omega.lambda"])
    
    x$log.omega.lambda.prop.sd <- sapply(1:nrow(ol), function (k)
        sqrt(MCMC.adapt.cov(ol[k,])))
    
    x$log.eta.lambda.prop.sd   <- sqrt(MCMC.adapt.cov(log(x["eta.lambda"])))
    
    x$log.U.lambda.prop.sd     <- sqrt(MCMC.adapt.cov(log(x["U.lambda"])))

    x$eta.prop.sd <- sqrt(MCMC.adapt.cov(as.numeric(x["eta"]))/x$eta.block.length)
    
#    x$eta.prop.sd              <- 0.1
}





update.eta.pars <- function (x) {
    
    mh <- update.GSP.pars(x$eta.tau2, x$eta.lambda,
                          chol.R=x$R.eta.chol,
                          z=x$eta, X=x$X.eta, dists=D,
                          cov.fun=GSP.exp,
                          log.lambda.prop.sd=x$log.eta.lambda.prop.sd,
                          beta.mu=0, beta.prec=1/10,
                          tau2.shape=0.01, tau2.rate=0.01,
                          lambda.shape=2*10, lambda.rate=1*10)

    update(x, "eta.mu", mh$beta)
    update(x, "eta.tau2", mh$tau2)
    update(x, "eta.lambda", mh$lambda)
    update(x, "eta.lambda.jumps", mh$jump)

    if (mh$jump) {

        x$R.eta.chol <- mh$chol.R
    }
}


update.omega.pars <- function (x) {

    n.omega <- ncol(x$omega)

    new.omega.mu       <- x$omega.mu
    new.omega.tau2     <- x$omega.tau2
    new.omega.lambda   <- x$omega.lambda
    omega.lambda.jumps <- rep(NA, n.omega)

    for (k in 1:n.omega) {

        mh <- update.GSP.pars(x$omega.tau2[k], x$omega.lambda[k],
                              chol.R=x$R.omega.chol[[k]],
                              z=x$omega[,k], X=x$X.omega[[k]], dists=D,
                              cov.fun=GSP.exp,
                              log.lambda.prop.sd=x$log.omega.lambda.prop.sd[k],
                              beta.mu=0, beta.prec=1/10,
                              tau2.shape=0.01, tau2.rate=0.01,
                              lambda.shape=2*10, lambda.rate=1*10)
        
        new.omega.mu[k] <- mh$beta
        new.omega.tau2[k] <- mh$tau2
        new.omega.lambda[k] <- mh$lambda
        omega.lambda.jumps[k] <- mh$jump
        
        if (mh$jump) {
            
            x$R.omega.chol[[k]] <- mh$chol.R
        }
    }

    update(x, "omega.mu", new.omega.mu)
    update(x, "omega.tau2", new.omega.tau2)
    update(x, "omega.lambda", new.omega.lambda)
    update(x, "omega.lambda.jumps", omega.lambda.jumps)
}



update.U.pars <- function (x) {

    lambda.shape <- 2*10
    lambda.rate  <- 1*10

    shape <- 0.01
    rate  <- 0.01

    z <- x$y - x$mu.Y

    phi <- star1.eta.to.phi(x$eta)
    
    R0 <- star1.Sigma0(phi, t(x$R.U.chol))
    
    U <- star1.U(z, phi)
    
    LU  <- base::backsolve(x$R.U.chol, t(U), transpose=TRUE)
    
    RSS <- sum(LU^2) + sum(solve(R0, z[1,]) * z[1,])

    new.U.tau2 <- 1.0 / rgamma(1, shape + 0.5 * prod(dim(z)), rate + 0.5 * RSS)

    update(x, "U.tau2", new.U.tau2)

    log.U.lambda     <- log(x$U.lambda)

    new.log.U.lambda <- rnorm(1, log.U.lambda, x$log.U.lambda.prop.sd)

    new.U.lambda     <- exp(new.log.U.lambda)

    new.R.U <- GSP.exp(D, c(1, new.U.lambda))

    new.R0 <- star1.Sigma0(phi, new.R.U)

    new.R.U.chol <- tryCatch(base::chol(new.R.U), error = function (e) e)

    ll.new <- log.dmvnorm(z[1,], sigma = x$U.tau2 * new.R0) +
        sum( log.dmvnorm(U, sigma = new.R.U.chol * sqrt(x$U.tau2), chol=TRUE) ) +
        dgamma(new.U.lambda, lambda.shape, lambda.rate, log=TRUE) +
        new.log.U.lambda ## log Jacobian
    
    ll.old <- log.dmvnorm(z[1,], sigma = x$U.tau2 * R0) +
        sum( log.dmvnorm(U, sigma = x$R.U.chol * sqrt(x$U.tau2), chol=TRUE) ) +
        dgamma(x$U.lambda, lambda.shape, lambda.rate, log=TRUE) +
        log.U.lambda ## log Jacobian

    jump <- MH(ll.new, ll.old)
    update(x, "U.lambda.jumps", jump)

    if (jump) {
        update(x, "U.lambda", new.U.lambda)
        x$R.U.chol <- new.R.U.chol
    } else {
        update(x, "U.lambda", x$U.lambda)
    }   
}


update.eta <- function (x) {

    eta.mu <- drop(x$X.eta %*% x$eta.mu)
    
    block.length <- x$eta.block.length

    eta.Sigma  <- x$eta.tau2 * crossprod(x$R.eta.chol)

    chol.Sigma <- sqrt(x$U.tau2) * x$R.U.chol
    Sigma      <- t(chol.Sigma)

    z <- x$y - x$mu.Y
    
    m <- length(x$eta)
    
    curr.eta <- x$eta
    curr.phi <- star1.eta.to.phi(curr.eta)

    Sigma0 <- star1.Sigma0(curr.phi, Sigma)

    ms <- sample(1:m)
    
    js <- seq(1, m, block.length)
   
    jumps <- 0
    
    for (k in js) {

        new.eta <- curr.eta
        new.phi <- curr.phi
        
        ks <- ms[k+(0:(block.length-1))]
        new.eta[ks] <- rnorm(block.length, curr.eta[ks], x$eta.prop.sd)
        new.phi[ks] <- star1.eta.to.phi(new.eta[ks])

        Sigma0.new <- star1.Sigma0(new.phi, Sigma)

        new.U  <- star1.U(z, new.phi)
        curr.U <- star1.U(z, curr.phi)

        S21 <- eta.Sigma[-ks,][,ks]
        B <- solve(eta.Sigma[-ks,][,-ks], S21)
       
        cond.mean <- eta.mu[ks] + crossprod(B, new.eta[-ks] - eta.mu[-ks])
        cond.cov  <- eta.Sigma[ks,][,ks] - crossprod(S21, B)
        
        llp.new <- log.dmvnorm(z[1,], sigma=Sigma0) +
            sum(log.dmvnorm(new.U, sigma=chol.Sigma, chol=TRUE)) + 
            log.dmvnorm(new.eta[ks], cond.mean, cond.cov)
        
        llp.cur <- log.dmvnorm(z[1,], sigma=Sigma0.new) +
            sum(log.dmvnorm(curr.U, sigma=chol.Sigma, chol=TRUE)) +
            log.dmvnorm(curr.eta[ks], cond.mean, cond.cov)

        the.jump <- MH(llp.new, llp.cur)

        if (is.na(the.jump)) {
            the.jump <- FALSE
        }
        
        jumps <- jumps + the.jump

        if (the.jump) {

            Sigma0 <- Sigma0.new
            curr.eta[ks] <- new.eta[ks]
            curr.phi[ks] <- new.phi[ks]
        }
    }
    
    update(x, "eta", curr.eta)
    update(x, "eta.jumps", jumps/length(js))
}



update.sigma2 <- function (x) {

    obs <- as.numeric(x$zeta)==1

    z.vec <- as.numeric(x$z0)
    y.vec <- as.numeric(x$y)

    update(x, "sigma2",
           update.var.invgamma(z.vec[obs], y.vec[obs]))
}



update.y <- function (x) {

    mu.Y  <- x$mu.Y
    new.y <- x$y

    phi <- star1.eta.to.phi(x$eta)

    N <- nrow(new.y)
    nlocs <- length(phi)

    Sigma.U.inv <- chol2inv(x$R.U.chol) / x$U.tau2
    Sigma.U0.inv <- star1.Sigma0(phi, Sigma.U.inv)

    D <- diag(phi)
    B <- crossprod(D, Sigma.U.inv)

    rhos <- star1.U(mu.Y, phi)

    t <- 1
    
    m2 <- new.y[t+1,] - rhos[t,]

    P <- diag(x$zeta[t,]) / x$sigma2 + B %*% D + Sigma.U0.inv
    m <- x$z0[t,] / x$sigma2 + B %*% m2 + Sigma.U0.inv %*% mu.Y[1,]
    
    new.y[t,] <- rmvnorm.cond.precision(m, P)

    G <- Sigma.U.inv + B %*% D
    
    for (t in 2:(N-1)) {
  
        m1 <- rhos[t-1,] + phi * new.y[t-1,]
        m2 <- new.y[t+1,] - rhos[t,]

        P <- diag(x$zeta[t,]) / x$sigma2 + G
        m <- x$z0[t,] / x$sigma2 + Sigma.U.inv %*% m1 + B %*% m2

        new.y[t,] <- rmvnorm.cond.precision(m, P)
    }

    t <- N

    m1 <- rhos[t-1,] + phi * new.y[t-1,]

    P <- diag(x$zeta[t,]) / x$sigma2 + Sigma.U.inv
    m <- x$z0[t,] / x$sigma2 + Sigma.U.inv %*% m1
    
    new.y[t,] <- rmvnorm.cond.precision(m, P)
    
    update(x, "y", new.y)
}



update.omega <- function (x) {

    n.omega <- ncol(x$omega)
    nlocs <- nrow(x$omega)
    
    phi <- star1.eta.to.phi(x$eta)

    N <- nrow(x$y)
    Sigma.inv <- chol2inv(x$R.U.chol) / x$U.tau2

    Sigma0.inv <- star1.Sigma0.inv(phi, Sigma.inv)

    new.omega <- x$omega

    for (k in 1:n.omega) {
        
        mu          <- rep(x$omega.mu[k], nlocs)
        V.omega.inv <- chol2inv(x$R.omega.chol[[k]]) / x$omega.tau2[k]
        
        nu.k <- star1.mu(cbind(x$X.mu[,-k]), cbind(new.omega[,-k]))

        aa <- t(x$y - nu.k)

        ## t = 1
        Dkt <- x$X.mu[1,k]
        Ckt <- aa[,1]

        B <- Dkt * Sigma0.inv

        P <- V.omega.inv + Dkt * t(B)
        m <- crossprod(V.omega.inv, mu) + B %*% Ckt

        for (t in 2:N) {
            
            Dkt <- x$X.mu[t,k] - x$X.mu[t-1,k] * phi
            Ckt <- aa[,t] - phi * aa[,t-1]
            
            B <- Dkt * Sigma.inv
            
            P <- P + Dkt * t(B)
            m <- m + B %*% Ckt
        }

        new.omega[,k] <- rmvnorm.cond.precision(m, P)        
        x$mu.Y <- star1.mu(x$X.mu, new.omega)
    }

    update(x, "omega", new.omega)
}


