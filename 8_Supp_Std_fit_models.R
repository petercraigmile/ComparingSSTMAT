
## Standardized models
## ======================================================================

ITERS <- 5000

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.std.model1 <- init.star1.Bayesian.model(model.name = "buoy_std_model1_2025_04_30",
                                        zmat  = t(std.buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.std.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.std.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.std.model1,
     file=paste("~/Desktop/chains_", buoy.std.model1$model.name, ".RData", sep=""))    

chs <-  buoy.std.model1
source("trace_plots_std.R")

for (k in 2:19) {
    
    run.MCMC(buoy.std.model1, to.upd, ITERS, thin=10, every=1000)

    chs <-  buoy.std.model1
    source("trace_plots_std.R")
    
    save(buoy.std.model1,
         file=paste("~/Desktop/chains_", buoy.std.model1$model.name, ".RData", sep=""))    
}

## ======================================================================

ITERS <- 5000

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.std.model2 <- init.star1.Bayesian.model(model.name = "buoy_std_model2_2025_04_30",
                                        zmat  = t(std.buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.std.model2, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.std.model2, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.std.model2,
     file=paste("~/Desktop/chains_", buoy.std.model2$model.name, ".RData", sep=""))    

chs <-  buoy.std.model2
source("trace_plots_std.R")


for (k in 1:19) {
    
    run.MCMC(buoy.std.model2, to.upd, ITERS, thin=10, every=1000)

    chs <-  buoy.std.model2
    source("trace_plots_std.R")
    
    save(buoy.std.model2,
         file=paste("~/Desktop/chains_", buoy.std.model2$model.name, ".RData", sep=""))    
}

## ======================================================================

ITERS <- 5000

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

ERA.std.model1 <- init.star1.Bayesian.model(model.name = "ERA_std_model1_2025_04_30",
                                        zmat  = t(std.ERA.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.std.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.std.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.std.model1,
     file=paste("~/Desktop/chains_", ERA.std.model1$model.name, ".RData", sep=""))    

chs <-  ERA.std.model1
source("trace_plots_std.R")

for (k in 2:19) {
    
    run.MCMC(ERA.std.model1, to.upd, ITERS, thin=10, every=1000)

    chs <-  ERA.std.model1
    source("trace_plots_std.R")
    
    save(ERA.std.model1,
         file=paste("~/Desktop/chains_", ERA.std.model1$model.name, ".RData", sep=""))    
}


## ======================================================================

ITERS <- 5000

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

ERA.std.model2 <- init.star1.Bayesian.model(model.name = "ERA_std_model2_2025_04_30",
                                        zmat  = t(std.ERA.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.std.model2, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.std.model2, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.std.model2,
     file=paste("~/Desktop/chains_", ERA.std.model2$model.name, ".RData", sep=""))    

chs <-  ERA.std.model2
source("trace_plots_std.R")

for (k in 1:19) {
    
    run.MCMC(ERA.std.model2, to.upd, ITERS, thin=10, every=1000)
    
    chs <-  ERA.std.model2
    source("trace_plots_std.R")
    
    save(ERA.std.model2,
         file=paste("~/Desktop/chains_", ERA.std.model2$model.name, ".RData", sep=""))    
}

