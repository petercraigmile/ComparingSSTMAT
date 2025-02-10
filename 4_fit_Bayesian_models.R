
ITERS <- 5000

## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.model1 <- init.star1.Bayesian.model(model.name = "buoy_model1_2024_12_04",
                                        zmat  = t(buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us, sin.term, cos.term),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.model1,
     file=paste("~/Desktop/chains_", buoy.model1$model.name, ".RData", sep=""))    

chs <- buoy.model1
source("trace_plots.R")

for (k in 1:19) {
    
    run.MCMC(buoy.model1, to.upd, ITERS, thin=10, every=1000)
    
    chs <- buoy.model1
    source("trace_plots.R")
    
    save(buoy.model1,
         file=paste("~/Desktop/chains_", buoy.model1$model.name, ".RData", sep=""))    
}

## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.model2 <- init.star1.Bayesian.model(model.name = "buoy_model2_2024_12_04",
                                        zmat  = t(buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us, sin.term, cos.term),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.model2, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.model2, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.model2,
     file=paste("~/Desktop/chains_", buoy.model2$model.name, ".RData", sep=""))    

chs <- buoy.model2
source("trace_plots.R")

for (k in 1:19) {
    
    run.MCMC(buoy.model2, to.upd, ITERS, thin=10, every=1000)

    chs <- buoy.model2
    source("trace_plots.R")
    
    save(buoy.model2,
         file=paste("~/Desktop/chains_", buoy.model2$model.name, ".RData", sep=""))    
}

## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

ERA.model1 <- init.star1.Bayesian.model(model.name = "ERA_model1_2024_12_04",
                                        zmat  = t(ERA.SST.minus.MAT),
                                        X.mu  = cbind(1, us, sin.term, cos.term),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.model1,
     file=paste("~/Desktop/chains_", ERA.model1$model.name, ".RData", sep=""))    

chs <- ERA.model1
source("trace_plots.R")

for (k in 1:19) {
    
    run.MCMC(ERA.model1, to.upd, ITERS, thin=10, every=1000)

    chs <- ERA.model1
    source("trace_plots.R")
    
    save(ERA.model1,
         file=paste("~/Desktop/chains_", ERA.model1$model.name, ".RData", sep=""))
}


## ======================================================================


source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

ERA.model2 <- init.star1.Bayesian.model(model.name = "ERA_model2_2024_12_04",
                                        zmat  = t(ERA.SST.minus.MAT),
                                        X.mu  = cbind(1, us, sin.term, cos.term),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.model2, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.model2, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.model2, file=paste("~/Desktop/chains_",
     ERA.model2$model.name, ".RData", sep=""))

chs <- ERA.model2
source("trace_plots.R")

for (k in 1:19) {
    
    run.MCMC(ERA.model2, to.upd, ITERS, thin=10, every=1000)

    chs <- ERA.model2
    source("trace_plots.R")
    
    save(ERA.model2, file=paste("~/Desktop/chains_",
         ERA.model2$model.name, ".RData", sep=""))

}

## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.anom.model1 <- init.star1.Bayesian.model(model.name = "buoy_anom_model1_2024_12_04",
                                        zmat  = t(ds.buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.anom.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.anom.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.anom.model1,
     file=paste("~/Desktop/chains_", buoy.anom.model1$model.name, ".RData", sep=""))    

chs <-  buoy.anom.model1
source("trace_plots_anom.R")

for (k in 1:19) {
    
    run.MCMC(buoy.anom.model1, to.upd, ITERS, thin=10, every=1000)

    chs <-  buoy.anom.model1
    source("trace_plots_anom.R")
    
    save(buoy.anom.model1,
         file=paste("~/Desktop/chains_", buoy.anom.model1$model.name, ".RData", sep=""))    
}

## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.anom.model2 <- init.star1.Bayesian.model(model.name = "buoy_anom_model2_2024_12_04",
                                        zmat  = t(ds.buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.anom.model2, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.anom.model2, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.anom.model2,
     file=paste("~/Desktop/chains_", buoy.anom.model2$model.name, ".RData", sep=""))    

chs <-  buoy.anom.model2
source("trace_plots_anom.R")


for (k in 1:19) {
    
    run.MCMC(buoy.anom.model2, to.upd, ITERS, thin=10, every=1000)

    chs <-  buoy.anom.model2
    source("trace_plots_anom.R")
    
    save(buoy.anom.model2,
         file=paste("~/Desktop/chains_", buoy.anom.model2$model.name, ".RData", sep=""))    
}

## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

ERA.anom.model1 <- init.star1.Bayesian.model(model.name = "ERA_anom_model1_2024_12_04",
                                        zmat  = t(ds.ERA.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.anom.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.anom.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.anom.model1,
     file=paste("~/Desktop/chains_", ERA.anom.model1$model.name, ".RData", sep=""))    

chs <-  ERA.anom.model1
source("trace_plots_anom.R")

for (k in 1:19) {
    
    run.MCMC(ERA.anom.model1, to.upd, ITERS, thin=10, every=1000)

    chs <-  ERA.anom.model1
    source("trace_plots_anom.R")
    
    save(ERA.anom.model1,
         file=paste("~/Desktop/chains_", ERA.anom.model1$model.name, ".RData", sep=""))    
}


## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

ERA.anom.model2 <- init.star1.Bayesian.model(model.name = "ERA_anom_model2_2024_12_04",
                                        zmat  = t(ds.ERA.SST.minus.MAT),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.anom.model2, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.anom.model2, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.anom.model2,
     file=paste("~/Desktop/chains_", ERA.anom.model2$model.name, ".RData", sep=""))    

chs <-  ERA.anom.model2
source("trace_plots_anom.R")

for (k in 1:19) {
    
    run.MCMC(ERA.anom.model2, to.upd, ITERS, thin=10, every=1000)
    
    chs <-  ERA.anom.model2
    source("trace_plots_anom.R")
    
    save(ERA.anom.model2,
         file=paste("~/Desktop/chains_", ERA.anom.model2$model.name, ".RData", sep=""))    
}




## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

## Copy the missing data pattern from the buoy data into the ERA anoms
z <- ds.ERA.SST.minus.MAT

for (k in 1:nrow(z)) {

    z[k,is.na(ds.buoy.SST.minus.MAT[k,])] <- NA
}

ERA.anom.miss.model1 <- init.star1.Bayesian.model(model.name = "ERA_anom_miss_model1_2024_12_04",
                                        zmat  = t(z),
                                        X.mu  = cbind(1, us),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(ERA.anom.miss.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(ERA.anom.miss.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(ERA.anom.miss.model1,
     file=paste("~/Desktop/chains_", ERA.anom.miss.model1$model.name, ".RData", sep=""))    

chs <-  ERA.anom.miss.model1
source("trace_plots_anom.R")

for (k in 1:19) {
    
    run.MCMC(ERA.anom.miss.model1, to.upd, ITERS, thin=10, every=1000)

    chs <-  ERA.anom.miss.model1
    source("trace_plots_anom.R")
    
    save(ERA.anom.miss.model1,
         file=paste("~/Desktop/chains_", ERA.anom.miss.model1$model.name, ".RData", sep=""))    
}


## ======================================================================

source("init.R")
source("read_data.R")
source("Bayesian_updates.R")

buoy.anom.seas.model1 <- init.star1.Bayesian.model(model.name = "buoy_anom_seas_model1_2024_12_05",
                                        zmat  = t(ds.buoy.SST.minus.MAT),
                                        X.mu  = cbind(1, us, sin.term, cos.term),
                                        X.eta = cbind(rep(1, nrow(D))),
                                        eta.block.length = 9)

to.upd <- c("sigma2", "eta", "eta.pars", "omega.pars", "omega", "U.pars", "y")

run.MCMC(buoy.anom.seas.model1, to.upd, ITERS, every=1000, burn.in=TRUE)

run.MCMC(buoy.anom.seas.model1, to.upd, ITERS, thin=10, every=1000, burn.in=FALSE)

save(buoy.anom.seas.model1,
     file=paste("~/Desktop/chains_", buoy.anom.seas.model1$model.name, ".RData", sep=""))    

chs <- buoy.anom.seas.model1
source("trace_plots.R")

for (k in 1:19) {
    
    run.MCMC(buoy.anom.seas.model1, to.upd, ITERS, thin=10, every=1000)
    
    chs <- buoy.anom.seas.model1
    source("trace_plots.R")
    
    save(buoy.anom.seas.model1,
         file=paste("~/Desktop/chains_", buoy.anom.seas.model1$model.name, ".RData", sep=""))    
}

## ======================================================================
