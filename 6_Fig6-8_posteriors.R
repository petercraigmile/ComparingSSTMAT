
library(scales)
library(excursions)

source("init.R")
source("read_data.R")

source("functions/paper_functions.R")
source("functions/calculate_posterior_summaries.R")

path <- "~/Desktop/"

load(file.path(path, "chains_buoy_model1_2024_12_04.RData"))
load(file.path(path, "chains_buoy_model2_2024_12_04.RData"))

load(file.path(path, "chains_ERA_model1_2024_12_04.RData"))
load(file.path(path, "chains_ERA_model2_2024_12_04.RData"))

load(file.path(path, "chains_buoy_anom_model1_2024_12_04.RData"))
load(file.path(path, "chains_buoy_anom_model2_2024_12_04.RData"))

load(file.path(path, "chains_ERA_anom_model1_2024_12_04.RData"))
load(file.path(path, "chains_ERA_anom_model2_2024_12_04.RData"))

load(file.path(path, "chains_ERA_anom_miss_model1_2024_12_04.RData"))




## ======================================================================
## Figures 6 and 7
## ======================================================================

buoy.phi.post <- star1.eta.to.phi(cbind(buoy.model1["eta"],
                                       buoy.model2["eta"]))                                
buoy.phi <- summarize.posteriors(buoy.phi.post)

ERA.phi.post <- star1.eta.to.phi(cbind(ERA.model1["eta"],
                                       ERA.model2["eta"]))


buoy.tau.post <- sqrt(c(buoy.model1["U.tau2"], buoy.model2["U.tau2"]))

buoy.phi <- summarize.posteriors(buoy.phi.post)

ERA.tau.post <- sqrt(c(ERA.model1["U.tau2"], ERA.model2["U.tau2"]))

ERA.phi <- summarize.posteriors(ERA.phi.post)

buoy.sd.post <- 1 / sqrt(- buoy.phi.post+1) * buoy.tau.post

ERA.sd.post <- 1 / sqrt(- ERA.phi.post+1) * ERA.tau.post


buoy.sd <- summarize.posteriors(buoy.sd.post)
ERA.sd <- summarize.posteriors(ERA.sd.post)


buoy.beta0 <- summarize.posteriors( cbind(buoy.model1["omega"][,1,],
                                          buoy.model2["omega"][,1,]) )

ERA.beta0 <- summarize.posteriors( cbind(ERA.model1["omega"][,1,],
                                          ERA.model2["omega"][,1,]) )

buoy.beta1 <- summarize.posteriors( cbind(buoy.model1["omega"][,2,],
                                          buoy.model2["omega"][,2,]) )

ERA.beta1 <- summarize.posteriors( cbind(ERA.model1["omega"][,2,],
                                          ERA.model2["omega"][,2,]) )

post.buoy.beta1 <- cbind(buoy.model1["omega"][,2,],
                         buoy.model2["omega"][,2,])

simconf.mc(post.buoy.beta1, 0.05)



buoy.beta.sin <- summarize.posteriors( cbind(buoy.model1["omega"][,3,],
                                          buoy.model2["omega"][,3,]) )

ERA.beta.sin <- summarize.posteriors( cbind(ERA.model1["omega"][,3,],
                                          ERA.model2["omega"][,3,]) )

buoy.beta.cos <- summarize.posteriors( cbind(buoy.model1["omega"][,4,],
                                          buoy.model2["omega"][,4,]) )

ERA.beta.cos <- summarize.posteriors( cbind(ERA.model1["omega"][,4,],
                                          ERA.model2["omega"][,4,]) )



buoy.sin <- cbind(buoy.model1["omega"][,3,], buoy.model2["omega"][,3,]) 
buoy.cos <- cbind(buoy.model1["omega"][,4,], buoy.model2["omega"][,4,])

buoy.A <- sqrt(buoy.sin^2 + buoy.cos^2)
buoy.phase <- atan(-buoy.sin/buoy.cos)

ERA.sin <- cbind(ERA.model1["omega"][,3,], ERA.model2["omega"][,3,]) 
ERA.cos <- cbind(ERA.model1["omega"][,4,], ERA.model2["omega"][,4,])

ERA.A <- sqrt(ERA.sin^2 + ERA.cos^2)
ERA.phase <- atan(-ERA.sin/ERA.cos)

buoy.beta.A <- summarize.posteriors(buoy.A)
ERA.beta.A <- summarize.posteriors(ERA.A)

buoy.beta.phase <- summarize.posteriors(buoy.phase)
ERA.beta.phase <- summarize.posteriors(ERA.phase)


##

buoy.anom.phi.post <- star1.eta.to.phi(cbind(buoy.anom.model1["eta"],
                                       buoy.anom.model2["eta"]))                                
buoy.anom.phi <- summarize.posteriors(buoy.anom.phi.post)

ERA.anom.phi.post <- star1.eta.to.phi(cbind(ERA.anom.model1["eta"],
                                       ERA.anom.model2["eta"]))

ERA.anom.anom.phi <- summarize.posteriors( star1.eta.to.phi(cbind(ERA.anom.model1["eta"],
                                                                  ERA.anom.model2["eta"]) ))

buoy.anom.beta0 <- summarize.posteriors( cbind(buoy.anom.model1["omega"][,1,],
                                               buoy.anom.model2["omega"][,1,]) )

ERA.anom.beta0 <- summarize.posteriors( cbind(ERA.anom.model1["omega"][,1,],
                                              ERA.anom.model2["omega"][,1,]) )

buoy.anom.beta1 <- summarize.posteriors( cbind(buoy.anom.model1["omega"][,2,],
                                               buoy.anom.model2["omega"][,2,]) )

ERA.anom.beta1 <- summarize.posteriors( cbind(ERA.anom.model1["omega"][,2,],
                                              ERA.anom.model2["omega"][,2,]) )


buoy.anom.tau.post <- sqrt(c(buoy.anom.model1["U.tau2"], buoy.anom.model2["U.tau2"]))

buoy.anom.phi <- summarize.posteriors(buoy.anom.phi.post)

ERA.anom.tau.post <- sqrt(c(ERA.anom.model1["U.tau2"], ERA.anom.model2["U.tau2"]))

ERA.anom.phi <- summarize.posteriors(ERA.anom.phi.post)

buoy.anom.sd.post <- 1 / sqrt(- buoy.anom.phi.post+1) * buoy.anom.tau.post

ERA.anom.sd.post <- 1 / sqrt(- ERA.anom.phi.post+1) * ERA.anom.tau.post


buoy.anom.sd <- summarize.posteriors(buoy.anom.sd.post)
ERA.anom.sd <- summarize.posteriors(ERA.anom.sd.post)


##


ERA.anom.miss.phi <- summarize.posteriors( star1.eta.to.phi(ERA.anom.miss.model1["eta"] ))
ERA.anom.miss.beta0 <- summarize.posteriors( ERA.anom.miss.model1["omega"][,1,])
ERA.anom.miss.beta1 <- summarize.posteriors( ERA.anom.miss.model1["omega"][,2,])




posterior.map.plot <- function (post.summ, label, scale=1) {

    show.map(country.labels=FALSE)
    
    grid.text(fmt(post.summ$post.mean*scale, digits=2),
              col=ifelse(post.summ$includes.zero, "gray40", "blue"))
    
    mtitle(label, col=SST.col)
}


pdf(file="figures/Fig6_Bayes_posterior_means.pdf", width=7, height=7)
par(mfrow=c(4,2), cex=0.4, mar=c(3,3,1.5,0.5), mgp=c(1.6,0.5,0), bty="l")

posterior.map.plot(buoy.beta0, "TAO Posterior intercept")
posterior.map.plot(ERA.beta0, "ERA5 Posterior intercept")

posterior.map.plot(buoy.beta1, "TAO Posterior change per decade", scale=10)
posterior.map.plot(ERA.beta1, "ERA5 Posterior change per decade", scale=10)

posterior.map.plot(buoy.phi, "TAO Posterior AR(1) parameter")
posterior.map.plot(ERA.phi, "ERA5 Posterior AR(1) parameter")

posterior.map.plot(buoy.sd, "TAO Posterior latent process SD")
posterior.map.plot(ERA.sd, "ERA5 Posterior latent process SD")

dev.off()


pdf(file="figures/Fig7_Bayes_posterior_means_seasonal.pdf", width=7, height=7)
par(mfrow=c(4,2), cex=0.4, mar=c(3,3,1.5,0.5), mgp=c(1.6,0.5,0), bty="l")

posterior.map.plot(buoy.anom.beta0, "TAO Posterior intercept")
posterior.map.plot(ERA.anom.beta0, "ERA5 Posterior intercept")

posterior.map.plot(buoy.anom.beta1, "TAO Posterior change per decade", scale=10)
posterior.map.plot(ERA.anom.beta1, "ERA5 Posterior change per decade", scale=10)

posterior.map.plot(buoy.anom.phi, "TAO Posterior AR(1) parameter")
posterior.map.plot(ERA.anom.phi, "ERA5 Posterior AR(1) parameter")

posterior.map.plot(buoy.anom.sd, "TAO Posterior latent process SD")
posterior.map.plot(ERA.anom.sd, "ERA5 Posterior latent process SD")

dev.off()



## ======================================================================
## Figure 8
## ======================================================================

buoy.sin <- cbind(buoy.model1["omega"][,3,], buoy.model2["omega"][,3,]) 
buoy.cos <- cbind(buoy.model1["omega"][,4,], buoy.model2["omega"][,4,])

ERA.sin <- cbind(ERA.model1["omega"][,3,], ERA.model2["omega"][,3,]) 
ERA.cos <- cbind(ERA.model1["omega"][,4,], ERA.model2["omega"][,4,])


cylim2 <- c(-0.8, 0.8)

pdf(file="figures/Fig8_seasonal.pdf", width=7, height=6)
par(mfrow=c(7,8), cex=0.45, mar=c(1.5,1.5,1,0.2), oma=c(1.4,1.4,0,0), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(buoy.grid.mapping)) {
    
    gj <- buoy.grid.mapping[j]
   
    if (handle.empty.plot(gj, buoy.ERA.label=TRUE)) {

        buoy.seasonal <- outer(sin.term[1:12], buoy.sin[gj,]) + outer(cos.term[1:12], buoy.cos[gj,])
        ERA.seasonal <- outer(sin.term[1:12], ERA.sin[gj,]) + outer(cos.term[1:12], ERA.cos[gj,])

        buoy.simult <- summarize.posteriors(buoy.seasonal)            
        ERA.simult  <- summarize.posteriors(ERA.seasonal)
        
        plot(1:12, rowMeans(ERA.seasonal),
             type="l", xlab="", ylab="", ylim=cylim2, col="gray40", xaxt="n")

        axis(side=1, at=c(1,4,7,10), labels=substr(month.name[c(1,4,7,10)], 1, 3))
        
        polygon(c(1:12, 12:1),
                c(ERA.simult$P025, rev(ERA.simult$P975)),
                col=alpha("gray60", 0.8),
                border=NA)

        lines(1:12, rowMeans(buoy.seasonal), col="blue")
        polygon(c(1:12, 12:1),
                c(buoy.simult$P025, rev(buoy.simult$P975)),
                col=alpha("blue", 0.6),
                border=NA)
        
        axis(side=1, at=seq(1996, 2016, 10), labels=paste(seq(1996, 2016, 10)), cex=0.45)
    }
    
}

grid.outside.labels()

dev.off()


