
library(scales)
library(excursions)

source("init.R")
source("read_data.R")

source("functions/paper_functions.R")
source("functions/calculate_posterior_summaries.R")

path <- "/Users/pfc/OneDrive - The Ohio State University/2024_Comparing_SST_and_SSA_chains"
##path <- "~/Desktop/"

load(file.path(path, "chains_buoy_std_model1_2025_04_30.RData"))
load(file.path(path, "chains_buoy_std_model2_2025_04_30.RData"))

load(file.path(path, "chains_ERA_std_model1_2025_04_30.RData"))
load(file.path(path, "chains_ERA_std_model2_2025_04_30.RData"))



## =====================================================================

buoy.std.phi.post <- star1.eta.to.phi(cbind(buoy.std.model1["eta"],
                                            buoy.std.model2["eta"]))                                
buoy.std.phi <- summarize.posteriors(buoy.std.phi.post)

ERA.std.phi.post <- star1.eta.to.phi(cbind(ERA.std.model1["eta"],
                                           ERA.std.model2["eta"]))

ERA.std.std.phi <- summarize.posteriors( star1.eta.to.phi(cbind(ERA.std.model1["eta"],
                                                                ERA.std.model2["eta"]) ))

buoy.std.beta0 <- summarize.posteriors( cbind(buoy.std.model1["omega"][,1,],
                                              buoy.std.model2["omega"][,1,]) )

ERA.std.beta0 <- summarize.posteriors( cbind(ERA.std.model1["omega"][,1,],
                                             ERA.std.model2["omega"][,1,]) )

buoy.std.beta1 <- summarize.posteriors( cbind(buoy.std.model1["omega"][,2,],
                                              buoy.std.model2["omega"][,2,]) )

ERA.std.beta1 <- summarize.posteriors( cbind(ERA.std.model1["omega"][,2,],
                                             ERA.std.model2["omega"][,2,]) )


buoy.std.tau.post <- sqrt(c(buoy.std.model1["U.tau2"], buoy.std.model2["U.tau2"]))

buoy.std.phi <- summarize.posteriors(buoy.std.phi.post)

ERA.std.tau.post <- sqrt(c(ERA.std.model1["U.tau2"], ERA.std.model2["U.tau2"]))

ERA.std.phi <- summarize.posteriors(ERA.std.phi.post)

buoy.std.sd.post <- 1 / sqrt(- buoy.std.phi.post+1) * buoy.std.tau.post

ERA.std.sd.post <- 1 / sqrt(- ERA.std.phi.post+1) * ERA.std.tau.post


buoy.std.sd <- summarize.posteriors(buoy.std.sd.post)
ERA.std.sd <- summarize.posteriors(ERA.std.sd.post)

## ======================================================================


posterior.map.plot <- function (post.summ, label, scale=1) {

    show.map(country.labels=FALSE)
    
    grid.text(fmt(post.summ$post.mean*scale, digits=2),
              col=ifelse(post.summ$includes.zero, "gray40", "blue"))
    
    mtitle(label, col=SST.col)
}



pdf(file="figures/FigS6_Bayes_posterior_means_seasonal_std.pdf", width=7, height=7)
par(mfrow=c(4,2), cex=0.4, mar=c(3,3,1.5,0.5), mgp=c(1.6,0.5,0), bty="l")

posterior.map.plot(buoy.std.beta0, "TAO Posterior intercept")
posterior.map.plot(ERA.std.beta0, "ERA5 Posterior intercept")

posterior.map.plot(buoy.std.beta1, "TAO Posterior change per decade", scale=10)
posterior.map.plot(ERA.std.beta1, "ERA5 Posterior change per decade", scale=10)

posterior.map.plot(buoy.std.phi, "TAO Posterior AR(1) parameter")
posterior.map.plot(ERA.std.phi, "ERA5 Posterior AR(1) parameter")

posterior.map.plot(buoy.std.sd, "TAO Posterior latent process SD")
posterior.map.plot(ERA.std.sd, "ERA5 Posterior latent process SD")

dev.off()



## Table S1

=================================================

fname <- "tables/TableS1_posterior_std_table.tex"

cat("", file=fname)

posterior.std.line("$\\sigma$", "sigma2", fname, transform=sqrt)

hline(fname)

posterior.std.line("$\\mu_{\\eta}$", "eta.mu", fname)
posterior.std.line("$\\tau_{\\eta}$", "eta.tau2", fname, sqrt)
posterior.std.line("$\\lambda_{\\eta}$", "eta.lambda", fname)

hline(fname)

posterior.std.line("$\\tau_{\\zeta}$", "U.tau2", fname, sqrt)
posterior.std.line("$\\lambda_{\\zeta}$", "U.lambda", fname)

for (k in 1:2) {

 hline(fname)
posterior.std.line(paste("$\\mu_{\\beta_", k, "}$", sep=""), "omega.mu", fname, index=k)
posterior.std.line(paste("$\\tau_{\\beta_", k, "}$", sep=""), "omega.tau2", fname, sqrt, index=k)
posterior.std.line(paste("$\\lambda_{\\beta_", k, "}$", sep=""), "omega.lambda", fname, index=k)
}
