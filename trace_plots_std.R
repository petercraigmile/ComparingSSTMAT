
mcmc.eta <- chs["eta"]
mcmc.phi <- star1.eta.to.phi(mcmc.eta)
mcmc.omega <- chs["omega"]

pdf(file=paste("trace_plots/trace_plots_", chs$model.name, ".pdf", sep=""),
    width=12, height=10)
par(mfrow=c(5,4), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

#GSP.image(eta.sim)
#GSP.image(eta0)
#GSP.image(rowMeans(mcmc.eta))

plot(chs, "eta.mu")
plot(chs, "eta.tau2")
plot(chs, "eta.lambda")

plot(chs["eta.tau2"]/chs["eta.lambda"], type="l")

plot(chs, "U.tau2")

plot(chs, "U.lambda")

plot(chs["U.tau2"]/chs["U.lambda"], type="l")

matplot(t(sapply(chs$omega.beta.chain, unlist)), type="l", lty=1)

#sapply(chs$omega.beta.chain, unlist))

#print(round(apply(sapply(chs$omega.beta.chain, unlist), 1, function (x) c(mean(x), quantile(x, 0.025), quantile(x, 0.975))), 3))

#plot(chs, "omega.beta")

plot(chs, "omega.tau2")

plot(chs, "omega.lambda")

for (k in 1:ncol(chs$omega)) {
    plot(chs["omega.tau2"][k,]/chs["omega.lambda"][k,], type="l")
}

plot(chs, "eta.jumps")

plot(chs, "sigma2")


matplot(t(mcmc.eta), type="l", lty=1)

matplot(t(mcmc.omega[,1,]), type="l", lty=1)
matplot(t(mcmc.omega[,2,]), type="l", lty=1)

##plot(chs, "y.jumps")


par(mfrow=c(8,7), cex=0.75, mar=c(2,2,0.5,0.5), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(chs$eta)) {

    plot(mcmc.eta[j,], type="l", xlab="", ylab="", col="gray", lwd=2)
}

if (FALSE) {
par(mfrow=c(8,8), cex=0.75, mar=c(2,2,0.5,0.5), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:ncol(chs$omega)) {
    for (j in 1:nrow(chs$omega)) {
        
        plot(mcmc.omega[j,k,], type="l", xlab="", ylab="", col="gray",
             ylim=c(0,3))
     }
}

mcmc.y <- chs["y"]


par(mfrow=c(8,8), cex=0.75, mar=rep(0.5, 4), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(chs$eta)) {

    rand.t <- sample(1:dim(mcmc.y)[1], 1)
    
    plot(mcmc.y[rand.t,j,], type="l", lty=1, xlab="", ylab="", col="gray",
                  ylim=range(z))
}


par(mfrow=c(8,8), cex=0.75, mar=rep(0.5, 4), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(chs$eta)) {
    
    rand.t <- which(!not.missing)[1:8]

    for (k in rand.t) {

        plot(mcmc.y[k,j,], type="l", lty=1, xlab="", ylab="", col="gray",
             ylim=range(z))
        add.guide(y.T[k,j])
    }
}
}


par(mfrow=c(1,1), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(buoy.sites, type="n")
text(buoy.sites[,1], buoy.sites[,2], round(rowMeans(mcmc.phi), 2))


par(mfrow=c(2,1), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:length(chs$omega.lambda)) {
    
    L <- apply(mcmc.omega[,k,], 1, quantile, p=0.025)
    U <- apply(mcmc.omega[,k,], 1, quantile, p=0.975)
    
    CI <- paste(round(L, 3), round(U, 3))
    
    plot(buoy.sites, type="n", ylim=c(-10, 10))
    text(buoy.sites[,1], buoy.sites[,2], round(rowMeans(mcmc.omega[,k,]), 3))
    text(buoy.sites[,1], buoy.sites[,2]-1, CI, cex=1,
         col=ifelse(L>0, "red", ifelse(U<0, "blue", "gray")))

}


if (FALSE) {
    
omegas <- chs["omega"]

As <- sqrt(omegas[,3,]^2 + omegas[,4,]^2)

phases <- atan(-omegas[,3,] / omegas[,4,])


L <- apply(As, 1, quantile, p=0.025)
U <- apply(As, 1, quantile, p=0.975)

CI <- paste(round(L, 3), round(U, 3))

plot(buoy.sites, type="n", ylim=c(-10, 10))
text(buoy.sites[,1], buoy.sites[,2], round(rowMeans(As), 3))
text(buoy.sites[,1], buoy.sites[,2]-1, CI, cex=1,
     col=ifelse(L>0, "red", ifelse(U<0, "blue", "gray")))


L <- apply(phases, 1, quantile, p=0.025)
U <- apply(phases, 1, quantile, p=0.975)

CI <- paste(round(L, 3), round(U, 3))

plot(buoy.sites, type="n", ylim=c(-10, 10))
text(buoy.sites[,1], buoy.sites[,2], round(rowMeans(phases), 3))
text(buoy.sites[,1], buoy.sites[,2]-1, CI, cex=1,
     col=ifelse(L>0, "red", ifelse(U<0, "blue", "gray")))
}

     
dev.off()



#print(round(mean(chs["eta.jumps"])*100, 1))

cat("\n eta lambda\n")
print(round(mean(chs["eta.lambda.jumps"])*100, 1))

cat("\n eta\n")
print(round(mean(chs["eta.jumps"])*100, 1))

cat("\n U lambda\n")
print(round(mean(chs["U.lambda.jumps"])*100, 1))

cat("\n omega lambda\n")
print(round(mean(chs["omega.lambda.jumps"])*100, 1))
