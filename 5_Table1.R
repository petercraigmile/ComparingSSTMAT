
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


fname <- "tables/Table1_posterior_table.tex"

cat("", file=fname)

posterior.line("$\\sigma$", "sigma2", fname, transform=sqrt)

hline(fname)

posterior.line("$\\mu_{\\eta}$", "eta.mu", fname)
posterior.line("$\\tau_{\\eta}$", "eta.tau2", fname, sqrt)
posterior.line("$\\lambda_{\\eta}$", "eta.lambda", fname)

hline(fname)

posterior.line("$\\tau_{\\zeta}$", "U.tau2", fname, sqrt)
posterior.line("$\\lambda_{\\zeta}$", "U.lambda", fname)

hline(fname)

for (k in 1:2) {

posterior.line(paste("$\\mu_{\\beta_", k, "}$", sep=""), "omega.mu", fname, index=k)
posterior.line(paste("$\\tau_{\\beta_", k, "}$", sep=""), "omega.tau2", fname, sqrt, index=k)
posterior.line(paste("$\\lambda_{\\beta_", k, "}$", sep=""), "omega.lambda", fname, index=k)
hline(fname)
}

for (k in 3:4) {

posterior.line2(paste("$\\mu_{\\beta_", k, "}$", sep=""), "omega.mu", fname, index=k)
posterior.line2(paste("$\\tau_{\\beta_", k, "}$", sep=""), "omega.tau2", fname, sqrt, index=k)
posterior.line2(paste("$\\lambda_{\\beta_", k, "}$", sep=""), "omega.lambda", fname, index=k)
hline(fname)
}
