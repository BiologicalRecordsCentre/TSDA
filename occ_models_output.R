# OCC_MODELS_OUTPUT.R

# THIS EXAMINES THE OUTPUT OF THE OCCUPANCY MODELS:

rm(list=ls())

library(reshape2)
library(snow)
library(R2jags)
library(R2OpenBUGS)
library(jagsUI)
library(ggplot2)
load.module("dic")

# Load in the bugs data object:
#load("Results/Cluster/R1_nits100000/Dynamic_Multisp_v3_1_100000.RData")
#load("Results/Cluster/R1_nits35000_v2/Dynamic_Multisp_v3_1_35000.RData")
#load("Results/Cluster/R1_nits15000_its5000/output_extend_R1_15000_nits_5000_na_1000.RData")
load("Results/Cluster/model4/R1_nits5000/Dynamic_Multisp_v4_1_5000.RData")
#out <- new_out

summary(out$summary)
out$summary

sims.array <- as.data.frame(out$sims.list)
sims.array$chain <- melt(sapply(1:3, rep, times=out$mcmc.info$n.samples/3))$value
sims.array$it <- rep(1:(out$mcmc.info$n.samples/3), times=3)
sims.array <- melt(sims.array, id=c('it', 'chain'))

gp <- ggplot(data=sims.array, aes(x=it, y=value, col=factor(chain))) +
  facet_wrap(~variable, scales='free') + geom_line() + theme_bw()

#ggsave(gp, file = "Results/Cluster/R1_nits100000/traceplot_R1_nits100000.png",
#ggsave(gp, file = "Results/Cluster/R1_nits35000_v2/traceplot_R1_nits35000.png",
#ggsave(gp, file = "Results/Cluster/R1_nits15000_its5000/traceplot_R1_nits15000_its5000.png",
ggsave(gp, file = "Results/Cluster/model4/R1_nits50/traceplot_R1_nits50.png",
       height=210, width=350, units='mm', type='cairo')


# NOW GENERATE THE ITERATIONS FOR THE SPECIES-SPECIFIC ANNUAL OCCUPANCY VALUES:

# Harlequin arrived in 2003 (year 14), but only at two monads
# so may be better to consider 2004 (year 15), or median year of arrival
# (2010; year 21).
start_year <- 1990
end_year <- 2016
Harl_year <- 2004
nyear <- length(start_year:end_year)
nspecies <- 8
nits <- 7500 # no. of iterations from the MCMC chains
Hyear <- length(Harl_year:end_year)
Hyear1 <- Harl_year - start_year + 1
# Inverse logit function:
inv.logit <- function(x) exp(x)/(1+exp(x)) 




muZ_un = array(NA, dim = c(nits, nspecies, nyear))
muZ_H = array(NA, dim = c(nits, nspecies, nyear))


muZ_un[,,1] = out$sims.list$init.occ

###### This next step no longer works as the output parameters have changed ######
###### and also not sure if the year random effect on persistance is now incorporated #############

for(t in 2:nyear){
  muZ_un[,,t] = muZ_un[,,t-1] * inv.logit(out$sims.list$beta0.phi) + (1-muZ_un[,,t-1]) * out$sims.list$gamma
} # muZ_un

# for a site where harlequin arrived, there is no effect in the first year
# so occupancy is identical prior to this
muZ_H[,,1:Hyear1] <- muZ_un[,,1:Hyear1]
  
for(t in (Hyear1+1):nyear){
 muZ_H[,,t] = muZ_H[,,t-1]  * inv.logit(out$sims.list$beta0.phi + out$sims.list$beta1) + 
          (1-muZ_H[,,t-1]) * out$sims.list$gamma
}  # muZ_H
        



# Now extract the median and lower and upper 95% credible intervals for:
# 1. muZ_un - all species
# 2. muZ_H - all species

low <- function(x) quantile(x, prob=0.025, na.rm=TRUE)
up <- function(x) quantile(x, prob=0.975, na.rm=TRUE)

# 1. Harlequin absent:
median_un = matrix(NA, nrow=nyear, ncol=nspecies)
lower_un = matrix(NA, nrow=nyear, ncol=nspecies)
upper_un = matrix(NA, nrow=nyear, ncol=nspecies)
mean_un = matrix(NA, nrow=nyear, ncol=nspecies)


for(t in 1:nyear){
  for(i in 1:nspecies){
median_un[t,i] <- median(muZ_un[,i,t])
lower_un[t,i] <- low(muZ_un[,i,t])
upper_un[t,i] <- up(muZ_un[,i,t])
mean_un[t,i] <- mean(muZ_un[,i,t])
  }
}

# 2. Harlequin present:
median_H = matrix(NA, nrow=nyear, ncol=nspecies)
lower_H = matrix(NA, nrow=nyear, ncol=nspecies)
upper_H = matrix(NA, nrow=nyear, ncol=nspecies)
mean_H = matrix(NA, nrow=nyear, ncol=nspecies)


for(t in 1:nyear){
  for(i in 1:nspecies){
    median_H[t,i] <- median(muZ_H[,i,t])
    lower_H[t,i] <- low(muZ_H[,i,t])
    upper_H[t,i] <- up(muZ_H[,i,t])
    mean_H[t,i] <- mean(muZ_H[,i,t])
  }
}


# Now start plotting the median values over time:
plot(NA,
     ylim = c(0,1), 
     xlim = c(start_year, end_year),
     ylab = "Probability of occupancy",
     xlab = "Year",
     lwd=3, lty=1)
lines(start_year:end_year, median_un[,1], lwd=3)
lines(start_year:end_year, median_H[,1], lty=2, lwd=3)

lab = c("Adalia bipunctata", 
        "Adalia decempunctata", 
        "Calvia quattuordecimguttata", 
        "Coccinella septempunctata", 
        "Exochomus quadripustulatus", 
        "Halyzia sedecimguttata",
        "Propylea quattuordecimpunctata", 
        "Psyllobora vigintiduopunctata")

#tiff("Results/Probability occupancy by species.tiff", height=1000, width=1000)
par(mfrow=c(2,4))
for(i in 1:nspecies){
plot(NA,
     ylim = c(0,1), 
     xlim = c(start_year, end_year),
     ylab = "Probability of occupancy",
     xlab = "Year",
     main = lab[i],
     lwd=3, lty=1)
lines(start_year:end_year, median_un[,i], lwd=3)
lines(start_year:end_year, median_H[,i], lwd=3, lty=2)
}
#dev.off()

