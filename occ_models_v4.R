# OCC_MODELS_V4.R

# THIS SCRIPT IS DESIGNED TO WORK ON THE CIRRUS CLUSTER

# REQUIRED: 1.RDATA FROM PROJECTS DRIVE
#           2.SCRIPTS FROM SOURCE FOLDER ON PROJECTS DRIVE
#           3.BUGS MODEL TXT (This repository)

# V4 REVISES V3 BY:
# 1. USING DYNAMIC_MULTISP_V4.TXT MODEL - ADDS RANDOM EFFECT FOR YEAR IN PERSISTENCY MODEL
# 2. DAISY-CHAIN HAS NOW BEEN ADDED (still requires testing)

# V3 REVISES V2 BY:
# 1. USING THE HALF-CAUCHY DISTRIBUTION ON THE STANDARD DEVIATION, RATHER THAN THE PRECISION 

# V2 REVISES V1 BY:
# 1. USING THE CORRECT HARL OBJECT - HAS THE AHRLEQUIN ARRIVED BY YEAR X (0,1),
# RATHER THAN THE YEAR OF FIRST ARRIVAL.

rm(list=ls())
setwd("/home/scratch/pywell/jachat/ladybird/")
wd <- getwd()


library(reshape2)
library(snow)
library(R2jags)
library(R2OpenBUGS)
library(jagsUI)
load.module("dic")

# Load in the bugs data object:
load("ladybird_180220.RData")

######### The below batch of code is taken from Nick's "run_models_Occ_160414.R" ###########

# define the identifier for this analysis and set up a 
#datecode <- format(Sys.Date(), '%y%m%d')
#results_folder <-paste0(datecode)
#dir.create(results_folder)
results_folder <- "/home/scratch/pywell/jachat/ladybird/model4/R1_its10000"

###################################################  Get Colin's conversion code
source(paste0(wd, "/source/", "reformat_gr.R"))
source('source/gr_components.R')

############################################### load the function containing the call to JAGS
source('source/dynocc.R')

###################################################  functions

LenUniq <- function(x) length(unique(x))
pc.change.fitted<-function(x) as.numeric(100*(x[2]-x[1])/x[1])
ilt<-function(x) exp(x)/(1+exp(x)) #returns p
logit <- function(p) log(p/(1-p)) 
sd2pr <- function(sd) 1/sd^2
pr2sd <- function(pr) sqrt(1/pr)


################################################ define the BUGS model file
model_file <- 'Dynamic_Multisp_v4.txt'


################### bugs settings
#source('settings.r') # defines ni 
#ni<-1e5; nb<-5e4; nt<-10; nc<-3; R=1 # production run OLD
#ni<-1e6; nb<-5e5; nt<-10; nc<-3; R=1 # SF advice OLD
#ni<-1e4; nb<-5e3; nt<-5; nc<-3; R=1 # for running overnight OLD
#ni<-1000; nb<-ni/2; nt<-1; nc<-3; R=10 # for testing ovenight
#ni<-1000; nb<-ni/2; nt<-1; nc<-3; R=50 # 
#ni<-100; nb<-ni/2; nt<-1; nc<-3; R=200 # for quick testing: ~1 min per sp
#ni & R & nt are set externally
nt <- 3
ni <- 10000
R <- 1
nb<-ni/2
nc<-3

# set the global options
Parallelize <- TRUE #
JAGS <- TRUE #F
debug= TRUE #T

################ DEFINE SPECIES
# subset to just a few species (for speed)
sp_cols <- 1:ncol(lady_data$focal) # all 8 species
recs_per_spp <- colSums(lady_data$focal)
#sort(recs_per_spp)
#write.csv(recs_per_spp, file='OccMods/recs_per_spp.csv')
#sp_cols <- 1:4  # test with just 4 species


############################################### Prepare BUGS

parameters <- c("init.occ", "beta0.phi", "beta1", "beta2.p", "gamma",
                'mu.beta0.phi', 'tau.beta0.phi',
                "mu.gamma", 'tau.gamma',
                'mu.beta1','tau.beta1',
                'tau.alpha.p', # precision across years in detectability
                'tau.alpha.phi',
                'mu.beta2.p', 'tau.beta2.p', # mean detectability on a list of length 1 (and precision across species)
                'mu.beta3.p', 'tau.beta3.p', # mean difference of short lists over single speceis lists & precision across species 
                'mu.beta4.p','tau.beta4.p' # mean difference of long lists over single speceis lists & precision across species
               ) 

# define the dataframe: everything from lady_data except the species info (to be added below)

bugs_data <- lady_data[-which(names(lady_data)=='focal')]

# in the multispecies model we are not using other variables too
#bugs_data <- bugs_data[-which(names(bugs_data)=='nsite')]

# finally, add a scale parameter for the half-Cauchy priors
#bugs_data$v.scale <- 1

############################################### which species? - to speed things up

sum(lady_data$focal) # sum of the records per species


# subset by these species
bugs_data$nspecies <- length(sp_cols)
bugs_data$y <- lady_data$focal[,sp_cols]


############################################### MAIN EVENT


time <- system.time({
  
  if(Parallelize) {  # (in parallel)
    
      
    
    ################### USING jagsUI
    output <- dynocc(model_file = model_file, rf=results_folder, R=R, ir=1, 
                            debug=debug, use_jags=JAGS, parallel=Parallelize)
    
  } 
})
print(time)

############################################### end of BUGS model

############################################### clean output and save
write.csv(output, paste0(results_folder,'/',model_file,'_',R,'_',ni,'.csv'))
gc()

############################### daisy chain
source('source/daisychain.R') # Daisy chain function
# from https://github.com/NERC-CEH/cluster_guides/wiki/Cirrus#getting-files-to-cirrus-using-winscp

rdataFile<-load(file=paste0(results_folder, '/',model_file,'_',R,'_',ni,'.rData'))

daisyChain(rdataFile, total.t = 40000, outDir = results_folder, 
                       by.it = 5000, n.thin = nt, quiet = TRUE)
