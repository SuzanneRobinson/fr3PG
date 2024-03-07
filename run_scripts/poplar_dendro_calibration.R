
#options(scipen=999)

# SOIL TYPE
# FERTILIY RATING 
# PRESCRIPTION
# NUMBER of STEMS


library(tidyr)
library(tidyverse)
library(purrr)
library(BayesianTools)
library(sensitivity)
library(dplyr)
library(future)
library(ncdf4)
library(ggplot2)
library(httr)
library(furrr)
library(viridis)
library(tibble)
library(miscTools)
library(parallel)
library(sf)
library(rgdal)
library(raster)
library(lubridate)
library(coda)

## Load the 3PG package
devtools::load_all(".")

#read in arguments from batch file
args<-c("monthly", "monthly_1", "1")
print(args)
timeStep=args[1]
chainID=args[2]
chainNum=args[3]

startYear=1992
endYear=2019


path2files<-'C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/'

###################################
# Get climate
##################################

# Location of daily CHESS-met climate data
simDatLoc<-(paste0(path2files,"Tadcaster_allVars_CHESS-met_1992_2019.csv"))

# Currently set to monthly timestep
clm.tadcaster<-as_tibble(getIndvClm(scape=F, siteName = "Tadcaster", simDatLoc))

clm.tadcaster <- clm.tadcaster %>%
  dplyr::select(Year, Month, Tmean, Tmax, Tmin,  Rain, SolarRad, FrostDays, MonthIrrig)


###################################
# Set parameters
##################################
poplar<-getParms_poplar(weather=clm.tadcaster)


###################################
# Set a prescription ? 
##################################
# Site description -> 
# branches of poplar pruned before they reached a diameter of 5cm



###################################
# create priors 
##################################

# name of parameters to calibrate for; currently set to parameters calibrated in xenakis et al., 2008
nm<-c("pFS2","pFS20","aS","nS","pRx","pRn","gammaFx","gammaF0","tgammaF","Rttover",
      "mF","mR","mS","SLA0","SLA1","tSLA","alpha","Y", "MaxCond", "LAIgcx", "m0", 
      "wSx1000","fN0", "MaxIntcptn","k", "fullCanAge", "kF","rhoMin", "rhoMax", "tRho")


priorVals<-createPriors_poplar(poplar=poplar)[[1]]
param_scaler<-createPriors_poplar(poplar=poplar)[[2]]

# Save param_scaler
saveRDS(param_scaler, file = "data/param_scaler_poplar.RDS")



###################################
# get measured values
##################################
dbh_average<-read.csv(paste0(path2files,"DBT_Poplar_Tadcaster_annual_average.csv"))
# Climate data currently up to 2019
dbh_average<-dbh_average%>%filter(YEAR<=2019)

# Create array with DBH
observed<-c(dbh_average$D.ob..cm.)

# SD estimated from the dendro 
dev<-c(dbh_average$SD.D.ob..cm.)
# Set the first year to non-zero value
dev[1]<-0.2


###################################
# likelihood function
##################################
likelihoodFunc<-poplarLL


###################################
# RUN CALIBRATION
##################################

#run in loop and write to file every 100k in case of errors or problems with JASMIN - allows for easy restarting of mcmc chain if something goes wrong
for (i in c(1:15)){
  iters=10000
  #Initiate bayesian setup
  settings = list(
    iterations = iters,
    startValue = 7, # internal chain number
    nrChains = 1, # Number of chains
    pSnooker = 0.5,
    burnin = round(iters/100*10), #10% burnin
    parallel = T,
    message = TRUE)
  
  #check if files already exist and restart the chain or start from initial
  if(file.exists(paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_",timeStep,"_",chainNum,".RDS"))==TRUE){
    print("previous file exists, restarting chain")
    out<-readRDS(paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_",timeStep,"_",chainNum,".RDS"))
  }
  
  #on JASMIN I found you need to create bayesian setup even if re-starting a chain, I think as this initiates the cluster needed to run in parallel
  BS3PGDN <- createBayesianSetup(likelihood = likelihoodFunc, prior = priorVals, names = nm, parallel = 8, catchDuplicates = F )
  
  #check if a run already exists and if so restart it
  out<-suppressWarnings(if(file.exists(paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_",timeStep,"_",chainNum,".RDS"))==TRUE) runMCMC(bayesianSetup =out, sampler = "DEzs", settings = settings) else runMCMC(bayesianSetup =BS3PGDN, sampler = "DEzs", settings = settings))
  
  summary(out)
  
  #Save output
  saveRDS(out,file=paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_",timeStep,"_",chainNum,".RDS"))
  
  #stop cluster before restarting the loop otherwise JASMIN sometimes throws an error
  stopParallel(BS3PGDN)
  rm(BS3PGDN)
  
  
}

##########################################################################################
# Run model with calibrated parameters
##########################################################################################

# Location of daily CHESS-met climate data
simDatLoc<-(paste0(path2files,"Tadcaster_allVars_CHESS-met_1992_2019.csv"))

# Currently set to monthly timestep
clm.tadcaster<-as_tibble(getIndvClm(scape=F, siteName = "Tadcaster", simDatLoc))

clm.tadcaster <- clm.tadcaster %>%
  dplyr::select(Year, Month, Tmean, Tmax, Tmin,  Rain, SolarRad, FrostDays, MonthIrrig)


poplar<-getParms_poplar(weather=clm.tadcaster)



exampParams<-readRDS("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_monthly_1.RDS")
exampParams<-mergeChains(exampParams$chain)
exParms<-miscTools::colMedians(as.data.frame(exampParams))
exParms<-exParms[1:30] 

exParms<-exParms* param_scaler

names(exParms)<-nm
poplar[nm]<-exParms[nm]



output<-do.call(fr3PG,poplar)%>%
  slice(-1)
output$Year<- clm.tadcaster$Year
output<- output %>%
  mutate(yearMonth = paste(Year, Month, sep = "-"))
