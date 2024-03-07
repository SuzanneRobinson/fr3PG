
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
library(patchwork)
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
simDatLoc<-(paste0(path2files,"Cirencester_allVars_CHESS-met_1992_2019.csv"))

# Currently set to monthly timestep
clm.cirencester<-as_tibble(getIndvClm(scape=F, siteName = "Cirencester", simDatLoc))

clm.cirencester <- clm.cirencester %>%
  dplyr::select(Year, Month, Tmean, Tmax, Tmin,  Rain, SolarRad, FrostDays, MonthIrrig)


###################################
# Set parameters
##################################
poplar<-getParms_poplar(weather=clm.cirencester)


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
      "wSx1000","fN0", "MaxIntcptn","k", "fullCanAge", "kF","rhoMin", "rhoMax", "tRho", "FR")


priorVals<-createPriors_poplar(poplar=poplar)[[1]]
param_scaler<-createPriors_poplar(poplar=poplar)[[2]]

# Save param_scaler
saveRDS(param_scaler, file = "data/param_scaler_poplar.RDS")



###################################
# get measured values
##################################
dbh_average<-read.csv(paste0(path2files,"double_bark_thickness/DBT_Poplar_Cirencester_annual_average.csv"))
# Climate data currently up to 2019
dbh_average<-dbh_average%>%filter(YEAR<=2019)

# Create array with DBH
observed<-c(dbh_average$D.ob..cm.)

# SD estimated from the dendro 
dev<-c(dbh_average$SEM.D.ob..cm.)
# Set the first year to non-zero value
dev[1]<-0.1


# Set a standard deviation
#coefVar=0.1
#dev=observed*coefVar


###################################
# likelihood function
##################################
likelihoodFunc<-poplarLL


###################################
# RUN CALIBRATION
##################################

#run in loop and write to file every 100k in case of errors or problems with JASMIN - allows for easy restarting of mcmc chain if something goes wrong
for (i in c(1:15)){
  iters=50000
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
  if(file.exists(paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_cirencester_",timeStep,"_",chainNum,".RDS"))==TRUE){
    print("previous file exists, restarting chain")
    out<-readRDS(paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_cirencester_",timeStep,"_",chainNum,".RDS"))
  }
  
  #on JASMIN I found you need to create bayesian setup even if re-starting a chain, I think as this initiates the cluster needed to run in parallel
  BS3PGDN <- createBayesianSetup(likelihood = likelihoodFunc, prior = priorVals, names = nm, parallel = 8, catchDuplicates = F )
  
  #check if a run already exists and if so restart it
  out<-suppressWarnings(if(file.exists(paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_cirencester_",timeStep,"_",chainNum,".RDS"))==TRUE) runMCMC(bayesianSetup =out, sampler = "DEzs", settings = settings) else runMCMC(bayesianSetup =BS3PGDN, sampler = "DEzs", settings = settings))
  
  summary(out)
  
  #Save output
  saveRDS(out,file=paste0("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_cirencester_",timeStep,"_",chainNum,".RDS"))
  
  #stop cluster before restarting the loop otherwise JASMIN sometimes throws an error
  stopParallel(BS3PGDN)
  rm(BS3PGDN)
  
  
}

##########################################################################################
# Run model with calibrated parameters
##########################################################################################

# Location of daily CHESS-met climate data
simDatLoc<-(paste0(path2files,"Cirencester_allVars_CHESS-met_1992_2019.csv"))

# Currently set to monthly timestep
clm.cirencester<-as_tibble(getIndvClm(scape=F, siteName = "Cirencester", simDatLoc))

clm.cirencester <- clm.cirencester %>%
  dplyr::select(Year, Month, Tmean, Tmax, Tmin,  Rain, SolarRad, FrostDays, MonthIrrig)

clm.site<-clm.cirencester

poplar<-getParms_poplar(weather=clm.cirencester)



exampParams<-readRDS("C:/Users/suzanne.robinson/OneDrive - Forest Research/projects/3PG_SonWaL/productive_broadleaf_development/dendro/calibration/poplar_cirencester_monthly_1.RDS")
exampParams<-mergeChains(exampParams$chain)
exParms<-miscTools::colMedians(as.data.frame(exampParams))
exParms<-exParms[1:31] 

exParms<-exParms* param_scaler

names(exParms)<-nm
poplar[nm]<-exParms[nm]



##################################
# Run 3PG
##################################

output<-do.call(fr3PG,poplar)%>%
  slice(-1)
output$Year<- clm.cirencester$Year
output<- output %>%
  mutate(yearMonth = paste(Year, Month, sep = "-"))


##################################
# Plot output
##################################

# Convert 'yearMonth' to a date format for better plotting
output$yearMonth <- as.Date(paste0(output$yearMonth, "-01"), format = "%Y-%m-%d")

pDg<-ggplot(output, aes(x = yearMonth, y = dg)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Mean stand DBH [cm]")+
  theme_classic()  

pVu<-ggplot(output, aes(x = yearMonth, y = Vu)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Stand Volume [m3/ha]")+
  theme_classic()  

pNPP<-ggplot(output, aes(x = yearMonth, y = NPP)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Annual NPP [tDM/ha]")+
  theme_classic()  


pLAI<-ggplot(output, aes(x = yearMonth, y = LAI)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "LAI [m2/m2]")+
  theme_classic() 


egg::ggarrange(pDg,pVu,pNPP,pLAI)



output2<-output%>%group_by(Year)%>%
  summarise(dg=mean(dg,na.rm=T),
            Vu=mean(Vu,na.rm=T),
            NPP=sum(NPP,na.rm=T),
            LAI=mean(LAI,na.rm=T))


pDg<-ggplot(output2, aes(x = Year, y = dg)) +
  geom_line() +
  #scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Mean stand DBH [cm]")+
  theme_classic()  

pVu<-ggplot(output2, aes(x = Year, y = Vu)) +
  geom_line() +
  #scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Stand Volume [m3/ha]")+
  theme_classic()  

pNPP<-ggplot(output2, aes(x = Year, y = NPP)) +
  geom_line() +
  #scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Annual NPP [tDM/ha]")+
  theme_classic()  


pLAI<-ggplot(output2, aes(x = Year, y = LAI)) +
  geom_line() +
  #scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "LAI [m2/m2]")+
  theme_classic() 


egg::ggarrange(pDg,pVu,pNPP,pLAI)






#################################################
# get observed values (dbh over bark measurements from the dendro)
#################################################
dbh_average<-read.csv(paste0(path2files,"double_bark_thickness/DBT_Poplar_Cirencester_annual_average.csv"))

dbh_average<-dbh_average%>%filter(YEAR<=2019)


ggplot() +
  geom_line(data=output2, aes(x=Year, y=dg, color='Line'), linetype = "solid", linewidth= 1) +
  geom_errorbar(data=dbh_average, aes(x=YEAR, ymin=D.ob..cm.- SEM.D.ob..cm., ymax=D.ob..cm.+ SEM.D.ob..cm.), width=.3, color='black') +
  geom_point(data=dbh_average, aes(x=YEAR, y=D.ob..cm., color='D.ob..cm.'), size=2) +
  geom_point(data=dbh_average, aes(x=YEAR, y= D.ub..cm., color='D.ub..cm.'), size=2) +
 # geom_point(data=dbh_average, aes(x=YEAR, y= DBH.Average.across.years..2005.2022, color='DBH.Avarage.across.years..2005.2022'), size=2) +
  scale_color_manual(name = "Data", 
                     values = c('Line' = 'lightblue', 
                                'D.ob..cm.' = 'black', 
                                'D.ub..cm.' = 'purple'), 
                               # 'DBH.Average.across.years..2005.2022' = 'red'),
                     labels = c('Line' = '3PG',
                                'D.ob..cm.' = 'D.ob..cm.', 
                                'D.ub..cm.' = 'D.ub..cm.'))+ 
                                #'DBH.Avarage.across.years..2005.2022' = 'DBH.Avarage.across.years..2005.2022')) +
  labs(x = "Year",
       y = "Mean stand DBH [cm]") +
  theme_classic()


