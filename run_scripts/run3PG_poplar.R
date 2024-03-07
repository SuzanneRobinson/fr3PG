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

## Load the 3PG package
devtools::load_all(".")

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





##################################
# Run 3PG
##################################

output<-do.call(fr3PG,poplar)%>%
  slice(-1)
output$Year<- clm.tadcaster$Year
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
dbh_average<-read.csv(paste0(path2files,"DBT_Poplar_Tadcaster_annual_average.csv"))

dbh_average<-dbh_average%>%filter(YEAR<=2019)

ggplot() +
  geom_line(data=output2, aes(x=Year, y=dg), color='lightblue',linewidth=1) +
  geom_errorbar(data=dbh_average, aes(x=YEAR, ymin=D.ob..cm.-SD.D.ob..cm., ymax=D.ob..cm.+SD.D.ob..cm.), width=.3, color='black') +
  geom_point(data=dbh_average, aes(x=YEAR, y=D.ob..cm.), size=2, color='black')+
  geom_point(data=dbh_average, aes(x=YEAR, y= D.ub..cm.), size=2, color='purple')+
  geom_point(data=dbh_average, aes(x=YEAR, y= DBH.Avarage.across.years..2005.2022), size=2, color='red')
  
  #scale_x_date(limits = as.Date(c("1992-01-01", "2019-12-31")), date_breaks = "2 year", date_labels = "%Y") +
  labs(x = "Year",
       y = "Mean stand DBH [cm]")+
  theme_classic() 


  
  ggplot() +
    geom_line(data=output2, aes(x=Year, y=dg, color='Line'), linetype = "solid", linewidth= 1) +
    geom_errorbar(data=dbh_average, aes(x=YEAR, ymin=D.ob..cm.-SD.D.ob..cm., ymax=D.ob..cm.+SD.D.ob..cm.), width=.3, color='black') +
    geom_point(data=dbh_average, aes(x=YEAR, y=D.ob..cm., color='D.ob..cm.'), size=2) +
    geom_point(data=dbh_average, aes(x=YEAR, y= D.ub..cm., color='D.ub..cm.'), size=2) +
    geom_point(data=dbh_average, aes(x=YEAR, y= DBH.Avarage.across.years..2005.2022, color='DBH.Avarage.across.years..2005.2022'), size=2) +
    scale_color_manual(name = "Data", 
                       values = c('Line' = 'lightblue', 
                                  'D.ob..cm.' = 'black', 
                                  'D.ub..cm.' = 'purple', 
                                  'DBH.Avarage.across.years..2005.2022' = 'red'),
                       labels = c('Line' = '3PG',
                                  'D.ob..cm.' = 'D.ob..cm.', 
                                  'D.ub..cm.' = 'D.ub..cm.', 
                                  'DBH.Avarage.across.years..2005.2022' = 'DBH.Avarage.across.years..2005.2022')) +
    labs(x = "Year",
         y = "Mean stand DBH [cm]") +
    theme_classic()
  






# Create array with DBH
obs<-c(dbh_average$D.ob..cm.)

# Set a standard deviation
#coefVar=0.25
#dev=obs*coefVar

# Or this could be estimated from the dendro 
dev<-c(dbh_average$SD.D.ob..cm.)
# Set the first year to non-zero value
dev[1]<-0.2



