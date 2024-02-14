#' @description function for getting climate data for individual dendro sites extracted
#' @export
getIndvClm<-function(scape=F, siteName = "Tadcaster", simDatLoc){
  
  relative_humidity_calc<-function(Tmean,Tref=273.16,pp,spec_hum){
    TmeanK<-Tmean+273.15
    
    RH<- 0.263*pp*spec_hum*(exp((17.67*(TmeanK-Tref))/(TmeanK-29.65)))^-1
    RH[RH>100]<-100
    RH[RH<0]<-0
    
    return(RH)
  }
  
  simDat<-read.csv(simDatLoc)%>%
    filter(site_code==siteName)
  
  
  clm<-simDat
  
  
  if(scape==F){
    #clm <- spread(clm, key = clm_var, value = value)
    
    clm<-clm[order(as.Date(clm$date, format="%d/%m/%Y")),]
    rownames(clm) <- NULL
    
    #clm$pyear<-1992
    
    # Calculate met variables 
    clm$Tmax<-clm$tas+(clm$dtr/2)
    clm$Tmin<-clm$tas-(clm$dtr/2)
    clm$RH<-relative_humidity_calc(Tmean=clm$tas-273.15,pp=clm$psurf,spec_hum=clm$huss)
    
    clm<-clm%>%dplyr::select(date,RH, precip,rlds,rsds,tas,Tmax,Tmin)

    
    names(clm) <-
      c("date",
        "RH",
        "Rain",
        "SolarRadLW",
        "SolarRad",
        "Tmean",
        "Tmax",
        "Tmin"
      )
  }
  
  if(scape==T){
    # Leaving this here incase want to run under CC scenarios
    #clm <- spread(clm, key = clm_var, value = value)
    clm<-clm[order(as.Date(clm$date, format="%d/%m/%Y")),]
    rownames(clm) <- NULL
    
    clm$pyear<-2018
    
    # Calculate met variables 
    clm$Tmax<-clm$tas+(clm$dtr/2)
    clm$Tmin<-clm$tas-(clm$dtr/2)
    clm$RH<-relative_humidity_calc(Tmean=clm$tas-273.15,pp=clm$psurf,spec_hum=clm$huss)
    
    clm<-clm%>%dplyr::select(date,RH, precip,rlds,rsds,tas,Tmax,Tmin,pyear)
    
    names(clm) <-
      c("date",
        "RH",
        "Rain",
        "SolarRadLW",
        "SolarRad",
        "Tmean",
        "Tmax",
        "Tmin",
        "pyear"
      )
  }
  #convert to celsius from kelvin add date, weeks and months
  clm<-clm%>%
    mutate(Tmean = Tmean-273.15, Tmax = Tmax - 273.15, Tmin = Tmin - 273.15) %>% 
    mutate(Date = as.Date(date,"%d/%m/%Y"))%>%
    mutate(Month = month(Date), week = week(Date), Year = year(Date))
  
  
  #calc vpd
  clm$VPD <-
    ((((0.61078 * exp(17.269 * (clm$Tmean) / 
                        (237.3 + clm$Tmean))) * (1 - (clm$RH) / 100))))
  

  
  
  
  
  #get climate data into correct units
  clm <- clm %>%
    dplyr::group_by(Year, Month) %>% # Month or week
    dplyr::summarise(
      Month = median(Month),
      Tmax = max(Tmax),
      Tmin = min(Tmin),
      Tmean = mean(Tmean),
      Rain = sum(Rain * 86400),
      # Converts from W/m2 to Mj/m2/day
      SolarRad = mean((SolarRad * 86400) / 1e+6),
      FrostDays = 0,
      MonthIrrig = 0,
      VPD = mean(VPD)
    )
  

  weather<-as.data.frame(clm)
  
  return(weather)
}