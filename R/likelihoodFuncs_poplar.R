# Extract simulated dbh data for use in likelihood function
sampleOutputPoplar<-function(sim,sY=1992,eY=2019){
  sim<- filter(sim,Year>=sY&Year<=eY)
  m<-c(
    # Mean annual DBH
    sim %>%
      group_by(Year)%>%
      summarise(dg_mean=mean(dg,na.rm=TRUE))%>%
      pull(dg_mean)
  )    
  return(m)
}


# sivia likelihood calculation
flogL <- function(sims,data,data_s)
{ 
  Ri         <- (sims - data) / data_s
  i0         <- which( abs(Ri)<1.e-08 )
  
  logLi      <- log(1-exp(-0.5*Ri^2)) - log(Ri^2) - 0.5*log(2*pi) - log(data_s)
  logLi[i0]  <- -0.5*log(2*pi) - log(2*data_s[i0])
  
  sum(logLi)
}

## Likelihood function
poplarLL<-  function(p){
  p<-p*.GlobalEnv$param_scaler
  poplar[.GlobalEnv$nm]<-p
  
  NlogLik <- tryCatch(
    {
      output<-   do.call(fr3PG,poplar)%>%
        slice(-1)
      output$Year<- .GlobalEnv$clm.site$Year
      modelled <- sampleOutputPoplar(output,.GlobalEnv$startYear,.GlobalEnv$endYear)
      mod_sim <- data.frame(modelled = modelled, obs = .GlobalEnv$observed, dev = .GlobalEnv$dev)
      mod_sim <- mod_sim[is.na(mod_sim$obs) == F,]
      NlogLik  <- ifelse(any(is.na(modelled) == T), -Inf, flogL(data=mod_sim$obs, sims=mod_sim$modelled, data_s=mod_sim$dev))
    },
    error=function(cond) {
      return(-Inf)
    })
  return(NlogLik)
}