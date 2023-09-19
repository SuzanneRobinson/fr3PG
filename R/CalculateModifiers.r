# Function to calculate biomass production modifiers
CalculateModifiers <- function(state, weather, site, parms, general.info){
  # state: vector with state variables
  # weather: vector with weather information corresponding to the current period
  # site: vector with site variables
  # parms: vector with parameters of 3-PG model
  # general.info: list with global information to be used by 3-PG model
  # fT - temperature modifier
  Tmin <- parms[["Tmin"]]  # Minimum temperature for growth (?C)
  Tmax <- parms[["Tmax"]]  # Maximum temperature for growth (?C)
  Topt <- parms[["Topt"]]  # Optimum temperature for growth (?C)
  Tav <- weather[["Tmean"]]  # Monthly mean temperature (?C)
  if(Tav < Tmin | Tav > Tmax){
    fT <- 0
  } else {
    fT <- ((Tav - Tmin) / (Topt - Tmin)) * ((Tmax - Tav) / (Tmax - Topt)) ^ ((Tmax - Topt) / (Topt - Tmin))
  }
  # fF - frost modifier
  kF <- parms[["kF"]]  # Days production lost per frost day
  FrostDays <- weather[["FrostDays"]]  # Monthly number of frost days
  fF <- 1 - kF * (FrostDays / 30)
  # fCalpha: CO2 modifier (included 14/06/2017)
  CO2 <- site[["CO2"]]
  fCalphax <- parms[["fCalphax"]]  # Parameter derived from fCalpha700 (assimilation enhancement factor at 700 ppm)
  fCalpha <- fCalphax * CO2 / (350 * (fCalphax - 1) + CO2)
  # fN - nutrition modifier
  fN0 <- parms[["fN0"]]  # Value of fN when FR = 0
  FR <- site[["FR"]]  # Fertility rating
  fN <- fN0 + (1 - fN0) * FR
  # fAge - age modifier
  t <- state[["t"]]
  RelAge <- t / parms[["MaxAge"]]  # Maximum stand age used in age modifier
  rAge <- parms[["rAge"]]  # Relative age to give fAge = 0.5
  nAge <- parms[["nAge"]]  # Power of relative age in function for fAge
  fAge <- 1 / (1 + (RelAge / rAge) ^ nAge)
  # fVPD - vapour pressure deficit modifier
  CoeffCond <- parms[["CoeffCond"]]  # Defines stomatal response to VPD
  VPD <- weather[["VPD"]]  # Monthly vapour pressure deficit (kPa)
  fVPD <- exp(-CoeffCond * VPD)
  # fSW - soil water modifier
  MaxASW <- site[["MaxASW"]]  # Maximum available soil water
  parms.soil <- general.info$parms.soil
  parms.sw.site <- parms.soil[which(parms.soil$soilclass == site[["soilclass"]]), ]  # Select the soil class according to code
  SWconst <- parms.sw.site[["SWconst"]]  # Moisture ratio deficit for fSW = 0.5 for a specific soil class
  SWpower <- parms.sw.site[["SWpower"]]  # Power of moisture ratio deficit for a specific soil class
  ASW <- state[["ASW"]]  # Available soil water (mm)
  MoistRatio <- ASW / MaxASW
  fSW <- 1 / (1 + ((1 - MoistRatio) / SWconst) ^ SWpower)
  # PhysMod - physiological modifier, composed by fAge, fVPD and fSW
  PhysMod <- fAge * min(fVPD, fSW)
  state[c("fT", "fF", "fCalpha", "fN", "fAge", "fVPD", "fSW", "PhysMod")] <- c(fT, fF, fCalpha, fN, fAge, fVPD, fSW, PhysMod)
  return(state)
}
