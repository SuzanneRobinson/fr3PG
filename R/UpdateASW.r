# Function to update available soil water in each iteration
UpdateASW <- function(state, weather, site, parms, general.info){
    # state: vector with state variables
    # weather: vector with weather information corresponding to the current period
    # site: vector with site variables
    # parms: vector with parameters of 3-PG model
    # general.info: list with global information to be used by 3-PG model
    # Rainfall - update ASW with rainfall
    Rain <- weather[["Rain"]]  # Rainfall for the current period (mm)
    MonthIrrig <- weather[["MonthIrrig"]]
    ASW <- state[["ASW"]]  # Available soil water at the beginning of the period (mm)
    ASWrain <- ASW + Rain + MonthIrrig # + pooledSW  # TODO: what to do with pooledSW
    # pooledSW = poolFractn * excessSW
    # RunOff = (1 - poolFractn) * excessSW
    # Rainfall - interception
    LAI <- state[["LAI"]]  # Leaf Area Index (m ^ 2 / m ^ 2 ground)
    LAImaxIntcptn <- parms[["LAImaxIntcptn"]]  # LAI for maximum rainfall interception
    MaxIntcptn <- parms[["MaxIntcptn"]]  # Maximum proportion of rainfall evaporated from canopy
    RainIntcptn <- MaxIntcptn * ifelse(LAImaxIntcptn <= 0, 1, min(1, LAI / LAImaxIntcptn)) * Rain
    # Transpiration - net radiation
    Qa <- parms[["Qa"]]  # Intercept of net v. solar radiation relationship (W / m ^ 2)
    Qb <- parms[["Qb"]]  # Slope of net v. solar radiation relationship
    month <- weather[["Month"]]  # Current period
    latitude <- site[["latitude"]]  # Site latitude 
    RAD.day <- state[["RAD.day"]]  # Daily solar radiation (MJ / m ^ 2)
    h <- GetDayLength(Lat = latitude, month = month)
    netRad <- Qa + Qb * (RAD.day * 1e6 / h)
    # Transpiration - canopy conductance
    MinCond <- parms[["MinCond"]]  # Minimum canopy conductance (m / s)
    MaxCond <- parms[["MaxCond"]]  # Maximum canopy conductance (m / s)
    LAIgcx <- parms[["LAIgcx"]]  # LAI for maximum canopy conductance
    fCg0 <- parms[["fCg0"]]  # Parameter derived from fCg700 (canopy conductance enhancement factor at 700 ppm)
    CO2 <- site[["CO2"]]
    PhysMod <- state[["PhysMod"]]  # Physiological modifier
    fCg <- fCg0 / (1 + (fCg0 - 1) * CO2 / 350)  # fCg: CO2 modifier (included 14/06/2017)
    CanCond <- (MinCond + (MaxCond - MinCond) * (min(1, LAI / LAIgcx))) * PhysMod * fCg # fCg included 14/06/2017
    CanCond <- ifelse(CanCond == 0, 0.0001, CanCond)
    # Transpiration - evapotranspiration (J / (m^2 s))
    e20 <- parms[["e20"]]  # Rate of change of saturated vapour pressure with temperature = 20oC
    rhoAir <- parms[["rhoAir"]]  # Density of air (kg / m ^ 3)
    lambda <- parms[["lambda"]]  # Latent heat of vapourisation of H2O (J / kg)
    VPDconv <- parms[["VPDconv"]]  # VPD conversion factor to saturation deficit = 18/29/1000
    BLcond <- parms[["BLcond"]]  # Canopy boundary layer conductance (m / s)
    VPD <- weather[["VPD"]]  # Vapour pressure deficit 
    Etransp <- (e20 * netRad + rhoAir * lambda * VPDconv * VPD * BLcond) / (1 + e20 + BLcond / CanCond)
    # Transpiration - canopy transpiration (kg / (m ^ 2 day) or mm / day)
    CanTransp <- Etransp / lambda * h
    # Transpiration - monthly canopy transpiration (kg / (m^2 month) or mm / month)
    Transp <- general.info$daysinmonth[month] * CanTransp
    # Evapotranspiration
    EvapTransp <- min(Transp + RainIntcptn, ASWrain)
    # Soil water excess
    MaxASW <- site[["MaxASW"]]  # Maximum available soil water
    excessSW <- max(ASWrain - EvapTransp - MaxASW, 0)
    # Update ASW
    state[["ASW"]] <- ASWrain - EvapTransp - excessSW
    # Update GPP and NPP estimations if SW restrictions are present
    scaleSW <- EvapTransp / (Transp + RainIntcptn)
    GPP <- state[["GPP"]]
    NPP <- state[["NPP"]]
    state[["GPP"]] <- GPP * scaleSW
    state[["NPP"]] <- NPP * scaleSW
    state[c("RainIntcptn", "netRad", "CanCond", "Etransp", "CanTransp", "Transp", "EvapTransp", "excessSW", "scaleSW")] <- c(RainIntcptn, netRad, CanCond, Etransp, CanTransp, Transp, EvapTransp, excessSW, scaleSW)
    return(state)
}
