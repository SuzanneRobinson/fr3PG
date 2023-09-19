# Function to estimate APAR (Absorbed Photosynthetically Active Radiation -phi.pa, mol / m ^ 2)
EstimateAPAR <- function(state, weather, parms, general.info){
    # state: vector with state variables
    # weather: vector with weather information corresponding to the current period
    # parms: vector with parameters of 3-PG model
    # general.info: list with global information to be used by 3-PG model
    molPAR_MJ <- parms[["molPAR_MJ"]]  # Conversion of solar radiation to PAR (mol / MJ)
    fullCanAge <- parms[["fullCanAge"]]  # Age at canopy cover (years)
    k <- parms[["k"]]  # Extinction coefficient for absorption of PAR by canopy (k)
    RAD.day <- weather[["SolarRad"]]  # Daily solar radiation (MJ / m ^ 2)
    year <- weather[["Year"]]  # Current year
    month <- weather[["Month"]]  # Current period
    t <- state[["t"]]  # Stand age (years)
    LAI <- state[["LAI"]]  # Leaf Area Index (m ^ 2 per m ^ 2 of ground)
    RAD <- RAD.day * general.info$daysinmonth[month]
    phi.p <- molPAR_MJ * RAD  # Photosynthetically active radiation (PAR, phi.p, mol / m ^ 2)
    CanCover <- if(fullCanAge > 0 & t < fullCanAge) (t  + 0.01) / fullCanAge else 1
    lightIntcptn <- 1 - exp(-k * LAI / CanCover)
    phi.pa <- phi.p * lightIntcptn * CanCover
    state[c("Year", "Month", "RAD.day", "RAD", "CanCover", "lightIntcptn", "phi.p", "phi.pa")] <- c(year, month, RAD.day, RAD, CanCover, lightIntcptn, phi.p, phi.pa)
    return(state)
}
