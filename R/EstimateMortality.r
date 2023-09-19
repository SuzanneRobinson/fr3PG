# Function to estimate mortality
EstimateMortality <- function(state, parms){
    # state: vector with state variables
    # parms: vector with parameters of 3-PG model
    wSx1000 <- parms[["wSx1000"]]  # Maximum stem tree biomass for N = 1000 trees / hectare
    mF <- parms[["mF"]]  # Proportion of stand foliage biomass assumed to be removed for each dead tree
    mR <- parms[["mR"]]  # Proportion of stand root biomass assumed to be removed for each dead tree
    mS <- parms[["mS"]]  # Proportion of stand stem biomass assumed to be removed for each dead tree
    thinPower <- parms[["thinPower"]]  # Power in self-thinning rule
    oldW <- state[["Wsbr"]]  # Stant stem and branches biomass (tDM / ha)
    oldN <- state[["N"]]  # Number of stems per hectare
    if(oldW > (wSx1000 * (1000 / oldN) ^ thinPower)){
        N.new <- optimize(f = function(x) ((oldW - mS * (oldN - x) * oldW / oldN) - (wSx1000 * (1000 / x) ^ thinPower * 1e-3) * x) ^ 2, interval = c(1, 5000))$minimum
        N <- min(oldN, N.new)
    } else {
        N <- oldN
    }
    Ndead <- oldN - N
    # Update biomass
    if(Ndead > 0){
        Wl <- state[["Wl"]]
        Wsbr <- oldW
        Wr <- state[["Wr"]]
        Wdl <- mF * Ndead * (Wl / N)
        Wds <- mS * Ndead * (Wsbr / N)
        Wdr <- mR * Ndead * (Wr / N)
        state[["Wl"]] <- Wl - Wdl
        state[["Wsbr"]] <- Wsbr - Wds
        state[["Wr"]] <- Wr - Wdr
        state[["N"]] <- state[["N"]] - Ndead
    } else {
        Wdl <- 0
        Wds <- 0
        Wdr <- 0
    }
    state[c("Ndead", "Wdl", "Wds", "Wdr")] <- c(Ndead, Wdl, Wds, Wdr)
    return(state)
}
