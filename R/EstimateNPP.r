# Function to estimate Gross Primary Production (GPP, tDM / ha) and Net Primary Production  (NPP, tDM / ha)
EstimateNPP <- function(state, parms){
    # state: vector with state variables
    # parms: vector with parameters of 3-PG model
    alpha <- parms[["alpha"]]  # Canopy quantum efficiency (mol C / mol PAR)
    gDM_mol <- parms[["gDM_mol"]]  # Molecular weight of dry matter (gDM / mol)
    Y <- parms[["Y"]]  # Ratio NPP / GPP
    phi.pa <- state[["phi.pa"]]  # Absorbed Photosynthetically Active Radiation (mol / m ^ 2)
    fT <- state[["fT"]]  # Temperature modifier
    fF <- state[["fF"]]  # Frost modifier
    fCalpha <- state[["fCalpha"]]  # CO2 modifier
    fN <- state[["fN"]]  # Nutrition modifier
    PhysMod <- state[["PhysMod"]]  # Physiological modifier
    alphaC <- alpha * fT * fF * fCalpha * fN * PhysMod
    GPP <- alphaC * gDM_mol * phi.pa / 100
    NPP <- Y * GPP
    state[c("alphaC", "GPP", "NPP")] <- c(alphaC, GPP, NPP)
    return(state)
}
