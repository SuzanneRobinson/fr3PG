# 5.3.Functions of the 3-PG model----
# Function to predict variables of interest within 3-PG model
PredictVariablesInterest.3PG <- function(state, parms, cod.pred){
    # state: vector with state variables
                                        # parms: vector with parameters of 3-PG model
    leaf.grow <- parms[["leaf.grow"]]  ## Leaf grow parameter
    leaf.fall <- parms[["leaf.fall"]]  ## Leaf fall parameter
    SLA0 <- parms[["SLA0"]]  # Specific leaf area at age 0 (m ^ 2 / kg)
    SLA1 <- parms[["SLA1"]]  # Specific leaf area for mature leaves  (m ^ 2 / kg)
    tSLA <- parms[["tSLA"]]  # Age at which specific leaf area = (SLA0 + SLA1) / 2 (years)
    fracBB0 <- parms[["fracBB0"]]  # Branch and bark fraction at age 0
    fracBB1 <- parms[["fracBB1"]]  # Branch and bark fraction for mature stands
    tBB <- parms[["tBB"]]  # Age at which fracBB = (fracBB0+fracBB1)/2 (years)
    t <- state[["t"]]  # Stand age (years)
    N <- state[["N"]]  # Number of stems per hectare
    Wl <- state[["Wl"]]  # Foliage biomass (tDM / ha)
    Wr <- state[["Wr"]]  # Root biomass (tDM / ha)
    #WlDormant <- state[["WlDormant"]]  ## Foliage biomass (tDM/ha) when dormant
    Wsbr <- state[["Wsbr"]]  # Stem and branches biomass (tDM / ha)
    SLA <- SLA1 + (SLA0 - SLA1) * exp(-log(2) * (t / tSLA) ^ 2)  # Specific leaf area (m ^ 2 / kg)
    LAI <- Wl * SLA * 0.1  # Leaf Area Index (m ^ 2 per m ^ 2 of ground)
    fracBB <- fracBB1 + (fracBB0 - fracBB1) * exp(-log(2) * (t / tBB))
    Ww <- Wsbr * (1 - fracBB)  # Wood stand biomass (tDM / ha)
    Wb <- (Wsbr - Ww) * 0.5675  # Bark stand biomass (tDM / ha)
    Wbr <- (Wsbr - Ww) - Wb  # Branches stand biomass (tDM / ha)
    Wa <- Wl + Wsbr  # Above-ground stand biomass (tDM / ha)
    W <- Wl + Wr + Wsbr  # Total stand biomass (tDM / ha)
    hdom <- EstimateH.3PG(N = N, Wa = Wa)  # Dominant height (m)
    wsbrg <- Wsbr * 1000 / N  # Tree stem and branches biomass (kg / tree)
    if(cod.pred == "3PG"){  # To obtain the same results and in Excel sheet "3PG - EXCEL - Eglobulus.xls", we need to use this approach
        rhoMax <- parms[["rhoMax"]]
        rhoMin <- parms[["rhoMin"]]
        tRho <- parms[["tRho"]]
        WoodDensity <- rhoMax + (rhoMin - rhoMax) * exp(-log(2) * t / tRho)
        Vu <- Wsbr * (1 - fracBB) / WoodDensity
        dg <- (wsbrg / parms[["aS"]]) ^ (1 / parms[["nS"]])   
        G <- (dg / 200) ^ 2 * pi * N
    } else if(cod.pred == "Allometric") {
        G <- EstimateG.3PG(N = N, Wa = Wa)  # Stand basal area (m ^ 2 / ha)
        Vub <- EstimateV.3PG(N = N, Ww = Ww)  # Stand volume under bark (m ^ 3 / ha)
        dg <- ObtainDg(N = N, G = G)  # Quadratic mean diameter (cm)  
    }
    state[c("LAI", "Ww", "Wb", "Wbr", "Wa", "W", "hdom", "wsbrg", "G", "Vu", "dg" ##, "WlDormant"
            )] <- c(LAI, Ww, Wb, Wbr, Wa, W, hdom, wsbrg, G, Vu, dg ## WlDormant
                    )
    return(state)
}
