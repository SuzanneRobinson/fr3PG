# Allometric relationships to obtain stand variables from biomass variables
# Function for stand basal area estimation 
EstimateG.3PG <- function(N, Wa){
    # N: number of stems per hectare
    # Wa: above-ground biomass
    k <- 0.6222 + 0.1719 * N / 1000
    a <- 0.6987 - 0.0196 * N / 1000
    G <- k * Wa ^ a 
    return(G)
}
