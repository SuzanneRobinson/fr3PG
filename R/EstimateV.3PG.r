# Function for stand volume estimation
EstimateV.3PG <- function(N, Ww){
    # N: number of stems per hectare
    # Ww: wood biomass
    k <- 2.3415 + 0.1049 * N / 1000
    a <- 0.9378 - 0.00438 * N / 1000
    V <- k * Ww ^ a
    return(V)
}
