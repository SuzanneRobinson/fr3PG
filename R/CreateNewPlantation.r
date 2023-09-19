# Function to create a new plantation
CreateNewPlantation <- function(state, parms, fma){
    # state: vector with state variables
    # parms: vector with parameters of 3-PG model
    # fma: data.frame with the FMA of the current period
    Wl.s <- parms[["Wl.s"]]
    Wr.s <- parms[["Wr.s"]]
    Wsbr.s <- parms[["Wsbr.s"]]
    if(missing(fma)){
        N <- state[["N"]]
    } else {
        N <- unique(fma$Npl)
    }
    state[["N"]] <- N
    state[["Wl"]] <- N * Wl.s / 1000
    state[["Wr"]] <- N * Wr.s / 1000
    state[["Wsbr"]] <- N * Wsbr.s / 1000
    return(state)
}
