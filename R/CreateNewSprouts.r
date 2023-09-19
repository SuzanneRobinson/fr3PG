# Function to create new sprouts
CreateNewSprouts <- function(state, parms, newsprouts){
    # state: vector with state variables
    # parms: vector with parameters of 3-PG model
    Wl.s <- parms[["Wl.s"]]
    Wsbr.s <- parms[["Wsbr.s"]]
    N <- state[["N"]]
    Nharv <- state[["Nharv"]]
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    N0 <- N
    if(newsprouts == 1){  # First creation of new sprouts
        N <- 4.26831 * Nharv + 285.96252 * (1 / 12)
        ## The original equation is N = 4.26831 * Nharv + 285.96252 * t, but t has been replaced by 1/12, as it is accumulated across the whole year
        ## N <- 4.26831 * Nharv + 285.96252 * (1 / 3)  # The original equation is N = 4.26831 * Nharv + 285.96252 * t, but t has been replaced by 1/3, as it is accumulated in the three months of Spring, namely April, May, and June
    } else {  ## Accumulating the creation of new sprouts over time
        N <- N + 285.96252 * (1 / 12)  ## The original equation is N = 4.26831 * Nharv + 285.96252 * t, but t has been replaced by 1/12, as it is accumulated across the whole year
        # N <- N + 285.96252 * (1 / 3)
    }
    Wl <- Wl + (N - N0) * Wl.s / 1000
    Wsbr <- Wsbr + (N - N0) * Wsbr.s / 1000
    state[["N"]] <- N
    state[["Wl"]] <- Wl
    state[["Wsbr"]] <- Wsbr
    return(state)
}
