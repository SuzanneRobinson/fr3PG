# Function to remove sprouts after some time
RemoveSprouts <- function(state, parms, fma){
    # state: vector with state variables
    # parms: vector with parameters of 3-PG model
    # fma: data.frame with the FMA of the current period
    pWl.sprouts <- parms[["pWl.sprouts"]]
    pWsbr.sprouts <- parms[["pWsbr.sprouts"]]
    N <- state[["N"]]
    Nharv <- state[["Nharv"]]
    nsprouts <- unique(fma[, "nsprouts"])
    Nharv <- ifelse(is.na(Nharv), N / nsprouts, Nharv)  # To deal with those cases when rotation = 2, and rm.sprouts = 0, assumed that sprouts are actually not removed
    Wl <- state[["Wl"]]
    Wsbr <- state[["Wsbr"]]
    Nsprouts <- N
    N <- nsprouts * Nharv
    state[["Wl"]] <- Wl - (Nsprouts - N) * Wl / Nsprouts * pWl.sprouts
    state[["Wsbr"]] <- Wsbr - (Nsprouts - N) * Wl / Nsprouts * pWsbr.sprouts
    state[["rm.sprouts"]] <- 1
    state[["N"]] <- N
    return(state)
}
