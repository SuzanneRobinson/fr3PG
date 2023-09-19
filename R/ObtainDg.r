# 5.1.Auxiliary functions----
# Function to estimate quadratic mean diameter from N ang G
ObtainDg <- function(N, G){  # Predictions in centimetres
    if(N <= 0 | G <= 0){
        dg <- 0
    } else {
        dg <- sqrt((G * 4) / (pi * N)) * 100
    }
    return(dg)
}
