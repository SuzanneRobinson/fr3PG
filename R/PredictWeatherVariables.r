# Function to predict climatic variables that will be needed 
PredictWeatherVariables <- function(weather){
    # weather: vector with weather information corresponding to the current period
    VPD.Tmax <- 6.1078 * exp(17.269 * weather$Tmax / (237.3 + weather$Tmax))
    VPD.Tmin <- 6.1078 * exp(17.269 * weather$Tmin / (237.3 + weather$Tmin))
    VPD <- (VPD.Tmax - VPD.Tmin) / 2
    weather$VPDx <- VPD.Tmax
    weather$VPDn <- VPD.Tmin
    weather$VPD <- VPD
    return(weather)
}
