# Function to run 3-PG model for a specific plot, weather and site information
Run3PG <- function(stand.init, weather, site, parms, general.info = parms.general, presc = presc, cod.pred = "3PG", cod.clim = "Average"){
    # stand.init: vector with initial stand variables: t, N, Wl, Wr, Wsbr, ASW, Wlitt
    # weather: data.frame with weather information, where each row represents a month, with the following variables: Month, Tmean, Tmax, Tmin, Rain, SolarRad, FrostDays, VPD -> IMPORTANT: Month will be converted into a number, therefore it should be ordered, to get appropriate numbers for each month
    # site: vector with site variables: latitude, FR, soilclass, MaxASW, MinASW
    # parms: vector with parameters of 3-PG model
    # general.info: list with additional information: number of days per month, and parameter values for different soil classes (needed for soil water model)
    # presc: data.frame with the FMAs for all cycles
    state.init <- InitializeState(stand = stand.init, site = site, parms = parms)
    weather <- PredictWeatherVariables(weather = weather)
    proj.results <- list()
    state <- PredictVariablesInterest.3PG(state = state.init, parms = parms, cod.pred = cod.pred)
    t.proj <- 0
    proj.results[[1]] <- c(t.proj = t.proj, state)
    if(!missing(presc)){  # If a prescription is not provided, the function simply simulates the growth
        fma.c <- presc[presc$cycle == state[["cycle"]], ]
        t.nsprouts <- unique(fma.c[, "t.nsprouts"])
        fst.row.fma.app <- min(c(which(fma.c$t > state[["t"]]), nrow(fma.c)))
        fma.app <- fma.c[fst.row.fma.app:nrow(fma.c), ]
        fma.app$t[which(fma.app$t < state[["t"]])] <- state[["t"]]
    }
    if(cod.clim=="Average"){
        N=stand.init[["nyears"]]*12
    }else if(cod.clim=="Month"){
        N=nrow(weather)
    }
    j=1
    weather.i<-as.numeric()
    for(i in 1:N){
        if(cod.clim=="Average"){
            if(j>12) j=1
            weather.i <- weather[j,]
        }else if(cod.clim=="Month"){
            weather.i <- weather[i, ]
        }
        state.apar <- EstimateAPAR(state = state, weather = weather.i, parms = parms, general.info = general.info)
        state.mods <- CalculateModifiers(state = state.apar, weather = weather.i, site = site, parms = parms, general.info = general.info)
        state.npp <- EstimateNPP(state = state.mods, parms = parms)
        state.asw <- UpdateASW(state = state.npp, weather = weather.i, site = site, parms = parms, general.info = general.info)
        state.walloc <- AllocateBiomass(state = state.asw, site = site, parms = parms, weather = weather.i) ## also requires weather.i, to identify current.month
        state.mort <- EstimateMortality(state = state.walloc, parms = parms)
        state.mort[["t"]] <- state.mort[["t"]] + 1 / 12
        state.end <- PredictVariablesInterest.3PG(state = state.mort, parms = parms, cod.pred = cod.pred)
        if(!missing(presc) && nrow(fma.app) > 0 && ((abs(fma.app$t[1] - state.end[["t"]]) < 1 / 24) | (fma.app$t[1] < state.end[["t"]]))){  # Apply thinnings
            state.end <- DoThinning(state = state.end, parms = parms, fma = fma.app, presc = presc)
            fma.app <- fma.app[-1, ]
            state.end <- PredictVariablesInterest.3PG(state = state.end, parms = parms, cod.pred = cod.pred)
        }
        if(!missing(presc) && state.end[["N"]] == 0){   # Final harvest applied
            if(state.end[["cycle"]] > max(presc$cycle)){
                t.proj <- t.proj + 1 / 12
                proj.results[[i + 1]] <- c(t.proj = t.proj, state.end)
                break
            }
            fma.c <- presc[which(presc$cycle == state.end[["cycle"]]), ]
            t.nsprouts <- unique(fma.c[, "t.nsprouts"])
            fma.app <- fma.c
            it.newsprouts <- 1
            if (state.end[["rotation"]] == 1){  # Create new plantation
                state.end <- CreateNewPlantation(state = state.end, parms = parms, fma = fma.app)
                state.end <- PredictVariablesInterest.3PG(state = state.end, parms = parms, cod.pred = cod.pred)
            }  
        } 
        if(!missing(presc) && state.end[["rotation"]] > 1 && state.end[["t"]] < t.nsprouts){
            # && weather.i[["Month"]] %in% c(4, 5, 6)){  # Create new sprouts
            state.end <- CreateNewSprouts(state = state.end, parms = parms, newsprouts = it.newsprouts)
            state.end <- PredictVariablesInterest.3PG(state = state.end, parms = parms, cod.pred = cod.pred)
            it.newsprouts <- it.newsprouts + 1
        }
        if(!missing(presc) && state.end[["rotation"]] > 1 && state.end[["t"]] > (t.nsprouts - 1 / 24) && state.end[["rm.sprouts"]] == 0){
            state.end <- RemoveSprouts(state = state.end, parms = parms, fma = fma.app)
        }
        t.proj <- t.proj + 1 / 12
        proj.results[[i + 1]] <- c(t.proj = t.proj, state.end)
        state <- state.end
        j=j+1
    }
    proj.df <- data.frame(do.call(rbind, proj.results))
    return(proj.df)
}
