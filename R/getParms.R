#' getParms get baseline parameters for poplar; Populus hybrids; Headlee et al. 2013
#' @return list of parameters and values
#' @export
<<<<<<< HEAD
getParms_poplar<- function(weather = clm.site,
=======
getParms_poplar<- function(weather = clm.tadcaster,
>>>>>>> 86aa7db3d7c67afbfd30a0fde7f41d02a654789f
                     ## ~~ Initial pools ~~ ##
                     t = 0,
                     Wl = 0.01,
                     WlDormant = 0.01, 
                     Wr = 0.01,
                     Wsbr = 0.1,
                     Wlitt = 0,
                     rotation = 1,
                     cycle = 1,
                     rm.sprouts = FALSE,
                     nyears = 28,
                     ## ~~ Site ~~ ##
                     N = 156, # 6.4 m along rows, 10 m spacing between rows
                     latitude = 53.44,
<<<<<<< HEAD
                     FR = 0.3, #### Update !!
=======
                     FR = 0.5, #### Update !!
>>>>>>> 86aa7db3d7c67afbfd30a0fde7f41d02a654789f
                     soilclass = 3, # changed from 0 to clay loam (site description clay loam over limestone)
                     ASW = 100,
                     MaxASW = 200,
                     MinASW = 0,
                     CO2 = 400,
                     ## ~~ Parameters ~~ ##
                     pFS2 = 0.71,                      
                     pFS20 = 0.12,
                     aS = 0.081,
                     nS = 2.46,
                     pRx = 0.7,
                     pRn = 0.17,
                     Tmin = 10, #### Update !!
                     Topt = 30, #### Update !!
                     Tmax = 48, # These seem high for UK ? Or species specific
                     kF = 0,
<<<<<<< HEAD
                     SWconst0 = 0.7,#### Update if using soilclass=0
                     SWpower0 = 9, #### Update if using soilclass=0
=======
                     SWconst0 = 0.7,#### Update !!
                     SWpower0 = 9, #### Update !!
>>>>>>> 86aa7db3d7c67afbfd30a0fde7f41d02a654789f
                     m0 = 1,
                     fN0 = 0.26,
                     fNn = 1,
                     MaxAge = 50,
                     nAge = 4,
                     rAge = 0.95,
                     gammaFx = 0.01,
                     gammaF0 = 0.083,
                     tgammaF = 18,
                     Rttover = 0.02,
                     MaxCond = 0.02,
                     LAIgcx = 2.6,
                     BLcond = 0.05,
                     wSx1000 = 500,
                     thinPower = 1.45,
                     mF = 0,
                     mR = 0.2,
                     mS = 0.2,
                     SLA0 = 19,
                     SLA1 = 10,
                     tSLA = 5,
                     k = 0.779,
                     fullCanAge = 5,
                     MaxIntcptn = 0.24,
                     LAImaxIntcptn = 7.3,
                     alpha = 0.08,
                     Y = 0.43, #### Should this be 0.47?
                     poolFractn = 0,
                     e20 = 2.2,
                     rhoAir = 1.2,
                     lambda = 2460000,
                     VPDconv = 0.000622,
                     fracBB0 = 0.64,
                     fracBB1 = 0.24,
                     tBB = 3,
                     rhoMin = 0.39,
                     rhoMax = 0.35,
                     tRho = 2,
                     Qa = -90,
                     Qb = 0.8,
                     gDM_mol = 24,
                     molPAR_MJ = 2.3,
                     CoeffCond = 0.05,
                     fCalpha700 = 1.4,
                     fCg700 = 0.7,
                     fCalphax = 2.33333333333333,
                     fCg0 = 1.75,
                     MinCond = 0,
                     Wl.s = 0.526,
                     Wsbr.s = 0.2035,
                     Wr.s = 0.22775,
                     pWl.sprouts = 0.5,
                     pWsbr.sprouts = 0.9,
                     leaf.grow = 4, ## Tom Locatelli
                     leaf.fall = 11, ## Tom Locatelli
                     cod.pred = "3PG",
                     cod.clim = "Average"
) {
  parms_list <- list(
    weather = weather,
    ## ~~ Initial pools ~~ ##
    t = t,
    Wl = Wl,
    WlDormant = WlDormant, 
    Wr = Wr,
    Wsbr = Wsbr,
    Wlitt = Wlitt,
    rotation = rotation,
    cycle = cycle,
    rm.sprouts = rm.sprouts,
    nyears = nyears,
    ## ~~ Site ~~ ##
    N = N,
    latitude = latitude,
    FR = FR,
    soilclass = soilclass, 
    ASW = ASW,
    MaxASW = MaxASW,
    MinASW = MinASW,
    CO2 = CO2,
    ## ~~ Parameters ~~ ##
    pFS2 = pFS2,                      
    pFS20 = pFS20,
    aS = aS,
    nS = nS,
    pRx = pRx,
    pRn = pRn,
    Tmin = Tmin,
    Topt = Topt,
    Tmax = Tmax,
    kF = kF,
    SWconst0 = SWconst0,
    SWpower0 = SWpower0,
    m0 = m0,
    fN0 = fN0,
    fNn = fNn,
    MaxAge = MaxAge,
    nAge = nAge,
    rAge = rAge,
    gammaFx = gammaFx,
    gammaF0 = gammaF0,
    tgammaF = tgammaF,
    Rttover = Rttover,
    MaxCond = MaxCond,
    LAIgcx = LAIgcx,
    BLcond = BLcond,
    wSx1000 = wSx1000,
    thinPower = thinPower,
    mF = mF,
    mR = mR,
    mS = mS,
    SLA0 = SLA0,
    SLA1 = SLA1,
    tSLA = tSLA,
    k = k,
    fullCanAge = fullCanAge,
    MaxIntcptn = MaxIntcptn,
    LAImaxIntcptn = LAImaxIntcptn,
    alpha = alpha,
    Y = Y,
    poolFractn = poolFractn,
    e20 = e20,
    rhoAir = rhoAir,
    lambda = lambda,
    VPDconv = VPDconv,
    fracBB0 = fracBB0,
    fracBB1 = fracBB1,
    tBB = tBB,
    rhoMin = rhoMin,
    rhoMax = rhoMax,
    tRho = tRho,
    Qa = Qa,
    Qb = Qb,
    gDM_mol = gDM_mol,
    molPAR_MJ = molPAR_MJ,
    CoeffCond = CoeffCond,
    fCalpha700 = fCalpha700,
    fCg700 = fCg700,
    fCalphax = fCalphax,
    fCg0 = fCg0,
    MinCond = MinCond,
    Wl.s = Wl.s,
    Wsbr.s = Wsbr.s,
    Wr.s = Wr.s,
    pWl.sprouts = pWl.sprouts,
    pWsbr.sprouts = pWsbr.sprouts,
    leaf.grow = leaf.grow,
    leaf.fall = leaf.fall,
    cod.pred = cod.pred,
    cod.clim = cod.clim
  )
  
  return(parms_list)
}
