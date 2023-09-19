fr3PG<-function( ## Climate input drivers (matrix/DF?)
                weather,
                presc,
                ## Stand initalisation
                t = 0,
                N = 1200,
                Wl = 0,
                WlDormant = 0, 
                Wr = 0,
                Wsbr = 0,
                Wlitt = 0,
                rotation = 1,
                cycle = 1,
                rm.sprouts = F,
                nyears = 100,
                ## Site information
                latitude = 39,
                FR = 0.5,
                soilclass = 0,
                ASW = 100,
                MaxASW = 200,
                MinASW = 0,
                CO2 = 400,
                ## Parameters
                pFS2=0.3,                      
                pFS20=0.09,
                ## pfsPower=-0.522878745280338,
                ## pfsConst=0.43104582317421,
                aS=0.056,
                nS=2.7,
                pRx=0.5,
                pRn=0.25,
                Tmin=6,
                Topt=16,
                Tmax=40,
                kF=1,
                SWconst0=0.7,
                SWpower0=9,
                m0=0,
                fN0=1,
                fNn=0,
                MaxAge=50,
                nAge=4,
                rAge=0.95,
                gammaFx=0.013,
                gammaF0=0.001,
                tgammaF=6,
                Rttover=0.01,
                MaxCond=0.02,
                LAIgcx=3.33,
                BLcond=0.2,
                wSx1000=300,
                thinPower=1.5,
                mF=0,
                mR=0.2,
                mS=0.2,
                SLA0=11,
                SLA1=4,
                tSLA=2.5,
                k=0.5,
                fullCanAge=3,
                MaxIntcptn=0.15,
                LAImaxIntcptn=0,
                alpha=0.055,
                Y=0.47,
                poolFractn=0,
                e20=2.2,
                rhoAir=1.2,
                lambda=2460000,
                VPDconv=0.000622,
                fracBB0=0.75,
                fracBB1=0.15,
                tBB=2,
                rhoMin=0.45,
                rhoMax=0.45,
                tRho=4,
                Qa=-90,
                Qb=0.8,
                gDM_mol=24,
                molPAR_MJ=2.3,
                CoeffCond=0.05,
                fCalpha700=1.4,
                fCg700=0.7,
                fCalphax=2.33333333333333,
                fCg0=1.75,
                MinCond=0,
                Wl.s=0.526,
                Wsbr.s=0.2035,
                Wr.s=0.22775,
                pWl.sprouts=0.5,
                pWsbr.sprouts=0.9,
                leaf.grow = 0, ## Tom Locateli
                leaf.fall = 0, ## Tom Locateli
                cod.pred = "3PG",
                cod.clim = "Average"
                ){
    
    parms<-c(pFS2,pFS20,aS,nS,pRx,pRn,Tmin,Topt,Tmax,kF,SWconst0,SWpower0,m0,fN0,fNn,MaxAge,nAge,rAge,gammaFx,gammaF0,tgammaF,Rttover,MaxCond,LAIgcx,BLcond,wSx1000,thinPower,mF,mR,mS,SLA0,SLA1,tSLA,k,fullCanAge,MaxIntcptn,LAImaxIntcptn,alpha,Y,poolFractn,e20,rhoAir,lambda,VPDconv,fracBB0,fracBB1,tBB,rhoMin,rhoMax,tRho,Qa,Qb,gDM_mol,molPAR_MJ,CoeffCond,fCalpha700,fCg700,fCalphax,fCg0,MinCond,Wl.s,Wsbr.s,Wr.s,pWl.sprouts,pWsbr.sprouts, leaf.grow, leaf.fall)
        
    names(parms)<-c("pFS2","pFS20","aS","nS","pRx","pRn","Tmin","Topt","Tmax","kF","SWconst0","SWpower0","m0","fN0","fNn","MaxAge","nAge","rAge","gammaFx","gammaF0","tgammaF","Rttover","MaxCond","LAIgcx","BLcond","wSx1000","thinPower","mF","mR","mS","SLA0","SLA1","tSLA","k","fullCanAge","MaxIntcptn","LAImaxIntcptn","alpha","Y","poolFractn","e20","rhoAir","lambda","VPDconv","fracBB0","fracBB1","tBB","rhoMin","rhoMax","tRho","Qa","Qb","gDM_mol","molPAR_MJ","CoeffCond","fCalpha700","fCg700","fCalphax","fCg0","MinCond","Wl.s","Wsbr.s","Wr.s","pWl.sprouts","pWsbr.sprouts", "leaf.grow", "leaf.fall")

    vars.ini<-c(t,N,Wl,WlDormant,Wr,Wsbr,Wlitt,rotation,cycle,rm.sprouts,nyears)
    names(vars.ini)<-c("t","N","Wl","WlDormant","Wr","Wsbr","Wlitt","rotation","cycle","rm.sprouts","nyears")
    
    site.info<-c(latitude,FR,soilclass,ASW,MaxASW,MinASW,CO2)
    names(site.info)<-c("latitude","FR","soilclass","ASW","MaxASW","MinASW","CO2")
  

    ## Create list with information to be included in some functions
                          ## Vector with number of days per month  
    parms.general <- list(daysinmonth = c(Jan = 31, Feb = 28,
                                          Mar = 31, Apr = 30,
                                          May = 31, Jun = 30,
                                          Jul = 31, Aug = 31,
                                          Sep = 30, Oct = 31,
                                          Nov = 30, Dec = 31),
                          ## Table with parameters for soil water modifier, depending on the soil type
                          parms.soil = data.frame(soilclass.name = c("Sandy", "Sandy loam",
                                                                     "Clay loam", "Clay",
                                                                     "Non standard", "No effect of ASW"),
                                                  soilclass = c(1, 2, 3, 4, NA, 0),
                                                  SWconst = c(0.7, 0.6, 0.5, 0.4, parms[["SWconst0"]], 1),
                                                  SWpower = c(9, 7, 5, 3, parms[["SWpower0"]], 1)))
    
    proj <- Run3PG(stand.init = vars.ini, weather = weather,
                   site = site.info, general.info = parms.general, presc = presc,
                   parms = parms, cod.pred = cod.pred, cod.clim = cod.clim)

    return(as.data.frame(proj))

}


