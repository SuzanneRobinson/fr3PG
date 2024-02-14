#' @export
createPriors_poplar<-function(poplar,prior_distribution = 'uniform', sd=F){
  
  # Update with param names to calibrate
  nm<-c("pFS2","pFS20","aS","nS","pRx","pRn","gammaFx","gammaF0","tgammaF","Rttover",
        "mF","mR","mS","SLA0","SLA1","tSLA","alpha","Y", "MaxCond", "LAIgcx", "m0", 
        "wSx1000","fN0", "MaxIntcptn","k", "fullCanAge", "kF","rhoMin", "rhoMax", "tRho")
  
  f.decrease <- c(
    0.4, # pFS2
    0.01, # pFS20
    0.001, # aS
    2, # nS
    0.3, # pRx
    0.1, # pRn
    0.001, # gammaFx
    0.00001, # gammaF0
    0, # tgammaF
    0.001, # Rttover
    0.001, # mF
    0.1, # mR
    0.1, # mS
    4, # SLA0
    2, # SLA1
    1, # tSLA
    0.01, # alpha
    0.43, # Y
    0.01, # MaxCond 
    2.5, # LAIgcx 
    0.001, # m0
    100, # wSx1000 
    0.1, # fN0 
    0.002, # MaxIntcptn
    0.02, # k 
    0.001, # fullCanAge 
    0.0001, # kF 
    0.2, # rhoMin 
    0.2, # rhoMax 
    1 # tRho 
  ) 
  
  f.increase <-
    c(
      1,# pFS2   1
      1,# pFS20  1
      0.25,# aS
      3,# nS
      1,# pRx   1
      0.4,# pRn
      0.1,# gammaFx
      0.09,# gammaF0
      60,# tgammaF 
      0.16,# Rttover
      0.8,# mF 1
      0.8,# mR 1
      0.5,# mS
      22,# SLA0
      18,# SLA1
      8,# tSLA
      0.1,# alpha
      0.5,# Y
      0.03, # MaxCond 
      4, # LAIgcx
      1,# m0 1
      600, # wSx1000
      1, # fN0 1
      0.39, # MaxIntcptn
      0.6, # k
      15, # fullCanAge
      1, # kF 1
      0.5, # rhoMin
      0.5, # rhoMax
      8 # tRho
    ) 
  
  pMaxima <- f.increase*1.5
  pMinima<- f.decrease*0.5
  
  
  # Set the max as 1 for parameters between [0-1]
  pMaxima[1]<-1
  pMaxima[2]<-1
  pMaxima[5]<-1
  pMaxima[11]<-1
  pMaxima[12]<-1
  pMaxima[21]<-1
  pMaxima[23]<-1
  pMaxima[27]<-1
  
  sdVals<-(pMaxima-pMinima)*0.2

  prDatB<-data.frame(names=nm,sd=sdVals,
                     lower = pMinima, upper = pMaxima)
  prDatB$names<-(prDatB$names)
  prDatB<-prDatB[!duplicated(prDatB$names),]
  
  priorValsX <- createUniformPrior(lower = prDatB$lower, upper =prDatB$upper)
  
  priPlotFunc<-function(prDatBS){
    pp<-createUniformPrior(lower = prDatBS$lower, upper =prDatBS$upper)
    
    ppDat<-data.frame(pp=(pp$sampler(10000)))
    return(
      ggplot(data=ppDat,aes(pp))+
        geom_histogram(bins=50,fill="lightblue", col="darkgray")+
        ggtitle(paste0(prDatBS$names,": 10k prior samples"))
    )
    
  }
  prDatBX<-split(prDatB,seq(nrow(prDatB)))
  
  plots<-lapply(prDatBX,priPlotFunc)
  
  ggarrange(plotlist=plots)
  
  
  
  # Calculate scaling factor
  sc<-rowMeans(abs(cbind(pMinima,pMaxima) ) )
  
  if (prior_distribution == 'uniform') {
    priorVals <- createUniformPrior(lower = pMinima / sc, upper = pMaxima / sc)
  } else if (prior_distribution == 'normal') {
    pValues <- as.vector(unlist(poplar[nm]))
    priorVals <- createTruncatedNormalPrior(mean = pValues / sc, sd = sdVals / sc,
                                            lower = pMinima / sc, upper = pMaxima / sc)
  } else {
    stop("Invalid prior_distribution. Use 'normal' or 'uniform'.")
  }
  
  
  ifelse(sd==F,return(list(priorVals,sc)),return(sdVals))
  
}