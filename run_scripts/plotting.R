#  Plot the priors for poplar
plotPriors_poplar<-function(nm, sdVals, pMinima, pMaxima){
  
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
  
  prior_plot<-ggarrange(plotlist=plots)
  
  return(prior_plot)
  
}