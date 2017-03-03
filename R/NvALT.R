#<NvALT: automate all N v. ALT comparisons per sample>

NvALT <- function(x){
  
  #source('NvPLAuto.R')
  #source('NvCutAuto.R')
  #source('NvWeibAuto.R')
  #source('NvLgNAuto.R')
  #source('NvExpAuto.R')
  #source('NvPoiAuto.R')
  
  NvPLAutoResults<-NvPLAuto(x) #
  NvCutAutoResults<-NvCutAuto(x) #
  NvWeibAutoResults<-NvWeibAuto(x) #
  NvLgNAutoResults<-NvLgNAuto(x) #
  NvExpAutoResults<-NvExpAuto(x) #
  NvPoiAutoReults<-NvPoiAuto(x) #
  
  NvALT_results<-cbind(
    NvPLAutoResults,
    NvCutAutoResults,
    NvWeibAutoResults,
    NvLgNAutoResults,
    NvExpAutoResults,
    NvPoiAutoReults
  )
  return(NvALT_results)
}
