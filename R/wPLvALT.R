#<wPLvALT: automate all wPL v. ALT comparisons per sample>

wPLvALT <- function(x){
  
  #source('wPLvCutAuto.R') 
  #source('wPLvWeibAuto.R')
  #source('wPLvLgNAuto.R')
  #source('wPLvExpAuto.R')
  #source('wPLvPoiAuto.R')
  
  wPLvCutAutoResults<-wPLvCutAuto(x) 
  wPLvWeibAutoResults<-wPLvWeibAuto(x) 
  wPLvLgNAutoResults<-wPLvLgNAuto(x) 
  wPLvExpAutoResults<-wPLvExpAuto(x) 
  wPLvPoiAutoReults<-wPLvPoiAuto(x) 
  
  wPLvALT_results<-cbind(
    wPLvCutAutoResults,
    wPLvWeibAutoResults,
    wPLvLgNAutoResults,
    wPLvExpAutoResults,
    wPLvPoiAutoReults
  )
  
  return(wPLvALT_results)
}
