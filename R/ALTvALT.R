#<ALTvALT: automate all ALT v. ALT comparisons per sample>

ALTvALT <- function(x){
  
  #source('CutvWeibAuto.R')
  #source('CutvLgNAuto.R')
  #source('CutvExpAuto.R')
  #source('CutvPoiAuto.R')
  #source('WeibvLgNAuto.R')
  #source('WeibvExpAuto.R')
  #source('WeibvPoiAuto.R')
  #source('LgNvExpAuto.R')
  #source('LgNvPoiAuto.R')
  #source('ExpvPoiAuto.R')
  
  CutvWeibAutoResults<-CutvWeibAuto(x) #
  CutvLgNAutoResults<-CutvLgNAuto(x) #
  CutvExpAutoResults<-CutvExpAuto(x) #
  CutvPoiAutoReults<-CutvPoiAuto(x) #
  WeibvLgNAutoResults<-WeibvLgNAuto(x) #
  WeibvExpAutoResults<-WeibvExpAuto(x) #
  WeibvPoiAutoResults<-WeibvPoiAuto(x) #
  LgNvExpAutoResults<-LgNvExpAuto(x) #
  LgNvPoiAutoResults<-LgNvPoiAuto(x) #
  ExpvPoiAutoResults<-ExpvPoiAuto(x) #
  
  ALTvALT_results<-cbind(
    CutvWeibAutoResults,
    CutvLgNAutoResults,
    CutvExpAutoResults,
    CutvPoiAutoReults,
    WeibvLgNAutoResults,
    WeibvExpAutoResults,
    WeibvPoiAutoResults,
    LgNvExpAutoResults,
    LgNvPoiAutoResults,
    ExpvPoiAutoResults
  )
  return(ALTvALT_results)
}
  