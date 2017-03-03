#<descriptives: Descriptives per variable>

descriptives<- function(x){

  nomiss <- function(x) { x[ !is.na(x) ] }
  
  descriptivesResults<-matrix(,nrow=length(x),ncol=11)
  descriptivesResults<-data.frame(descriptivesResults)
  
  for (i in 1:length(x)) {
    if( sum( nomiss(x[[i]]) <0 ) > 0 ) 
      { descriptivesResults[i,1] <- matrix('Negative values detected',nrow=1,ncol=1) }
    else{ descriptivesResults[i,1] <- matrix('',nrow=1,ncol=1) }
    if( sum( nomiss(x[[i]]) ==0) > 0 ) 
      { descriptivesResults[i,2] <- matrix('zero values detected',nrow=1,ncol=1) }
    else{ descriptivesResults[i,2] <- matrix('',nrow=1,ncol=1) }
    descriptivesResults[i,3] <- matrix( nrow(data.frame(nomiss(x[[i]]))) )
    descriptivesResults[i,4] <- matrix( median(nomiss(x[[i]])) )
    descriptivesResults[i,5] <- matrix( mean(nomiss(x[[i]])) )
    descriptivesResults[i,6] <- matrix( sd(nomiss(x[[i]])) )
    descriptivesResults[i,7] <- matrix( moments::skewness(nomiss(x[[i]])) )
    descriptivesResults[i,8] <- matrix( moments::kurtosis(nomiss(x[[i]])) )
    descriptivesResults[i,9] <- matrix( min(nomiss(x[[i]])) )
    descriptivesResults[i,10] <- matrix( max(nomiss(x[[i]])) )
    descriptivesResults[i,11] <- matrix( (max(nomiss(x[[i]]))-min(nomiss(x[[i]])))/sd(nomiss(x[[i]])) )
  }
    
  colnames(descriptivesResults) <- c( "Negative.values?","zeros?","N","median","mean","SD",
                               "skewness","kurtosis","minimum","maximum","No.of.SDs" )
  return(descriptivesResults)
  
}
