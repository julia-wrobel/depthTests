#' A permutation test for testing dispersion
#'
#' This function computes an alternative dispersion measure for a sample of functions or images.
#' The output is a positive scalar that measures the dispersion of the data.
#' @param Data A matrix of functions where each row is an observed function or image
#' @param pr ask Sara
#' @author Sara Lopez-Pintado \email{sl2929@@columbia.edu}
#' 

scalep<-function(Data,pr){
  
  if (is.data.frame(Data)) 
    Data<-as.matrix(Data)
  n<-nrow(Data) # the rows are the subjects
  m<-ncol(Data) # the columns are the pixels
  if (is.null(n) || is.null(m)) stop("Input must be a matrix")
  
  # First step is to rank the images from center-outward
  depth <- fMBD(t(Data))
  order=order(depth)
  #lorc=length(order) ## finds index of median control (median is the deepest)
  pf=round(n*pr)
  Dataranked=Data[order,]
  Datatrimmed=Dataranked[(n-pf+1):n,]
  minData <- apply(Datatrimmed,2,min)
  maxData <- apply(Datatrimmed,2,max)
  scalep=sum(maxData-minData)
  
  return(scalep)
}
