#' A permutation test for testing dispersion
#'
#' This function will compute a dispersion measure for a sample of functions/images
#' @param Data A matrix of functions where each row is an observed function/image
#' 
#' @importFrom utils combn
#' @author Sara Lopez-Pintado \email{sl2929@@columbia.edu}
#' 
volumf<-function(Data){
  
  if (is.data.frame(Data)) 
    Data<-as.matrix(Data)
  n<-nrow(Data)
  m<-ncol(Data)
  if (is.null(n) || is.null(m)) stop("Input must be a matrix")
  
  
  comb2 <- combn(1:n, 2)  # These are all the possible combinations of n choose two numbers
  ncomb2 <- ncol(comb2)
  volum <- rep(0,ncomb2)
  
  for (j in 1:ncomb2) {
    funcncomb2 <- Data[comb2[,j],]
    minfunccomb2 <- apply(funcncomb2,2,min)
    maxfunccomb2 <- apply(funcncomb2,2,max)
    volum[j]=sum(maxfunccomb2-minfunccomb2)
  }
  
  volumf=sum(volum)/ncomb2
  return(volumf)
}

