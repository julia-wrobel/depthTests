#' modified band depth calculation for fda
#'  
#' Method for modified band depth (fMBD) calculation
#' 
#' @param x name of dataset
#' @param xRef Edit this
#' 
#' @author Ying Sun and Marc G.Genton
#'

MBD<- function(x, xRef=NULL)
{
  n <- nrow(x); d <- ncol(x) # n: number of observations (samples);  d: dimension of the data
  x <- as.matrix(x)
  
  if (length(xRef)==0) {  ## MBD with respect to the same sample
    
    ## depth computation
    if (ncol(x) == 1) {x <- t(x)}
    depth <- matrix(0,1,n)
    ordered.matrix <- x
    if (n>1) {
      for (columns in 1:d) {
        ordered.matrix[,columns] <- sort(x[,columns])
        for (element in 1:n) {
          index.1 <- length(which(ordered.matrix[,columns] < x[element,columns]))
          index.2 <- length(which(ordered.matrix[,columns] <= x[element,columns]))
          multiplicity <- index.2 - index.1
          depth[element] <- depth[element] + index.1 * (n - (index.2)) + multiplicity * (n - index.2 + index.1) + choose(multiplicity,2)
        }   ### end FOR element
      }  ### end FOR columns
      depth <- depth / (d * choose(n,2) )
    } ## end IF
    if (n==1) {deepest <- x; depth <- 0}
    ordering<-order(depth,decreasing=TRUE)
    
  } ## end IF no reference sample
  
  else {
    xRef <- as.matrix(xRef)
    if (ncol(xRef)!=d) {stop("Dimensions of x and xRef do not match")}
    n0 <- nrow(xRef)
    
    ## depth computations
    if (ncol(x) == 1) {x <- t(x)}
    depth <- matrix(0,1,n)
    ordered.matrix <- xRef
    if (n0>1) {
      for (columns in 1:d) {
        ordered.matrix[,columns] <- sort(xRef[,columns])
        for (element in 1:n) {
          index.1 <- length(which(ordered.matrix[,columns] < x[element,columns]))
          index.2 <- length(which(ordered.matrix[,columns] <= x[element,columns]))
          multiplicity <- index.2 - index.1
          depth[element] <- depth[element] + (index.1 + multiplicity ) * (n0 - index.1 - multiplicity) + multiplicity * ( index.1 + (multiplicity-1)/2)
        }   ### end FOR element
      }   ### end FOR columns
      depth <- depth / (d * choose(n0,2) )
    } ## end IF
    if (n==1) {deepest <- x; depth <- 0}
    ordering<-order(depth,decreasing=TRUE)
    
  }  ## end ELSE
  return(list(ordering=ordering,MBD=depth))
}

