#' internal function from 'fda' package
#' 
#' function used in method for fast modified band depth (MBD) calculation
#' 
#' @param n number of columns in your dataset
#' @param p number of rows in your dataset
#' 
#' @author Ying Sun and Marc G.Genton
#'

combinat=function(n,p){
  if (n<p){combinat=0}
  else {combinat=exp(lfactorial(n)-(lfactorial(p)+lfactorial(n-p)))}
}
