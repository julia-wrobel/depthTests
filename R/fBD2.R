#' second type of fast modified band depth calculation for fda
#'  
#' Method for fast modified band depth calculation. Different from fMBD. Find out what motivated this difference.
#' 
#' @param data name of dataset
#' 
#' @author Ying Sun and Marc G.Genton
#'

fBD2=function(data){
  p=dim(data)[1]
  n=dim(data)[2]
  rmat=apply(data,1,rank)
  down=apply(rmat,1,min)-1
  up=n-apply(rmat,1,max)
  (up*down+n-1)/combinat(n,2)
}