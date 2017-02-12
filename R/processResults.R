#' process Results returned from a simulation
#'
#' This function calculates and returns the rejection rate when given a list of simulation results
#' @param resultList Results from a simulation study, invludes pvalues for each simulation step.
#' @param nSims number of simulations that were run. Defaults to 100.
#' @param alpha Type I error rate.
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' 
processResults = function(resultList, nSims = 100, alpha = 0.05){
  p.perm = matrix(unlist(lapply(resultList, function(simNum) simNum$permTest$pvalues )), ncol = 5, nrow = nSims, byrow = TRUE)
  p.rank = matrix(unlist(lapply(resultList, function(simNum) simNum$rankTest$p.value )), ncol = 1, nrow = nSims, byrow = TRUE)
  
  ## calculate % of the time Ho rejected
  rejectionRate.perm = sapply(1:ncol(p.perm), function(i) sum(p.perm[,i] < alpha)/nSims )
  rejectionRate.rank = sum(p.rank < alpha)/nSims
  
  return(list(rate.perm = rejectionRate.perm, rate.rank = rejectionRate.rank))
}
