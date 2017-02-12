#' A function for simulating PET data
#'
#' This function takes a PCA decomposition of a PET dataset and simulates new images.
#' @param PCAobject The PCA breakdown of either your test or control data
#' @param npc Number of principal components to use for simulating data. Defaults to NULL.
#' @param pve Percent Variance explained by PCA decomposition. Defaults to 0.95.
#' @param nsubjs The number of images you want to simulate.
#' @param scoreMean The mean score for the simulated subjects.
#' @param muScale A constant multiplier which scales the mean image.
#' @param muShift A constant for shifting the mean image.
#' @param varscale A constant multiplier for increasing the variance of all eigenvalues
#' 
#' @importFrom stats rnorm
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' 
#' @export simulateBrains
#' 
#' 

simulateBrains = function(PCAobject, npc = NULL, pve = 0.95, nsubjs = 30, scoreMean = 0, muScale = 1, 
                          muShift = 0,varscale=1){
  npixels = dim(PCAobject$CovMat)[1]
  npc = ifelse(is.null(npc), min(which(cumsum(PCAobject$evalues)/sum(PCAobject$evalues) > pve)), npc) 
  
  evalues = PCAobject$evalues[1:npc]
  evalues=varscale*evalues
  efunctions = PCAobject$efunctions[,1:npc]
  mu = PCAobject$mean * muScale + muShift
  
  score.list = lapply(sqrt(evalues), function(lambda) rnorm(nsubjs, scoreMean, lambda) )
  scores = matrix(unlist(score.list), nrow = nsubjs, ncol = npc, byrow = FALSE)
  
  Yhat = matrix(0, nrow = nsubjs, ncol = npixels)
  
  for(i.subj in 1:nsubjs){
    Yhat[i.subj, ] = t(as.matrix(mu)) + scores[i.subj, ] %*%  t(efunctions[, 1:npc ])
  }
  return(list(brains = t(Yhat), meanBrain = mu))
}