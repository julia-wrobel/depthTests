#' A permutation test for testing dispersion
#'
#' This function takes a a control and a test dataset and permforms a permutation test for dispersion
#' @param controlData Data for control subjects, each column must be a subject
#' @param testData Data for test subjects, each column must be a subject
#' @param n_perm number of permutations to run
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' 
#' @export permutationTestdisp
#' 
permutationTestdisp = function(controlData, testData, n_perm){
  allData = cbind(controlData, testData) 
  dispControl=volumf(t(controlData));
  dispTest=volumf(t(testData));
  
  Tdispobs=dispControl/dispTest
  
  totaln=dim(allData)[2] ## number of columns in the combined dataset (tot num of subjects)
  ntest=dim(testData)[2] ## number of columns in the test dataset (num of test subjects)
  
  # Create the permuted samples and compute T statistic for each permutation
  Tpermdisp = rep(NA, n_perm)
  
  for (i in 1 : n_perm) {
    # randomly sample subjects (without replacement!) into new group a
    Aposition = sample(totaln, ntest)
    groupA = allData[,Aposition]
    groupB = allData[,-Aposition]
    
    dispControlA=volumf(t(groupA));
    dispTestB=volumf(t(groupB));
    
    Tpermdisp[i]=dispControlA/dispTestB
  }
  
  if (Tdispobs>1)
    pvaluedisp=sum(Tpermdisp>=Tdispobs)/n_perm
  else
    pvaluedisp=sum(Tpermdisp<=Tdispobs)/n_perm
  
  print (pvaluedisp)
  
  
  #hist(Tperm2)
  Tdispobs = data.frame(Tdispobs = Tdispobs)
  Tperms = data.frame(Tpermdisp = Tpermdisp)
  
  pvalues = data.frame(pvaluedisp = pvaluedisp)
  
  
  return(list(Tdispobs = Tdispobs, Tperms = Tperms, pvalues = pvalues))
}