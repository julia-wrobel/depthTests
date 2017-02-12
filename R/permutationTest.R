#' Implements permutation test based on band depth calculation
#'
#' This function takes a PCA decomposition of a PET dataset and simulates new images.
#' @param controlData Data for control subjects, each column must be a subject
#' @param testData Data for test subjects, each column must be a subject
#' @param n_perm number of permutations to run
#' 
#' @author Sara Lopez-Pintado \email{sl2929@@columbia.edu},
#' Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' @export permutationTest

permutationTest = function(controlData, testData, n_perm){
  k=9 # number of terms we are adding for calculating Tobsr
  allData = cbind(controlData, testData) 
  
  depthControl <- fMBD(controlData)
  depthTest <- fMBD(testData)
  orderControl=order(depthControl)
  lorc=length(orderControl) ## finds index of median control (median is the deepest)
  
  orderTest=order(depthTest)
  lort=length(orderTest)  ## finds index of median test
  
  medianIndexControl = orderControl[lorc]
  medianIndexTest = orderTest[lort]
  
  medianControl=controlData[, medianIndexControl] ## what is this supposed to be returning? This is the median in control group
  medianTest=testData[, medianIndexTest] ## This is the median in the test group
  
  diffmedian=medianControl-medianTest
  diffmean = rowMeans(controlData) - rowMeans(testData)
  Tobs=max(abs(diffmedian)) ## gets max difference between control and test medians
  b=sort(abs(diffmedian))
  Tobsr= sum(b[(length(b)-k):length(b)])# sum of the k+1 pixels with max differences (k=9)
  Tobs2=sum(abs(diffmedian)) ## sums absolute difference between control and test
  Tobs3 = max(abs(diffmean))
  Tobs4 = sum(abs(diffmean))
  
  totaln=dim(allData)[2] ## number of columns in the combined dataset (tot num of subjects)
  ntest=dim(testData)[2] ## number of columns in the test dataset (num of test subjects)
  
  # Create the permuted samples and compute T statistic for each permutation
  Tperm = rep(NA, n_perm)
  Tpermr= rep(NA, n_perm)
  Tperm2 = rep(NA, n_perm)
  Tperm3 = rep(NA, n_perm)
  Tperm4 = rep(NA, n_perm)
  
  totaln=dim(allData)[2] ## number of columns in the combined dataset (tot num of subjects)
  ntest=dim(testData)[2] ## number of columns in the test dataset (num of test subjects)
  
  for (i in 1 : n_perm) {
    # randomly sample subjects (without replacement!) into new group a
    Aposition = sample(totaln, ntest)
    groupA = allData[,Aposition]
    groupB = allData[,-Aposition]
    depthA = fMBD(groupA)
    depthB = fMBD(groupB)
    orderA=order(depthA)
    orderB=order(depthB)
    lorderA=length(orderA)
    lorderB=length(orderB)
    medianIndexA=orderA[lorderA]
    medianIndexB=orderB[lorderB]
    
    mediancontrol=groupA[,medianIndexA]
    mediantest=groupB[,medianIndexB]
    
    
    diffmedian=mediancontrol-mediantest
    diffmean = rowMeans(groupB) - rowMeans(groupA)
    b=sort(abs(diffmedian))
    bmaxv=b[(length(b)-k):length(b)]
    
    Tperm[i]=max(abs(diffmedian))
    Tpermr[i]=sum(bmaxv) #sum of the k+1 pixels with maximum differences
    Tperm2[i]=sum(abs(diffmedian))
    Tperm3[i] = max(abs(diffmean))
    Tperm4[i] = sum(abs(diffmean))
  }
  
  
  pvalue =sum(Tperm >= Tobs) / n_perm
  pvaluer=sum(Tpermr >= Tobsr) / n_perm
  pvalue2=sum(Tperm2>=Tobs2)/n_perm
  
  pvalue3=sum(Tperm3>=Tobs3)/n_perm
  pvalue4=sum(Tperm4>=Tobs4)/n_perm
  
  TobsAll = data.frame(Tobs1 = Tobs, Tobs1r=Tobsr, Tobs2 = Tobs2, Tobs3 = Tobs3, Tobs4 = Tobs4)
  Tperms = data.frame(T.median1 = Tperm, T.median1r=Tpermr, T.median2 = Tperm2, T.mean1 = Tperm3, T.mean2 = Tperm4)
  pvalues = data.frame(pvalue1 = pvalue, pvalue1r=pvaluer, pvalue2 = pvalue2, pvalue3 = pvalue3, pvalue4 = pvalue4)
  
  
  return(list(TobsAll = TobsAll, Tperms = Tperms, pvalues = pvalues))
}
