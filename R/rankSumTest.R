#' Implements rank-sum test based on band depth calculation
#'
#' This function samples the reference group several times and gets the average p-value of the rank test.
#' @param controlData Data for control subjects, each column must be a subject
#' @param testData Data for test subjects, each column must be a subject
#' @param n Edit this documentation
#' @param m Edit this documentation
#' 
#' @importFrom stats runif wilcox.test
#' @author Sara Lopez-Pintado \email{sl2929@@columbia.edu}
#' @export rankSumTest

## need to rename n and m and figuure out how those actually should be calculated
rankSumTest <- function(controlData, testData, n, m)
{
  #Input:
  
  # are subjects meant to be in columns or in rows? In rows
  # controlData = matrix of data from group 1 control = controlData
  # y = matrix of data from group 2 test
  # n = size of test group 1 control  #  20
  # m = size of test group 2  test # 29
  
  #Output:
  # pvalue = the p-value for the test
  # statistic = the value of the test statistic
  
  #set.seed(seed); #controlData<-as.matrix(controlData); testData<-as.matrix(testData)
  p <- ncol(controlData); N <- nrow(controlData); M <- nrow(testData)  #size of each data group 
  u <- matrix( runif(N),1,N)
  w <- matrix( runif(M),1,M)
  I <- order(u); J<-order(w)
  n0 <- max(N-n, M-m); 
  
  if (n0<=max(n,m)) {stop("Incorrect sample sizes")}
  g1 <- controlData[I[1:n],] #Randomly select n and m observations from each group to create the test groups
  g2 <- testData[J[1:m],] 
  
  if ((N-n)>=(M-m)) {gref <- controlData[I[(n+1):N],]}     # Consider the remaining observations from group 1 as the reference group
  else {gref <- testData[J[(m+1):M],] }                 # Consider the remaining observations from group 2 as the reference group
  
  r <- MBD(g1,gref)$MBD; # calculate the modified band depth of test group 1 with respect to reference group
  s <- MBD(g2,gref)$MBD; # calculate the modified band depth of test group 2 with respect to reference group
  z <- MBD(gref)$MBD     # calculate the modified band depth of reference group
  
  # we apply the two-sided rank sum test with null hypothesis that data in the vectors r and s are independent samples 
  # from identical continuous distributions. The test is equivalent to a Mann-Whitney U-test.
  
  n1 <-length(r); n2<-length(s);
  g <- cbind(r,s); I <- order(g)
  u <- which(I>n1)
  w <- which(I<=n1)
  p.value <- wilcox.test(u,w)$p.value
  
  Rx<-c();Ry<-c();
  for (i in 1:n) {Rx[i]<-sum(z<=r[i])/length(z)}
  for (i in 1:m) {Ry[i]<-sum(z<=s[i])/length(z)}
  #print(Rx);print(Ry)
  R <- order(c(Rx,Ry))
  W <- sum(R[(n+1):(n+m)])
  #print(R)
  #print(W)
  #print(p.value)
  
  return(list(p.value=p.value, statistic=W))
}
