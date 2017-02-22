#' Implements rank-sum test based on band depth calculation
#'
#' This function samples the reference group several times and gets the average p-value of the rank test. we apply the two-sided rank sum test 
#' with null hypothesis that data in the test and control groups are independent samples from identical continuous distributions.
#' The test is equivalent to a Mann-Whitney U-test.
#'  
#' @param controlData Data for control subjects, each column must be a subject
#' @param testData Data for test subjects, each column must be a subject
#' @param size.control Size of experimental group for control data. Choose such that size.control < number of control subjects
#' @param size.test Size of experimental group for test data. Choose such that size.test < number of test subjects
#' 
#' @importFrom stats runif wilcox.test
#' 
#' @author Sara Lopez-Pintado \email{sl2929@@columbia.edu}
#' 
#' @export rankSumTest


rankSumTest <- function(controlData, testData, size.control , size.test)
{

  p <- ncol(controlData);
  N <- nrow(controlData);
  M <- nrow(testData) 
  u <- matrix( runif(N),1,N)
  w <- matrix( runif(M),1,M)
  I <- order(u); J<-order(w)
  n0 <- max(N-size.control, M-size.test); 
  
  if (n0<=max(size.control,size.test)) {stop("Incorrect sample sizes")}
  
  g1 <- controlData[I[1:size.control],] 
  g2 <- testData[J[1:size.test],] 
  
  if ((N-size.control)>=(M-size.test)) {gref <- controlData[I[(size.control+1):N],]}     
  else {gref <- testData[J[(size.test+1):M],] }                
  
  r <- MBD(g1,gref)$MBD; 
  s <- MBD(g2,gref)$MBD; 
  z <- MBD(gref)$MBD     
  
  n1 <-length(r); n2<-length(s);
  g <- cbind(r,s); I <- order(g)
  u <- which(I>n1)
  w <- which(I<=n1)
  p.value <- wilcox.test(u,w)$p.value
  
  Rx<-c();Ry<-c();
  for (i in 1:size.control) {Rx[i]<-sum(z<=r[i])/length(z)}
  for (i in 1:size.test) {Ry[i]<-sum(z<=s[i])/length(z)}
  
  
  R <- order(c(Rx,Ry))
  W <- sum(R[(size.control+1):(size.control+size.test)])

  return(list(p.value=p.value, statistic=W))
}
