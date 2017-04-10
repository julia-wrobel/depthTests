#' A permutation test for testing dispersion
#'
#' This function takes a a control and a test dataset and permforms a permutation test for dispersion
#' @param controlData Data for control subjects, each column must be a subject
#' @param testData Data for test subjects, each column must be a subject
#' @param n_perm number of permutations to run
#' @param pr ask Sara
#' @param var ask Sara
#' 
#' @author Sara Lopez-Pintado \email{sl2929@@columbia.edu}
#' 
#' @export permutationTestdisp
#' 
permutationTestdisp = function(controlData, testData, n_perm, pr, var){
  
  allData = cbind(controlData, testData) 
  
  # First way of computing dispersion
  dispControl=volumf(t(controlData));
  dispTest=volumf(t(testData));
  
  #Second way of computing dispersion fixing p
  dispControl2=scalep(t(controlData),pr);
  dispTest2=scalep(t(testData),pr);
  
  #Third way of computing dispersion by just integrating/adding the variances at each pixel
  varControl=sum(apply(t(controlData),2,var));
  varTest=sum(apply(t(testData),2,var));
  
  Tdispobs=dispControl/dispTest;
  Tdispobs2=dispControl2/dispTest2;
  Tdispobs3=varControl/varTest;
  
  totaln=dim(allData)[2] 
  ntest=dim(testData)[2] 
  
  Tpermdisp = Tpermdisp2 = Tpermdisp3 = rep(NA, n_perm)

  for (i in 1 : n_perm) {
    # randomly sample subjects (without replacement!) into new group a
    Aposition = sample(totaln, ntest)
    groupA = allData[,Aposition]
    groupB = allData[,-Aposition]
    
    dispControlA=volumf(t(groupA));
    dispTestB=volumf(t(groupB));
    
    dispControlA2=scalep(t(groupA),pr);
    dispTestB2=scalep(t(groupB),pr);
    
    varControl=sum(apply(t(groupA),2,var));
    varTest=sum(apply(t(groupB),2,var));
    
    Tpermdisp[i]=dispControlA/dispTestB
    Tpermdisp2[i]=dispControlA2/dispTestB2
    Tpermdisp3[i]=varControl/varTest
  }
  

  if (Tdispobs>1){
    pvaluedispa=sum(Tpermdisp>=Tdispobs)/n_perm
    pvaluedispb=sum(Tpermdisp<=(1/Tdispobs))/n_perm
    pvaluedisp=pvaluedispa+pvaluedispb}
  
  else if (Tdispobs<=1){ 
    pvaluedispa=sum(Tpermdisp<=Tdispobs)/n_perm
    pvaluedispb=sum(Tpermdisp>=(1/Tdispobs))/n_perm
    pvaluedisp=pvaluedispa+pvaluedispb}

  
  if (Tdispobs2>1){
    pvaluedisp2a=sum(Tpermdisp2>=Tdispobs2)/n_perm
    pvaluedisp2b=sum(Tpermdisp2<=(1/Tdispobs2))/n_perm
    pvaluedisp2= pvaluedisp2a+ pvaluedisp2b}
  else if(Tdispobs2<=1){
    pvaluedisp2a=sum(Tpermdisp2<=Tdispobs2)/n_perm
    pvaluedisp2b=sum(Tpermdisp2>=(1/Tdispobs2))/n_perm
    pvaluedisp2= pvaluedisp2a+ pvaluedisp2b}
  
  if (Tdispobs3>1){
    pvaluedisp3a=sum(Tpermdisp3>=Tdispobs3)/n_perm
    pvaluedisp3b=sum(Tpermdisp3<=(1/Tdispobs3))/n_perm
    pvaluedisp3= pvaluedisp3a+ pvaluedisp3b}
  else if(Tdispobs3<=1){
    pvaluedisp3a=sum(Tpermdisp3<=Tdispobs3)/n_perm
    pvaluedisp3b=sum(Tpermdisp3>=(1/Tdispobs3))/n_perm
    pvaluedisp3= pvaluedisp3a+ pvaluedisp3b}
  
  Tdispobs = data.frame(Tdispobs = Tdispobs)
  Tperms = data.frame(Tpermdisp = Tpermdisp)
  
  pvalues = data.frame(pvaluedisp = pvaluedisp)
  
  Tdispobs2 = data.frame(Tdispobs2 = Tdispobs2)
  Tperms2 = data.frame(Tpermdisp2 = Tpermdisp2)
  
  pvalues2 = data.frame(pvaluedisp2 = pvaluedisp2)
  Tdispobs3 = data.frame(Tdispobs3 = Tdispobs3)
  Tperms3 = data.frame(Tpermdisp3 = Tpermdisp3)
  pvalues3 = data.frame(pvaluedisp3 = pvaluedisp3)
  
  
  
  return(list(Tdispobs = Tdispobs, Tperms = Tperms, pvalues = pvalues, 
              Tdispobs2 = Tdispobs2, Tperms2 = Tperms2, pvalues2 = pvalues2,
              Tdispobs3 = Tdispobs3, Tperms3 = Tperms3, pvalues3 = pvalues3))
}
