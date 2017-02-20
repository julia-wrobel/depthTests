##' PCA Decomposition of 2D PET data for control subjects.
##'
##' PCA decomposition of two-dimensional PET imaging data. The original data is proprietery, but consists of images from
##' 39 control subjects and 29 subjects with major depressive disorder.
##' The images represent maps of binding potential of 5- hydroxytryptamine (serotonin) 1A receptors (5-HT1A), 
##' which are thought to play an important role in major depressive disorder. We decomposed the 39 control subjects using 
##' principal components analysis (PCA). The data provided here are the resulting eigenvalues and eigenvectors of the PCA 
##' decomposition, and are used to create simulated data for our simulation examples. 
##'
##' @name controlPCA
##' @docType data
##' @format A list made up of \describe{
##' \item{efunctions}{A 7505 x 28 matrix
##' of eigenfunctions;}
##' \item{evalues}{Numeric vector of eigenvalues;}
##' \item{mean}{mean image represented as numeric vector;}
##' \item{npc}{scalar number of principal components (28).}
##' }
##' @references Parsey et al (2006).
##' Altered Serotonin 1A Binding in Major Depression: A [carbonyl-C-11]WAY100635 Positron Emission Tomography Study
##'\emph{Biological Psychiatry}, Vol 59 Issue 2, 106 - 113.
NULL