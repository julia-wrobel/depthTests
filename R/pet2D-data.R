##' Simulated 2-dimensional PET data for control subjects.
##'
##' Simulated two-dimensional PET imaging data. The original data is proprietary, but consists of images from
##' 39 control subjects and 29 subjects with major depressive disorder.
##' The images represent maps of binding potential of 5- hydroxytryptamine (serotonin) 1A receptors (5-HT1A), 
##' which are thought to play an important role in major depressive disorder. We decomposed the 39 control subjects using 
##' principal components analysis (PCA). The data provided here are rebuilt from the resulting eigenvalues and eigenvectors of the PCA 
##' decomposition, along with randomly generated subject-specific scores. 
##'
##' @name pet2D
##' @docType data
##' 
##' @usage data(pet2D)
##' 
##' @format A matrix made up of \describe{ \item{"pet2D"}{A 7505 x 50
##' matrix of simulated PET data. Each column is a vector for one subject which represents a 95 x 79 pixel image. There are 50 subjects in total.}
##' }
##' @references Parsey et al (2006).
##' Altered Serotonin 1A Binding in Major Depression: A [carbonyl-C-11]WAY100635 Positron Emission Tomography Study
##'\emph{Biological Psychiatry}, Vol 59 Issue 2, 106 - 113.
##'
##'  @keywords datasets
##'  
##' @examples
#' data(pet2D)
#' image1 = matrix(pet2D[, 1], nrow = 95, ncol = 79)
#' image(image1)

"pet2D"