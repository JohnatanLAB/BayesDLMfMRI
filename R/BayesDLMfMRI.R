#' Bayesian Matrix-Variate Dynamic Linear Models for Task-based fMRI Modeling in R
#'
#' The 'BayesDLMfMRI' package performs statistical analysis for task-based functional magnetic resonance imaging (fMRI) data at both individual and group levels. The analysis to detect brain activation at the individual level is based on modeling the fMRI signal using Matrix-Variate Dynamic Linear Models (MDLM). The analysis for the group stage is based on posterior distributions of the state parameter obtained from the modeling at the individual level. In this way, this package offers several R functions with different algorithms to perform inference on the state parameter to assess brain activation for both individual and group stages. Those functions allow for parallel computation when the analysis is performed for the entire brain as well as analysis at specific voxels when it is required.
#' 
#' @section Authors:
#' Maintainer: Carlos Peréz \email{caaperezag@unal.edu.co} \cr
#' Creator: Johnatan Cardona-Jiménez \email{jcardonj@unal.edu.co} \cr
#' Contributor: Isabel Ramírez \email{iscramirezgu@unal.edu.co} 
#' @docType package
#' @name BayesDLMfMRI
#' @useDynLib BayesDLMfMRI , .registration=TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @import mathjaxr
#' @import stats
#' @import utils
NULL
#> NULL
#' @title Covariates related to the observed BOLD response
#' @description
#' Covariates related to the observed BOLD response and its derivative used in the examples presented in the vignettes.
#' @examples
#' data("covariates", package="BayesDLMfMRI")
"Covariates"

#' @title MNI image used to plot posterior probability maps in the vignette examples.
#' @description MNI image used to plot posterior probability maps in the examples presented in the vignettes.
#' @examples
#' data("ffd", package="BayesDLMfMRI")
"ffd"

#' @title A 3D array that works as a brain of reference (MNI atlas).
#' @description A 3D array that works as a brain of reference (MNI atlas) for the group analysis.
#' @examples
#' data("mask", package="BayesDLMfMRI")
"mask"

#' @name summary.fMRI_group_evidence
#' @title summary.fMRI_group_evidence
#' @description
#' Summary function
#' @details
#' Summary function
#' @param res is the returned value of any of the fdGroupEvidence* functions
#' @examples
#' \donttest{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' data("mask", package="BayesDLMfMRI")
#' res <- ffdGroupEvidenceFETS(ffdGroup = DatabaseGroup, covariates = Covariates, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, N1 = FALSE, Nsimu1 = 100, Cutpos=30, 
#'                             r1 = 1, Test = "JointTest", mask = mask, Ncores = 7)
#' summary(res)
#' }
#' @export
print.fMRI_group_evidence  <- function(res) {
    str(res)
}

#' @name print.fMRI_group_evidence
#' @title print.fMRI_group_evidence
#' @description
#' print function
#' @details
#' print function
#' @param res is the returned value of any of the fdGroupEvidence* functions
#' @examples
#' \donttest{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' data("mask", package="BayesDLMfMRI")
#' res <- ffdGroupEvidenceFETS(ffdGroup = DatabaseGroup, covariates = Covariates, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, N1 = FALSE, Nsimu1 = 100, Cutpos=30, 
#'                             r1 = 1, Test = "JointTest", mask = mask, Ncores = 7)
#' print(res)
#' }
#' @export
print.fMRI_group_evidence  <- function(res) {
    str(res)
}

#' @name summary.fMRI_single_evidence
#' @title summary.fMRI_single_evidence
#' @description
#' Summary function
#' @details
#' Summary function
#' @param res is returned value of any of the ffdEvidence* functions
#' @examples
#' \donttest{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' res <- ffdEvidenceFFBS(ffdc = fMRI.data, covariates = Covariates, m0=0, Cova=100,
#'                        delta=0.95, S0=1, n0=1, N1=FALSE, 
#'                        Nsimu1 = 100, Cutpos1=30, r1 = 1,
#'                        perVol = 0.10, Ncores=3)
#' summary(res)
#' }
#' @export
summary.fMRI_single_evidence  <- function(res) {
    str(res)
}


#' @name print.fMRI_single_evidence
#' @title print.fMRI_single_evidence
#' @description
#' Print function
#' @details
#' Print function
#' @param res is returned value of any of the ffdEvidence* functions
#' @examples
#' \donttest{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' res <- ffdEvidenceFFBS(ffdc = fMRI.data, covariates = Covariates, m0=0, Cova=100,
#'                        delta=0.95, S0=1, n0=1, N1=FALSE, 
#'                        Nsimu1 = 100, Cutpos1=30, r1 = 1,
#'                        perVol = 0.10, Ncores=3)
#' print(res)
#' }
#' @export
print.fMRI_single_evidence  <- function(res) {
    str(res)
}


#' @name plot.fMRI_single_evidence
#' @title plot.fMRI_single_evidence
#' @description
#' Plot function
#' @details
#' Plot function
#' @param res is returned value of any of the ffdEvidence* functions.
#' @param overlay MNI image used to plot posterior probability maps. 
#' @param index the element of \code{res} to be plotted.
#' @param ... aditional parameters passed to the \code{ortho2} function.
#' @examples
#' \donttest{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' data("ffd", package="BayesDLMfMRI") # used for overlay.
#' res <- ffdEvidenceFETS(ffdc = fMRI.data,
#'                    covariates = Covariates,
#'                    m0 = 0, Cova = 100, delta = 0.95,
#'                    S0 = 1, n0 = 1, Nsimu1 = 100, Cutpos1 = 30,
#'                    r1 = 1, Test = "LTT", Ncores = 15)
#' plot(res, overlay=ffd, index=1, col.y = heat.colors(50), ycolorbar = TRUE, ybreaks = seq(0.95, 1, by = 0.001))
#' }
#' @export
plot.fMRI_single_evidence  <- function(res, overlay, index, ...) {
    
    if( (index > length(res)) | (1 > index) ) {
        stop("index out of range")
    }

    res.auxi <- res[[index]]

    Z.visual.c <- oro.nifti::nifti(res.auxi, datatype=16)
    neurobase::ortho2(x=overlay, y=ifelse(Z.visual.c > 0.95, Z.visual.c, NA), ...)
}



