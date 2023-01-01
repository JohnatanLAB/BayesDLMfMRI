#' BayesDLMfMRI: Bayesian Matrix-Variate Dynamic Linear Models for Task-based fMRI Modeling in R
#'
#' The BayesDLMfMRI package performs statistical analysis for task-based fMRI data at both individual and group levels. The analysis to detect brain activation at the individual level is based on modeling the fMRI signal using Matrix-Variate Dynamic Linear Models (MDLM). The analysis for the group stage is based on posterior distributions of the state parameter obtained from the modeling at the individual level. In this way, this package offers several R functions with different algorithms to perform inference on the state parameter to assess brain activation for both individual and group stages. Those functions allow for parallel computation when the analysis is performed for the entire brain as well as analysis at specific voxels when it is required.
#' 
#' @section Functions:
#' The BayesDLMfMRI functions
#'
#' @docType package
#' @name BayesDLMfMRI
#' @useDynLib BayesDLMfMRI , .registration=TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @import mathjaxr
#' @import stats
#' @import utils
NULL
#> NULL


#' Covariates
#' example covariates used in the vignettes.
#' @examples
#' data("covariates", package="BayesDLMfMRI")
"Covariates"

#' ffd
#' example overlay used in the vignettes.
#' @examples
#' data("ffd", package="BayesDLMfMRI")
"ffd"