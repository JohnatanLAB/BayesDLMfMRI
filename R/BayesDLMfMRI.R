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