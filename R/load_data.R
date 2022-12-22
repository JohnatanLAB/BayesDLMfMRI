#' @name get_example_fMRI_data
#' @title get_example_fMRI_data
#' @description
#' This function is used to download the example data used in the Vignettes. 
#' @details
#' By default this function 
#' @param save_path location where the data the example data is stored.
#' @param force force the download, even if the data already exists.
#' @export
get_example_fMRI_data <- function(save_path="./example_data", force=FALSE) {
  
  URL_1 <- "https://johnatanlab.github.io/files/test_1.rds"
  URL_2 <- "https://johnatanlab.github.io/files/test_2.rds"
  URL_3 <- "https://johnatanlab.github.io/files/test_3.rds"
  
  dir.create(save_path, showWarnings = FALSE)
  
  path_1 <- file.path(save_path,"test_1.rds")
  path_2 <- file.path(save_path,"test_2.rds")
  path_3 <- file.path(save_path,"test_3.rds")
  
  if( (!file.exists(path_1)) & (!force)) {
    download.file(URL_1, destfile=path_1, quiet=FALSE)
  }
  
  if( (!file.exists(path_2)) & (!force)) {
    download.file(URL_2, destfile=path_2, quiet=FALSE)
  }
  
  if( (!file.exists(path_3)) & (!force)) {
    download.file(URL_3, destfile=path_3, quiet=FALSE)
  }
  
  d1 <- readRDS(path_1)
  d2 <- readRDS(path_2)
  d3 <- readRDS(path_3)
  
  fMRI.data <- abind::abind(d1, d2, d3, along = 1)
  
  # tehe file is too big, to be upload to CRAN
  # data("data_test", package="BayesDLMfMRI")
  
  return(fMRI.data)
  # return(m_data)
}

#' @name get_example_covariates
#' @title get_example_covariates
#' @description
#' This function is used to load the example covariates used in the Vignettes. 
#' @export
get_example_covariates <- function() {
  
  # m_data <- system.file( file.path("test_data",  "covariates.Rds"),  
  #                        package="BayesDLMfMRI") |> 
  #           readRDS()
  
  # m_data <- data("covariates.Rds", package="BayesDLMfMRI")
  # m_data <- data("covariates", package="BayesDLMfMRI")
  data("covariates", package="BayesDLMfMRI")
  
  return(Covariates)
  
  # return(m_data)
  
}

