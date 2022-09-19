#' @export
get_example_fMRI_data <- function() {
  
  # m_data <- system.file( file.path("test_data",  "data_test.Rds"),  
  #                        package="BayesDLMfMRI") |> 
  #           readRDS()
  
  # m_data <- data("data_test.Rds", package="BayesDLMfMRI")
  # m_data <- data("data_test", package="BayesDLMfMRI")
  data("data_test", package="BayesDLMfMRI")
  
  
  return(fMRI.data)
  # return(m_data)
}

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

