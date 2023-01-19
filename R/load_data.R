#' @name get_example_fMRI_data
#' @title get_example_fMRI_data
#' @description
#' This function is used to download the example data used in the Vignettes. 
#' @references
#' \insertRef{pernet2015human}{BayesDLMfMRI}
#' 
#' \insertRef{gorgolewski2017openneuro}{BayesDLMfMRI}
#' @details
#' The data for this example is related to an fMRI experiment where a sound stimulus is presented. 
#' That experiment is intended to offer a "voice localizer" scan, which allows rapid and reliable 
#' localization of the voice-sensitive "temporal voice areas" (TVA) of the human auditory cortex
#' \insertCite{pernet2015human}{BayesDLMfMRI}. The data of this "voice localizer" scan is freely  
#' available on the online platform OpenNEURO \insertCite{gorgolewski2017openneuro}{BayesDLMfMRI}.
#' @param save_path location where the data the example data is stored.
#' @param force force the download, even if the data already exists.
#' @return It returns an array of dimensions \code{[91, 109, 91, 310]}.
#' @examples
#' \dontrun{
#' fMRI.data  <- get_example_fMRI_data()
#' }
#' @export
get_example_fMRI_data <- function(save_path=NULL, force=FALSE) {
  
  if(is.null(save_path)) {
    save_path  <- tempdir()
  }

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
  
  temp <- oro.nifti::dim_
  
  fMRI.data <- abind::abind(d1, d2, d3, along = 1)
  

  
  return(fMRI.data)
}


#' @name get_example_fMRI_data_group
#' @title get_example_fMRI_data_group
#' @description
#' This function is used to download the example data used in the Vignettes. 
#' @references
#' \insertRef{pernet2015human}{BayesDLMfMRI}
#' 
#' \insertRef{gorgolewski2017openneuro}{BayesDLMfMRI}
#' @details
#' The data for this example is related to an fMRI experiment where a sound stimulus is presented. 
#' That experiment is intended to offer a "voice localizer" scan, which allows rapid and reliable 
#' localization of the voice-sensitive "temporal voice areas" (TVA) of the human auditory cortex
#' \insertCite{pernet2015human}{BayesDLMfMRI}. The data of this "voice localizer" scan is freely  
#' available on the online platform OpenNEURO \insertCite{gorgolewski2017openneuro}{BayesDLMfMRI}.
#' @param save_path location where the data the example data is stored.
#' @param force force the download, even if the data already exists.
#' @return It returns an array of dimensions \code{[91, 109, 91, 310]}.
#' @examples
#' \dontrun{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' }
#' @export
get_example_fMRI_data_group  <- function(save_path=NULL, force=FALSE) {


}