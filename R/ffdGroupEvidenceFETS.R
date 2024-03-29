#' @import pbapply

#' @name ffdGroupEvidenceFETS
#' @title ffdGroupEvidenceFETS
#' @description
#' \loadmathjax
#' This function can be used to build activation maps for group task-based fMRI data.
#' @references
#' \insertRef{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}
#' 
#' \insertRef{cardona2021bayesdlmfmri}{BayesDLMfMRI}
#' @details
#' A multivariate dynamic linear model is fitted in the same fashion as at the individual level for every subject in the sample. 
#' However, at this stage, the posterior distributions from all the subjects are combined to build a single one, 
#' which is then employed to compute the activation evidence maps for the group using Forward estimated trajectories sampler (FETS) algorithm. To deeply understand the method implemented in this package, a reading of \insertCite{CARDONAJIMENEZ2021107297}{BayesDLMfMRI} and \insertCite{cardona2021bayesdlmfmri}{BayesDLMfMRI} is mandatory.
#' @param ffdGroup list of N elements, each being a 4D array (\code{ffdc[i,j,k,t]}) that contains the sequence of MRI images related to each of the N subjects in the sample.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup.
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at \code{t=0} (\code{m=0} is the default value when no prior information is available). For the case of available prior information, \code{m0} can be defined as a \mjseqn{p\times q} matrix, where \mjseqn{p} is the number of columns in the covariates object and \mjseqn{q} is the cluster size.
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at \code{t=0} (\code{Cova=100} is the default value when no prior information is available). For the case of available prior information, \code{Cova} can be defined as a pXp matrix, where p is the number of columns in the covariates object.
#' @param delta a discount factor related to the evolution variances. Recommended values between \code{0.85<delta<1}. \code{delta=1} will yield results similar to the classical general linear model.
#' @param S0 prior covariance structure between pair of voxels within every cluster at \code{t=0}. \code{S0=1} is the default value when no prior information is available and defines an \mjseqn{q\times q} identity matrix. For the case of available prior information, \code{S0} can be defined as an \mjseqn{q \times q} matrix, where \mjseqn{q} is the common number of voxels in every cluster.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix \code{S0} at \mjseqn{t=0} (\code{n1=1} is the default value when no prior information is available). For the case of available prior information, \code{n0} can be set as \code{n0=np}, where \code{np} is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (\code{2<N1<T}) from the \code{ffdc} array employed in the model fitting. \code{N1=NULL} (or equivalently \code{N1=T}) is its default value, taking all the images in the \code{ffdc} array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (\code{30<Cutpos1<T}).  
#' @param r1 positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different \code{r1} values: \mjseqn{r1 = 0, 1, 2, 3, 4}, which leads to \mjseqn{q = 1, 7, 19, 27, 33}, where \mjseqn{q} is the size of the cluster.
#' @param Test test type either \code{"LTT"} (Average cluster effect) or \code{"JointTest"} (Joint effect).
#' @param mask a 3D array that works as a brain of reference (MNI atlas) for the group analysis.
#' @param Ncores a positive integer indicating the number of threads or cores to be used in the computation of the activation maps.
#' @return It returns a list of \mjseqn{2 \times p} elements, where \mjseqn{p} is the number of covariates, and 2 is the number 
#' of options evaluated as sampler distributions: Average cluster effect and Marginal effect (when \code{Test=="LTT"}) or Joint effect and Marginal effect (when \code{Test=="JointTest"}). The first \code{p} elements from the list are 
#' the activation maps related to each column of the covariates matrix respectively when computing the activation evidence using either
#' \code{Test=="LTT"} or \code{Test=="JointTest"}. The remaining activation maps are those associated with the marginal distribution.
#' @examples
#'\dontrun{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' data("mask", package="BayesDLMfMRI")
#' res <- ffdGroupEvidenceFETS(ffdGroup = DatabaseGroup, covariates = Covariates, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, N1 = FALSE, Nsimu1 = 100, Cutpos=30, 
#'                             r1 = 1, Test = "JointTest", mask = mask, Ncores = 7)
#' str(res)
#' }
#' @export
ffdGroupEvidenceFETS <- function(ffdGroup, covariates, m0=0, Cova=100,
                                delta = 0.95, S0 = 1, n0 = 1, 
                                N1 = FALSE, Nsimu1=100, 
                                Cutpos=30, r1, Test, mask, Ncores = NULL){
  
  # browser()
  
  if(is.logical(N1)) {
    if(N1==FALSE){N1 = dim(covariates)[1]}
  }

  # validation
  Ncores  <- .get_n_cores(Ncores)

  .validate_input(
    ffdGroup=ffdGroup,
    covariates=covariates,
    delta=delta,
    n0=n0,
    N1=N1,
    Nsimu1=Nsimu1,
    Cutpos1=Cutpos,
    r1=r1,
    Test=Test
  )
  
  covariates <- as.matrix(covariates)
  
  #TAKING THE POSITIONS FROM THE 4D IMAGE WITH NON-NULL VALUES 
  posiffd <- which(mask[,,] != 0, arr.ind = TRUE)
  
  if(Test == "LTT"){
    ffd.out <- pbapply::pbapply(posiffd, 1, .ffdGroupVoxelFETS, ffdGroup, covariates, m0, Cova,
                                delta, S0, n0, N1, Nsimu1, r1, Test, Cutpos, cl = Ncores)
    
    vol.evidence <- list()
    
    for(k in 1:(dim(covariates)[2])){
      vol.evidence[[k]] <- array(0, dim(mask))
    }
    
    
    for(j in 1:(dim(covariates)[2])){
      for(i in 1:dim(posiffd)[1]){
        vol.evidence[[j]][posiffd[i,1], posiffd[i,2], posiffd[i,3]] <- ffd.out[j, i]
      }
    }

    attr(vol.evidence, "class") <- "fMRI_group_evidence"
    return(vol.evidence)
    
  }
  if(Test == "JointTest"){
    
    ffd.out = pbapply::pbapply(posiffd, 1, .ffdGroupVoxelFETS, ffdGroup, covariates, m0, Cova,
                               delta, S0, n0, N1, Nsimu1, r1, Test, Cutpos, cl = Ncores)
    
    Ntest <- 2
    pAux <- dim(covariates)[2]
    vol.evidence <- list()
    
    
    for(k in 1:(dim(covariates)[2]*Ntest)){
      vol.evidence[[k]] <- array(0, dim(mask)[1:3])
    }
    
    
    for(j in 1:dim(covariates)[2]){
      for(ii in 1:dim(posiffd)[1]){
        vol.evidence[[j]][posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenceJoint[j]
        vol.evidence[[Ntest+j]][posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenceMargin[j]
      }
    }
    
    attr(vol.evidence, "class") <- "fMRI_group_evidence"

    return(vol.evidence)
    
  }
  
  
}
    
  









