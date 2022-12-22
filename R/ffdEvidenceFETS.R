#' @import pbapply
#' @importFrom  Rcpp evalCpp

#' @name ffdEvidenceFETS
#' @title ffdEvidenceFETS
#' @description
#' This function can be used to build activation maps for task-based fMRI data. 
#' @references
#' \insertRef{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}
#' 
#' \insertRef{cardona2021bayesdlmfmri}{BayesDLMfMRI}
#' @details
#' Every voxel from the 4D array image is clustered with its nearest neighbors. There are as many clusters as voxels in the image. Then, activation maps are obtained by fitting a multivariate dynamic linear model on every cluster of voxels. 
#' The resulting activation evidence measure for every voxel is obtained using the Forward Estimated Trajectories Sampler (FETS) algorithm. To deeply understand the method implemented in this package, a reading of \insertCite{CARDONAJIMENEZ2021107297}{BayesDLMfMRI} and \insertCite{cardona2021bayesdlmfmri}{BayesDLMfMRI} is mandatory.
#' @param ffdc  a 4D array (ffdc[i,j,k,t]) that contains the sequence of MRI images that are meant to be analyzed. (i,j,k) define the position of the observed voxel at time t.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup.
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at t=0 (m=0 is the default value when no prior information is available). For the case of available prior information, m0 can be defined as a pXr matrix, where p is the number of columns in the covariates object and r is the cluster size.
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at t=0 (Cova=100 is the default value when no prior information is available). For the case of available prior information, Cova0 can be defined as a pXp matrix, where p is the number of columns in the covariates object.
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model.
#' @param S0 prior covariance structure among voxels within every cluster at t=0. S0=1 is the default value when no prior information is available and defines an rXr identity matrix. For the case of available prior information, S0 can be defined as an rXr matrix, where r is the common number of voxels in every cluster.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting. N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos1 a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @param perVol helps to define a threshold for the voxels considered in the analysis. For example, Min.vol = 0.10 means that all the voxels with values
#' below to max(ffdc)*perVol can be considered irrelevant and discarded from the analysis.
#' @param Test test type either "LTT" (Average cluster effect) or "JointTest" (Joint effect).
#' @param Ncores a positive integer indicating the number of threads or cores to be used in the computation of the activation maps.
#' @return It returns a list of the type res[[p]][x,y,z], where p represents the column position in
#' the covariates matrix and [x,y,z] represent the voxel position in the brain image.
#' @examples
#' "See the Vignettes for examples."
#' @export
ffdEvidenceFETS = function(ffdc, covariates, m0=0, Cova=100,
                           delta=0.95, S0=1, n0=1, N1=FALSE, 
                           Nsimu1 = 100, Cutpos1=30, r1 = 1, 
                           perVol = 0.10, Test = "LTT", Ncores = NULL, seed=NULL){
  
  if(is.logical(N1)) {
    if(N1==FALSE){N1=dim(ffdc)[4]}
  }
  
  # validation
  Ncores  <- get_n_cores(Ncores)

  validate_input(
    N1=N1, Test=Test, 
    ffdc=ffdc, covariates=covariates,
    n0=n0, Nsimu1=Nsimu1, perVol=perVol,
    Cutpos1=Cutpos1, r1=r1, delta=delta
  )


  #TAKING THE POSITIONS FROM THE 4D IMAGE WITH NON-NULL VALUES 
  posiffd1 <- which(ffdc[,,,1] != 0, arr.ind = TRUE)
  
  
  if(Test == "LTT"){
    #COMPUTING THE EVIDENCE FOR BRAIN ACTIVATION: VOXEL-WISE ANALYSIS
    set.seed(seed)
    ffd.out = pbapply::pbapply(posiffd1, 1, ffdSingleVoxelFETS, covariates, ffdc, m0, Cova,
                               delta, S0, n0, N1, Nsimu1, Cutpos1, Min.vol = perVol*max(ffdc), r1, Test, cl = Ncores)
    
    
    vol.evidence <- list()
    
    
    for(k in 1:(dim(covariates)[2])){
      vol.evidence[[k]] <- array(0, dim(ffdc)[1:3])
    }
    
    
    for(j in 1:dim(covariates)[2]){
      for(ii in 1:dim(posiffd1)[1]){
        vol.evidence[[j]][posiffd1[ii,1], posiffd1[ii,2], posiffd1[ii,3]] <- ffd.out[j, ii]
      }
    }
    
    return(vol.evidence) 
  }
  if(Test == "JointTest"){
    #COMPUTING THE EVIDENCE FOR BRAIN ACTIVATION: VOXEL-WISE ANALYSIS
    set.seed(seed)
    ffd.out = pbapply::pbapply(posiffd1, 1, ffdSingleVoxelFETS, covariates, ffdc, m0, Cova,
                               delta, S0, n0, N1, Nsimu1, Cutpos1, Min.vol = perVol*max(ffdc), r1, Test, cl = Ncores)
    #number of tests from the output of ffdIndividualVoxelLTT  (Joint and marginal)
    Ntest <- 2
    pAux <- dim(covariates)[2]
    vol.evidence <- list()
    
    
    for(k in 1:(dim(covariates)[2]*Ntest)){
      vol.evidence[[k]] <- array(0, dim(ffdc)[1:3])
    }
    
    
    for(j in 1:dim(covariates)[2]){
      for(ii in 1:dim(posiffd1)[1]){
        vol.evidence[[j]][posiffd1[ii,1], posiffd1[ii,2], posiffd1[ii,3]] <- ffd.out[[ii]]$EvidenceJoint[j]
        vol.evidence[[Ntest+j]][posiffd1[ii,1], posiffd1[ii,2], posiffd1[ii,3]] <- ffd.out[[ii]]$EvidenceMargin[j]
      }
    }
    
    return(vol.evidence)
  }
  
  
  
}




