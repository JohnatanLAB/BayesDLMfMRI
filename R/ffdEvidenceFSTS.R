#' @import pbapply
#' @importFrom Rdpack reprompt

#' @name ffdEvidenceFSTS
#' @title ffdEvidenceFSTS
#' @description
#' This function can be used to build activation maps for task-based fMRI data. 
#' @references
#' \insertRef{jimenez_bayesdlmfmri_2021}{BayesDLMfMRI}
#' @details
#' Every voxel from the 4D array image is clustered with its nearest neighbors. There are as many clusters as voxels in the image. Then, activation maps are obtained by fitting a multivariate dynamic linear model on every cluster of voxels. The resulting activation evidence measure for every voxel is obtained by using the Forward State Trajectories Sampler (FSTS) algorithm \insertCite{jimenez_bayesdlmfmri_2021}{BayesDLMfMRI}.
#' @param ffdc a 4D array (ffdc[i,j,k,t]) that contains the sequence of MRI images that are meant to be analyzed. (i,j,k) define the position of the voxel observed at time t.   
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at t=0 (m=0 is the default value when no prior information is available). For the case of available prior information, m0 can be defined as a pXr matrix, where p is the number of columns in the covariates object and r is the cluster size
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at t=0 (Cova=100 is the default value when no prior information is available). For the case of available prior information, Cova0 can be defined as a pXp matrix, where p is the number of columns in the covariates object
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model
#' @param S0 prior covariance structure among voxels within every cluster at t=0, S0=1 is the default value when no prior information is available and defines an rXr identity matrix. For the case of available prior information, S0 can be defined as an rXr matrix, where r is the common number of voxels in every cluster
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting.N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Nsimu1  is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos1 a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @param perVol a positive numeric variable
#' @param Ncores a postive integer indicating the number of threads or cores to be used in the computation of the activation maps.
#' @return  It returns a list of the form [[k]][p,x,y,z], where k defines the type of test (k = 1 for "Marginal", k = 2 for "JointTest", and k = 3 for "LTT"), p represents the column position in the covariates matrix and x,y,z represent the voxel position in the brain image.
#' @examples 
#' ffdEvidenceFFBS(ffdc = fMRI.data, covariates = covariables, 
#' m0 = 0, Cova = 100, delta = 0.95, S0 = 1,n0 = 1, Nsimu1 = 100, Cutpos1 = 30,
#' r1 = 1, Ncores = 15)
#' @export
ffdEvidenceFSTS = function(ffdc, covariates, m0=0, Cova=100,
                           delta=0.95, S0=1, n0=1, N1=FALSE, 
                           Nsimu1 = 100, Cutpos1=30, r1 = 1, 
                           perVol = 0.10, Ncores = NULL, seed=NULL){
  
  # check ncores
  if(!is.null(Ncores)) {
    
    if(is.na(Ncores)) {
      Ncores  <-  1
    }
    
    if(Ncores < 1) {
      stop("The number of cores must be greater than zero")
    }
  }

  # check number of simulations
  if(Nsimu1 > 2) {
    stop("The simulations must be grater than 2")
  }

  # TODO in the docmendation says thtat this variable un an integer, but by default is boolean
  # check number of images
  #if(N1 > 2) {
  #  stop("The number of images must be grater than 2")
  #}


  
  # Check ffdc
  if(!is.array(ffdc)) {
    stop("ffde must be an array.")
  }
  
  
  
  if(length(dim(ffdc)) != 4 ) {
    
    stop("ffdc must be a 4D array.")
    
  }
  
  # check covariates
  if(!is.data.frame(covariates)) {
    stop("covariates must be a dataframe.")
  }
  
  # check
  if(length(dim(covariates)) != 2) {
    
    stop("covariates must be a 2D array.")
  }

  
  if(N1==FALSE){N1=dim(ffdc)[4]}
  
  #TAKING THE POSITIONS FROM THE 4D IMAGE WITH NON-NULL VALUES 
  posiffd1 <- which(ffdc[,,,1] != 0, arr.ind = TRUE)
  
  
  
  #COMPUTING THE EVIDENCE FOR BRAIN ACTIVATION: VOXEL-WISE ANALYSIS
  set.seed(seed)
  ffd.out = pbapply::pbapply(posiffd1, 1, ffdSingleVoxelFSTS, covariates, ffdc, m0, Cova,
                  delta, S0, n0, N1, Nsimu1, Cutpos1, Min.vol = perVol*max(ffdc), r1, cl = Ncores)
  #number of tests from the output of ffdsingleVoxelFFBS  (Joint, marginal and LTT)
  Ntest <- 3
  vol.evidence <- list()
  
  
  for(k in 1:(Ntest)){
    vol.evidence[[k]] <- array(0, c(dim(covariates)[2], dim(ffdc)[1:3]))
  }
  
  
  for(ii in 1:dim(posiffd1)[1]){
    vol.evidence[[1]][ ,posiffd1[ii,1], posiffd1[ii,2], posiffd1[ii,3]] <- ffd.out[[ii]]$EvidenceJoint
    vol.evidence[[2]][ ,posiffd1[ii,1], posiffd1[ii,2], posiffd1[ii,3]] <- ffd.out[[ii]]$EvidenceMargin
    vol.evidence[[3]][ ,posiffd1[ii,1], posiffd1[ii,2], posiffd1[ii,3]] <- ffd.out[[ii]]$EvidenLTT
  }
  
  return(vol.evidence)
  
  
  
}



