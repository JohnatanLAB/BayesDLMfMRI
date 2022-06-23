#' @import pbapply


#' @name ffdAverageGroupEvidenceFEST
#' @title ffdAverageGroupEvidenceFEST 
#' @description 
#' descripcion de la funcion ffdAverageGroupEvidenceFEST TODO
#' @details
#' detalles de uso de la funcion' 
#' @param ffdGroup A 5D array TODO
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at t=0 (m=0 is the default value when no prior information is available). For the case of available prior information, m0 can be defined as a pXr matrix, where p is the number of columns in the covariates object and r is the cluster size
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at t=0 (Cova=100 is the default value when no prior information is available). For the case of available prior information, Cova0 can be defined as a pXp matrix, where p is the number of columns in the covariates object
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model
#' @param S0 prior covariance structure between pair of voxels within every cluster at t=0, S0=1 is the default value when no prior information is available and defines an rXr identity matrix. For the case of available prior information, S0 can be defined as an rXr matrix, where r is the common number of voxels in every cluster
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting.N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param r1  positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @param mask define this TODO
#' @param Ncores a postive integer indicating the number of threads to be use
#' @export
ffdAverageGroupEvidenceFEST <- function(ffdGroup, covariates, m0=0, Cova=100,
                                 delta = 0.95, S0 = 1, n0 = 1, N1 = FALSE, Nsimu1=100, Cutpos=30, r1, mask, Ncores = NULL){
  
  if(N1==FALSE){N1 = dim(covariates)[1]}
  
  #TAKING THE POSITIONS FROM THE 4D IMAGE WITH NON-NULL VALUES 
  posiffd <- which(mask[,,] != 0, arr.ind = TRUE)
  
  
  
  
  ffd.out <- pbapply::pbapply(posiffd, 1, ffdAverageGroupVoxelFEST, ffdGroup, covariates, m0, Cova,
                              delta, S0, n0, N1, Nsimu1, r1, Cutpos, cl = Ncores)
  
  Ntest <- 2
  pAux <- dim(covariates)[2]
  vol.evidence <- list()
  
  
  for(k in 1:(dim(covariates)[2]*Ntest)){
    vol.evidence[[k]] <- array(0, dim(mask)[1:3])
  }
  
  
  for(j in 1:dim(covariates)[2]){
    for(ii in 1:dim(posiffd)[1]){
      vol.evidence[[j]][posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenceMargin[j]
      vol.evidence[[Ntest+j]][posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenceLTT[j]
    }
  }
  
  
  return(vol.evidence)

  
  
  
}











