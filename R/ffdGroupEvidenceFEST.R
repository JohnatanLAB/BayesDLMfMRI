#' @import pbapply

#' @name ffdGroupEvidenceFEST
#' @title ffdGroupEvidenceFEST
#' @description
#' This function can be used to build activation maps for task-based fMRI group data
#' @references
#' \insertRef{jimenez_bayesdlmfmri_2021}{BayesDLMfMRI}
#' @details
#' A multivariate dynamic linear model is fitted in the same fashion as at the individual level for every subject in the sample. 
#' However, at this stage, the posterior distributions from every subject are combined to build a single one, 
#' which is then employed to compute the activation evidence maps for the group of subjects by using the Forward estimated trajectories sampler (FETS) algorithm \insertCite{jimenez_bayesdlmfmri_2021}{BayesDLMfMRI}.
#' @param ffdGroup List of N elements, being each of them a 4D array (ffdc[i,j,k,t]) that contains the sequence of MRI images related to each of the N subjects in the sample.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at t=0 (m=0 is the default value when no prior information is available). For the case of available prior information, m0 can be defined as a pXr matrix, where p is the number of columns in the covariates object and r is the cluster size
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at t=0 (Cova=100 is the default value when no prior information is available). For the case of available prior information, Cova0 can be defined as a pXp matrix, where p is the number of columns in the covariates object
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model
#' @param S0 prior covariance structure between pair of voxels within every cluster at t=0, S0=1 is the default value when no prior information is available and defines an rXr identity matrix. For the case of available prior information, S0 can be defined as an rXr matrix, where r is the common number of voxels in every cluster
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting. N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param r1 positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @param Test test type either "LTT"or "JointTest"
#' @param mask 3D array that works as a brain of reference (MNI atlas) for the group analysis.
#' @param Ncores a postive integer indicating the number of threads or cores to be used in the computation of the activation maps.
#' @return It returns a list of 2 × p elements, where p is the number of covariates, and 2 is the number 
#' of options evaluated as sampler distributions: "Joint" and "Marginal." The first p elements from the list are 
#' the activation maps related to each column of the covariates matrix respectively when computing the activation evidence using the 
#' "Join" distribution. The remaining activation maps are those associated with the marginal distribution.
#' @examples TODO
#' @export
ffdGroupEvidenceFEST <- function(ffdGroup, covariates, m0=0, Cova=100,
                                delta = 0.95, S0 = 1, n0 = 1, N1 = FALSE, Nsimu1=100, Cutpos=30, r1, Test, mask, Ncores = NULL){
  
  if(N1==FALSE){N1 = dim(covariates)[1]}
  
  #TAKING THE POSITIONS FROM THE 4D IMAGE WITH NON-NULL VALUES 
  posiffd <- which(mask[,,] != 0, arr.ind = TRUE)
  
  
  
  if(Test == "LTT"){
    ffd.out <- pbapply::pbapply(posiffd, 1, ffdGroupVoxelFEST, ffdGroup, covariates, m0, Cova,
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
    
    return(vol.evidence)
    
  }
  if(Test == "Joint"){
    
    ffd.out = pbapply::pbapply(posiffd, 1, ffdGroupVoxelFEST, ffdGroup, covariates, m0, Cova,
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
    
    
    return(vol.evidence)
    
  }
  
  
}
    
  









