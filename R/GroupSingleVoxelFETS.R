#' @name GroupSingleVoxelFETS
#' @title GroupSingleVoxelFETS
#' @description
#' \loadmathjax
#' This function is used to perform a group activation analysis for single voxels based on the FETS algorithm.
#' @references
#' \insertRef{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}
#' 
#' \insertRef{cardona2021bayesdlmfmri}{BayesDLMfMRI}
#' @details
#' This function allows the performance of a group activation analysis for single voxels. A multivariate dynamic linear model is fitted to a cluster of voxels, with its center at location (i,j,k), in the way it is presented in \insertCite{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}.
#' @param posi.ffd the position of the voxel in the brain image.
#' @param DatabaseGroup list of N elements, each being a 4D array (ffdc[i,j,k,t]) that contains the sequence of MRI images related to each of the N subjects in the sample.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup.
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at t=0 (m=0 is the default value when no prior information is available). For the case of available prior information, m0 can be defined as a pXr matrix, where p is the number of columns in the covariates object and r is the cluster size.
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at t=0 (Cova=100 is the default value when no prior information is available). For the case of available prior information, Cova0 can be defined as a pXp matrix, where p is the number of columns in the covariates object.
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model.
#' @param S0 prior covariance structure between pair of voxels within every cluster at t=0. S0=1 is the default value when no prior information is available and defines an rXr identity matrix. For the case of available prior information, S0 can be defined as an rXr matrix, where r is the common number of voxels in every cluster.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting.N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @param Test test type either "LTT" (Average cluster effect) or "JointTest" (Joint effect).
#' @return a list containing a vector (Evidence) with the evidence measure of 
#' activation for each of the p covariates considered in the model, the simulated 
#' online trajectories related to the state parameter, the simulated BOLD responses, \mjseqn{\hat{Y}}{ascii}.
#'  and a measure to examine the goodness of fit of the model \mjseqn{(100 \ast |Y[i,j,k]_t - \hat{Y}[i,j,k]_t | \hat{Y}[i,j,k]_t )} for that particular voxel (FitnessV).
#' @examples
#' "See the Vignettes for examples."
#' @export
GroupSingleVoxelFETS <- function(posi.ffd, DatabaseGroup, 
                                 covariates, m0, Cova, delta, S0, n0, N1, Nsimu1, 
                                 r1, Test, Cutpos){
  
  if(is.logical(N1)) {
    if(N1==FALSE){N1 = dim(covariates)[1]}
  }

  validate_input(
    covariates=covariates,
    delta=delta,
    n0=n0,
    N1=N1,
    Nsimu1=Nsimu1,
    r1=r1,
    Cutpos1=Cutpos,
    Test=Test
  )
  
  
  if(r1 == 0){
    
    posi <- distanceNeighbors(posi.refer = posi.ffd, r1)
    Ngroup <- length(DatabaseGroup)
    #BOLD RESPONSE SERIES FOR A SPECIFIC CLUSTER
    series.group = NULL 
    #system.time(
    for(i in 1:Ngroup){
      ffd.c <- DatabaseGroup[[i]]
      series <- sapply(1:dim(posi)[1], function(ii){ffd.c[posi[ii,1], posi[ii,2], posi[ii,3], ]})
      series.group <- rbind(series.group, series)
    }
    
    #case for single voxels
    if(any(series.group==0)){ 
      if(Test=="LTT"){return(NA)}
      if(Test=="Joint"){return( NA )}}else{
                                       
                                       if(Test=="Joint"){
                                         Cova1 <- diag(rep(Cova, dim(covariates)[2]))
                                         delta1<- sqrt(delta)
                                         Beta1 <- diag(1/c(rep(delta1, dim(covariates)[2])))
                                         res   <- Group_FunctionalMultiTest(ffd1 = series.group, Cova = covariates, m0In = m0, c0In = Cova1, S0In = S0, 
                                                                             beta0In = Beta1, nt0In = n0, flag1 = 0, NIn = N1, NS = Ngroup, Nsimu = Nsimu1, CUTpos = Cutpos)
                                         
                                         return(res)
                                         
                                       }
                                       
                                       if(Test=="LTT"){return(NA)}}}else{
                                         
                                         
                                         
                                         Ngroup <- length(DatabaseGroup)
                                         #THIS LINE RETURN THE POSITIONS OF EACH VOXEL INSIDE THE CLUSTER GIVEN THE DISTANCE r1
                                         posi1 <- distanceNeighbors(posi.refer = posi.ffd, r1)
                                         aux.pos <- dim(DatabaseGroup[[1]])[1:3]
                                         row_sub1 <- apply(posi1, 1, function(row, x1){0 < row & row<=x1}, x1=aux.pos)
                                         
                                         posi <- posi1[apply(t(row_sub1), 1, sum)==3, ]
                                         
                                         
                                         
                                         #BOLD RESPONSE SERIES FOR A SPECIFIC CLUSTER
                                         series.group = NULL 
                                         for(i in 1:Ngroup){
                                           ffd.c <- DatabaseGroup[[i]]
                                           series <- sapply(1:dim(posi)[1], function(ii){ffd.c[posi[ii,1], posi[ii,2], posi[ii,3], ]})
                                           series.group <- rbind(series.group, series)
                                         }
                                         
                                         
                                         if(any(series.group[,1]==0)){if(Test=="LTT"){return(NA)}
                                           if(Test=="Joint"){return(NA)}}else{
                                                                            flag <- any(series.group==0)
                                                                            
                                                                            Cova1 <- diag(rep(Cova, dim(covariates)[2]))
                                                                            delta1 <- sqrt(delta)
                                                                            Beta1 <- diag(1/c(rep(delta1, dim(covariates)[2])))
                                                                            
                                                                            if(Test=="LTT"){
                                                                              
                                                                              res <- .Gruop_FunctionalTestLT(series.group, covariates, m0, Cova1, S0, Beta1, n0, sum(flag), N1, Ngroup, Nsimu1, Cutpos)  
                                                                              return(res)
                                                                            }
                                                                            
                                                                            if(Test=="Joint"){
                                                                              
                                                                              res   <- Group_FunctionalMultiTest(ffd1 = series.group, Cova = covariates, m0In = m0, c0In = Cova1, S0In = S0, 
                                                                                                              beta0In = Beta1, nt0In = n0, flag1 = sum(flag), NIn = N1, NS = Ngroup, Nsimu = Nsimu1, CUTpos = Cutpos)
                                                                              
                                                                              return(res)
                                                                              
                                                                            }
                                                                            
                                                                            
                                                                          }  
                                         
                                       }
  
  
  
  
}
