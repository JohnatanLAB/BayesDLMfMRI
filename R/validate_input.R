# this function is used to read the number of cores
get_n_cores  <- function(Ncores) {

    # check ncores
    if(!is.null(Ncores)) {
        
        if(is.na(Ncores)) {
            Ncores  <-  1
        }

        if(!is.numeric(Ncores)) {
            stop("The number of cores must be numeric")
        }

        Ncores  <- as.integer(Ncores)
        
        if(Ncores < 1) {
            stop("The number of cores must be greater than zero")
        }
    }

    return(Ncores)

}

check_ffdgroup  <- function(ffdGroup) {

    if(!is.list(ffdGroup)) {
        stop("ffdGroup must be a list.")
    }

    for(i in 1:length(ffdGroup)) {

        ffdc  <- ffdGroup[[i]]

        # Check ffdc
        if(!is.null(ffdc)) {

            if(!is.array(ffdc)) {
                stop("all elements of ffdGroup must be an array.")
            }

            if(length(dim(ffdc)) != 4 ) {
            
                stop("all elements of ffdGroup must be an 4D array.")
            }

        }



    }

}

# this function is used to validate input
validate_input  <- function(N1=NULL, Test=NULL, Nsimu1=NULL,
                            ffdc=NULL, covariates=NULL,
                            r1 = NULL, delta=NULL, perVol=NULL,Min.vol=NULL,
                            n0 = NULL, Cutpos1=NULL, ffdGroup=NULL) {


  # check_ffdgroup
  if(!is.null(ffdGroup)) {
    check_ffdgroup(ffdGroup)
  }

  # check perVol
  if(!is.null(n0)) {

    if(n0 < 0 ) {
        stop("n0 must be non-negative" )
    }

  }
  
  # check delta
  if(!is.null(delta)) {

    if( (delta >= 1)  | (delta <= 0) ) {
        stop("The discount factor, delta must between 0 and 1" )
    }

  }

  # check perVol
  if(!is.null(perVol)) {

    if(perVol < 0 ) {
        stop("The threshold for the voxels, perVol must be non-negative" )
    }

  }

  # check Min.vol
  if(!is.null(Min.vol)) {

    if(Min.vol < 0 ) {
        stop("The threshold for the voxels, Min.vol must be non-negative" )
    }

  }

  # check r1
  if(!is.null(r1)) {

    if(r1 < 0 ) {
        stop("r1 must be non-negative" )
    }

  }

  

  # check Cutpos1
  if(!is.null(Cutpos1)) {

    if(Cutpos1 < 0 ) {
        stop("the cutpoint, Cutpos1 must be non-negative" )
    }

  }

  # check N1
  if(!is.null(N1)) {

    
    if( !(N1 > 2) ) {
        stop("The number of images must be grater than 2")
    }

  }

   # check Nsimu1
  if(!is.null(Nsimu1)) {

    
    if( !(Nsimu1 > 2) ) {
        stop("The  numbersimulations must be grater than 2")
    }

  }

  # check test type
  if(!is.null(Test)) {

    
    if(!(Test %in% c("LTT", "JointTest"))) {
        stop("Test must be LTT or JointTest")
    }

  }

  
  
  # Check ffdc
  if(!is.null(ffdc)) {

    if(!is.array(ffdc)) {
        stop("ffdc must be an array.")
    }

    if(length(dim(ffdc)) != 4 ) {
    
        stop("ffdc must be a 4D array.")
    }

  }

  # check covariates
  if(!is.null(covariates)) {

    if(!is.data.frame(covariates)) {
        stop("covariates must be a dataframe.")
    }

    if(length(dim(covariates)) != 2) {
        stop("covariates must be a 2D array.")
    }

  }


  

}