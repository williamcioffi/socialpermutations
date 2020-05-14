# simsightings

# # test
# # set a seed
# orcid <- 0000000311828578 # my orcid
# set.seed(orcid)

# dat <- simsightings(
  # n_ids = 50,
  # n_samp_periods = 100, 
  # lambda_n_grps = 2, 
  # lambda_grp_size = 3
# )

# # set parameters for debugging (is there a better way to do this?)
# # i mean just automatically run a list of parameters ignoring the commas or
# # turning them into newlines or something?
# n_ids = 50
# n_samp_periods = 100 
# lambda_n_grps = 2
# lambda_grp_size = 3


# simulate some sighting data function
simsightings <- function(n_ids, n_samp_periods, lambda_n_grps, lambda_grp_size) {
  NIDS <- n_ids
  
  IDS <- 1:NIDS
  NSAMPPERIODS <- n_samp_periods
  SAMPPERIODS <- 1:NSAMPPERIODS

  NGRPS_LAMBDA <- lambda_n_grps
  GRP_SIZE_LAMBDA <- lambda_grp_size
  
  # make up the sightings group stats
  ngrp_samp <- rpois(SAMPPERIODS, NGRPS_LAMBDA)
  grpsize_samp <- lapply(ngrp_samp, function(x) rpois(x, GRP_SIZE_LAMBDA))
  nids_samp <- sapply(grpsize_samp, sum)

  # make a list
  samps <- lapply(1:NSAMPPERIODS, function(x) {
    list(
      ngrp    = ngrp_samp[x],
      grpsize = grpsize_samp[[x]],
      nids    = nids_samp[x]
    )
  })

  # make sure every sampling period has >= max NIDS
  samps <- samps[sapply(samps, '[[', 'nids') <= NIDS]

  # filter out groups of size 0
  for(i in 1:length(samps)) {
    keep <- samps[[i]]$grpsize > 0
    if(any(!keep)) {
      samps[[i]]$grpsize <- samps[[i]]$grpsize[keep]
      samps[[i]]$ngrp <- length(which(keep))
    }
  }
  
  # filter out sampling periods that has 0 groups
  samps <- samps[sapply(samps, '[[', 'ngrp') > 0]
  


  # build up a sighting table 
  id <- vector()
  grp <- vector()
  sampid <- vector()

  for(i in 1:length(samps)) {
    curids <- sample(IDS, samps[[i]]$nids)
    grpids <- 1:samps[[i]]$ngrp
    
    curgrps <- rep(grpids, times = samps[[i]]$grpsize)
    
    # make the vectors
    id <- c(id, curids)
    grp <- c(grp, curgrps)
    sampid <- c(sampid, rep(i, length(curids)))
  }

  data.frame(id, grp, sampid)
}