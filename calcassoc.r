# calculat hwis

# test
# source("simsightings.r")
# set a seed
# orcid <- 0000000311828578 # my orcid
# set.seed(orcid)

# sightdat <- simsightings(
  # n_ids = 50,
  # n_samp_periods = 100, 
  # lambda_n_grps = 2, 
  # lambda_grp_size = 3
# )

calcassoc <- function(sightdat, associndex = 'hwi', assocfun = 'grpvar') { 

# constants
ASSOCINDEX_HWI <- 'hwi'
ASSOCINDEX_SR  <- 'sr'
ASSOCFUN_GRPVAR <- 'grpvar'

# association functions (for outer)
grpvar <- function(a, b) {
  sightdat$grp[a] == sightdat$grp[b]
}

# assign association function
if(assocfun == ASSOCFUN_GRPVAR) ASSFUN <- grpvar

# grab some handles
usamps <- sort(unique(sightdat$sampid))
nsamps <- length(usamps)

uids <- sort(unique(sightdat$id))
nids <- length(uids)

# calculate assoc and nax
assoc   <- matrix(0, nids, nids, dimnames = list(uids, uids))
nax     <- matrix(0, nids, nsamps, dimnames = list(uids, usamps))

for(i in 1:nsamps) {
  cursamp <- sightdat$sampid == usamps[i]
  curids <- sightdat$id[cursamp]

  # increment nax
  nax[match(curids, uids), i] <- 1
  
  # increment assoc
  assocmatch <- outer(which(cursamp), which(cursamp), ASSFUN)
  diag(assocmatch) <- NA
  
  assocmatch_ind <- which(assocmatch, arr.ind = TRUE)
  
  if(nrow(assocmatch_ind) > 0) {
    a <- match(curids[assocmatch_ind[, 1]], uids)
    b <- match(curids[assocmatch_ind[, 2]], uids)
    
    toinc <- unique((b-1)*nids + a) # does this need to be unique?
    assoc[toinc] <- assoc[toinc] + 1
  }
}

# calculatye nmatrix and yab_prime
nmatrix 	  <- matrix(0, nids, nids, dimnames = list(uids, uids))
yab_prime 	<- matrix(0, nids, nids, dimnames = list(uids, uids))

nsight <- rowSums(nax)
nmatrix <- outer(nsight, nsight, '+')
yab_prime <- tcrossprod(nax)


# calculate  simple ratio denominator
if(associndex == ASSOCINDEX_SR) assdenom <- nmatrix - yab_prime
if(associndex == ASSOCINDEX_HWI) assdenom <- nmatrix/2

assrate <- assoc / assdenom
diag(assrate) <- NA

assrate
}
