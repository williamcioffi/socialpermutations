# mat and df helper translators
# functions for converting symmetrical matrices to data frames and back

mat2df <- function(m, datlabel = "value", symmetrical = TRUE, diag = FALSE, stringsAsFactors = FALSE) {
  if(!symmetrical) stop("non symmetric matrices not implemented yet...")
  if(diag) stop("diagonal not implemented yet")
  if(stringsAsFactors) warning("stringsAsFactors set to TRUE... are you sure you want that?")
  
  if(is.null(rownames(m))) rownames(m) <- 1:nrow(m)
  if(is.null(colnames(m))) colnames(m) <- 1:nrow(m)
  
  i1 <- matrix(rep(rownames(m), ncol(m)), nrow(m), ncol(m))
  i2 <- t(i1)
  
  ut <- upper.tri(m)
  
  out <- data.frame(id1 = i1[ut], id2 = i2[ut], dat = m[ut], stringsAsFactors = stringsAsFactors)
  names(out)[3] <- datlabel
  
  out
}

df2mat <- function(d, symmetrical = TRUE) {
  if(!symmetrical) stop ("non symmetric matrices not implemented yet...")
  
  uids <- sort(unique(c(d$id1, d$id2)))
  nids <- length(uids)
  
  m <- matrix(NA, nids, nids)
  dimnames(m) <- list(uids, uids)
  
  for(i in 1:nrow(d)) {
    m[d$id1[i], d$id2[i]] <- d[i, 3]
    m[d$id2[i], d$id1[i]] <- d[i, 3]
  }
  
  m
}
