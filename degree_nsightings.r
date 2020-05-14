# get some functions
source('calcassoc.r')
source('simsighting.r')

library(igraph)

# sim some sightings

# set a seed
orcid <- 0000000311828578 # my orcid
set.seed(orcid)

sightdat <- simsightings(
  n_ids = 100,
  n_samp_periods = 1000, 
  lambda_n_grps = 2, 
  lambda_grp_size = 3
)

hwis <- calcassoc(sightdat)
g1 <- igraph::graph_from_adjacency_matrix(hwis, mode = "undirect", weighted = TRUE, diag = FALSE)

# make those weights bigger
E(g1)$width <- E(g1)$weight*10

# compare sightingcount to degree
sightingcount <- as.data.frame(table(sightdat$id))
colnames(sightingcount) <- c('id', 'n')
sightingcount[, 'deg'] <- NA

# merge in degree
deg <- degree(g1)
sightingcount$deg[match(names(deg), sightingcount$id)] <- deg

# mod
mod <- lm(deg ~ n, data = sightingcount)
summary(mod)
int <- coef(summary(mod))[1, 1]
bet <- coef(summary(mod))[2, 1]

# plot
plot(sightingcount$n, sightingcount$deg,
  type = 'n',
  las = 1,
  xlab = 'number of sightings',
  ylab = 'degree',
  axes = FALSE
)

axis(1, tcl = -0.3)
axis(2, tcl = -0.3)
axis(1, lab = NA, tcl = 0.3)
axis(2, lab = NA, tcl = 0.3)

points(jitter(sightingcount$n), jitter(sightingcount$deg),
  pch = 16
)

curve(x*bet + int,
  from = min(sightingcount$n),
  to = max(sightingcount$n),
  add = TRUE,
  lwd = 3, lty = 2
)

# # take a peak
# lay <- igraph::layout_with_fr(g1)
# plot(g1,
  # edge.curved = TRUE,
  # layout = lay
# )
