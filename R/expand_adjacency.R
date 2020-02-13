expand_adjacency <- function(M, max.dist, specific_districts) {
  
tmp.adj.m <- M

## Zero is unconnected
tmp.adj.m[tmp.adj.m == 0] <- 0
## Two is adjacent
tmp.adj.m[tmp.adj.m == 1] <- 2
## One is "self"
diag(tmp.adj.m) <- 1

## degree.m will hold the degrees
degree.m <- c()
for (i in 1:nrow(M)) {
  # print(i)
  ## First-degree neighbors for region i
  col.v <- tmp.adj.m[, i]
  ## Storage for collection of pre-identified regions
  matched <- which(col.v > 0)
  ## Rolling no. of neighbors
  n.matched <- length(matched)
  ## Expanding scope to neighbors of neighbors
  ## (3 is N-of-Ns, 4 is N-of-N-of-Ns and so on)
  sep.degree <- 3
  ## Initialize stopping condition
  none_added <- F
  
  ## Go until there are no more neighbors to take
  ## up to maximum degree
  while(any(col.v == 0) & !none_added & n.matched > 1 & sep.degree <= max.dist) {
    ## Find neighbors of current matches
    adj.set <- which(rowSums(tmp.adj.m[,matched]) > 0)
    ## Exclude already-matched regions
    col.v[adj.set[!(adj.set %in% matched)]] <- sep.degree
    ## Increment degree of adjacency
    sep.degree <- sep.degree + 1
    ## Add new regions to list of matched regions
    matched <- sort(unique(c(matched, which(rowSums(tmp.adj.m[,matched]) > 0))))
    ## Check that no new regions have been added
    none_added <- length(sort(unique(c(matched, which(rowSums(tmp.adj.m[,matched]) > 0))))) == n.matched
    ## Update number of matches
    n.matched <- length(matched)
  }
  degree.m <- cbind(degree.m, col.v)
}
degree.m[degree.m == 0] <- Inf
degree.m[degree.m > max.dist] <- Inf

degree.m <- 1*is.finite(degree.m)
diag(degree.m) <- 0

if(!is.na(specific_districts)) {
  
  M[specific_districts,] <- degree.m[specific_districts,]
  M[, specific_districts] <- degree.m[, specific_districts]
  
} else {
  
  M <- degree.m
  
}

M

}
