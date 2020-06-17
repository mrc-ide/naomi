#' Create adjacency matrix from spatial polygons
#'
#' @param sh a spatial polygons object
#'
#' @return an adjacency matrix with 1s and 0s
#'
#' @details
#' Input `sh` can be either sf class or SpatialPolygons from `sp`. Uses `spdep`
#' package to convert shapefile to neighbor list to adjacency matrix.
#' 
#' @export
create_adj_matrix <- function(sh){

  if(!is(sh, "sp"))
    sh <- as(sh, "Spatial")

  nb <- spdep::poly2nb(sh)
  adj <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
  colnames(adj) <- rownames(adj)

  adj
}
    
    
#' Create edgelist from adjacency matrix
#'
#' @export
create_edge_list <- function(adj_matrix) {

  w <- adj_matrix
  
  ## convert W to a sparse matrix if not already sparse.
  if(!is(w, "sparseMatrix"))
    w <- Matrix(w, sparse = TRUE)
  
  w[upper.tri(w)] <- 0
  
  ## pull out adjacency pairs from W
  edges <- summary(w)  # analagous to `which(w == 1, arr.ind = TRUE)` on dense matrix
  edges <- edges[,grep("^i$|^j$", colnames(edges))]
  
  edges
}

calculate_scaling_factor <- function(n, edges) {

  ## Build the adjacency matrix

  adj.matrix <- Matrix::sparseMatrix(i=edges[,1],j=edges[,2],x=1,symmetric=TRUE)

  ## The ICAR precision matrix (note! This is singular)
  Q <-  Matrix::Diagonal(n, Matrix::rowSums(adj.matrix)) - adj.matrix

  ## Add a small jitter to the diagonal for numerical stability (optional but recommended)
  Q_pert <- Q + Matrix::Diagonal(n) * max(Matrix::diag(Q)) * sqrt(.Machine$double.eps)

  ## Compute the diagonal elements of the covariance matrix subject to the 
  ## constraint that the entries of the ICAR sum to zero.
  ## See the function help for further details.
  ## Q_inv <- INLA::inla.qinv(Q_pert, constr=list(A = matrix(1, 1, n),e=0))
  Q_inv <- qinv(Q_pert, A = matrix(1, 1, n))
  
  # Compute the geometric mean of the variances, which are on the diagonal of Q.inv
  scaling_factor <- exp(mean(log(Matrix::diag(Q_inv))))
  return(scaling_factor)
}

qinv <- function(Q, A = NULL) {
  Sigma <- Matrix::solve(Q)
  if (is.null(A))
    return(Sigma)
  else {
    A <- matrix(1,1, nrow(Sigma))
    W <- Sigma %*% t(A)
    Sigma_const <- Sigma - W %*% solve(A %*% W) %*% t(W)
    return(Sigma_const)
  }
}

