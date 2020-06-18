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

#' Scale of GMRF precision matrix
#'
#' This function scales the precision matrix of a GMRF such that the geometric
#' mean of the marginal variance is one.
#'
#' @param Q Precision matrix for a GMRF.
#' @param A Linear constraint for Q.
#' @param eps Value of the small constant added to the diagonal of Q for
#'   invertibility.
#' 
#' @details
#'
#' This implements the same thing as [`INLA::inla.scale.model`]. The marginal
#' variance of each connected component is one.
#'
#' @export
scale_gmrf_precision <- function(Q,
                                 A = matrix(1, ncol = ncol(Q)),
                                 eps = sqrt(.Machine$double.eps)) {

  nb <- spdep::mat2listw(abs(Q))$neighbours
  comp <- spdep::n.comp.nb(nb)

  for (k in seq_len(comp$nc)) {
    idx <- which(comp$comp.id == k)
    Qc <- Q[idx, idx, drop = FALSE]

    if (length(idx) == 1) {
      ## set marginal variance for islands = 1
      Qc[1, 1] <- 1
    } else {
      Ac <- A[ , idx, drop = FALSE]
      Qc_eps <- Qc + Matrix::Diagonal(ncol(Qc)) * max(Matrix::diag(Qc)) * eps
      Qc_inv <- qinv(Qc_eps, A = Ac)
      scaling_factor <- exp(mean(log(Matrix::diag(Qc_inv))))
      Qc <- scaling_factor * Qc
    }

    Q[idx, idx] <- Qc
  }

  Q
}


qinv <- function(Q, A = NULL) {
  Sigma <- Matrix::solve(Q)
  if (is.null(A))
    return(Sigma)
  else {
    A <- matrix(1,1, nrow(Sigma))
    W <- Sigma %*% t(A)
    Sigma_const <- Sigma - W %*% Matrix::solve(A %*% W) %*% Matrix::t(W)
    return(Sigma_const)
  }
}

