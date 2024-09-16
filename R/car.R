#' Create adjacency matrix from spatial polygons
#'
#' @param sh a spatial polygons object
#'
#' @return an adjacency matrix with 1s and 0s
#'
#' @details
#' Input `sh` can be either `sf` class or SpatialPolygons from `sp`. Uses `spdep`
#' package to convert shapefile to neighbor list to adjacency matrix.
#'
#' @export
create_adj_matrix <- function(sh) {

  s2_current <- sf::sf_use_s2()
  on.exit(invisible(
    suppress_conditions(utils::capture.output(sf::sf_use_s2(s2_current)),
                        "Spherical geometry \\(s2\\) switched"))
  )
  invisible(suppress_conditions(utils::capture.output(sf::sf_use_s2(FALSE)),
                                "Spherical geometry \\(s2\\) switched"))

  if (nrow(sh) == 1) {
    adj <- matrix(0, dimnames = list(sh$area_id, sh$area_id))
  } else {
    nb <- suppress_conditions(
      spdep::poly2nb(sh),
      message_regexp = "although coordinates are longitude/latitude, st_intersects assumes that they are planar",
      warning_regexp = c(
        "some observations have no neighbours",
        "neighbour object has 2 sub-graphs")
      )
    adj <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
    colnames(adj) <- rownames(adj)
  }

  adj
}


#' Create edgelist from adjacency matrix
#'
#' @param adj_matrix Adjacency matrix
#'
#' @export
create_edge_list <- function(adj_matrix) {

  w <- adj_matrix

  ## convert W to a sparse matrix if not already sparse.
  if(!methods::is(w, "sparseMatrix"))
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
#' This implements the same thing as INLA::inla.scale.model. The marginal
#' variance of each connected component is one.
#'
#' @export
scale_gmrf_precision <- function(Q,
                                 A = matrix(1, ncol = ncol(Q)),
                                 eps = sqrt(.Machine$double.eps)) {

  ## `style = ` argument is arbitrary; it will throw a warning if NULL (default),
  ## but the neighbours list does not depend on it.
  nb <- suppress_conditions(
    spdep::mat2listw(abs(Q), style = "B", zero.policy = TRUE)$neighbours,
    warning_regexp = "neighbour object has 2 sub-graphs"
  )
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

