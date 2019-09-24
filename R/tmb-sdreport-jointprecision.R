## Non Exported Functions from TMB
updateCholesky <- function (L, H, t = 0)
{
  .Call("tmb_destructive_CHM_update", L, H, t, PACKAGE = "TMB")
}

## Non exported function from TMB
solveSubset <- function(Q, L = Matrix::Cholesky(Q, super = TRUE, perm = TRUE), diag = FALSE)
{
  stopifnot(methods::is(L, "dCHMsuper"))
  invQ <- .Call("tmb_invQ", L, PACKAGE = "TMB")
  iperm <- Matrix::invPerm(L@perm + 1L)
  if (diag) {
    invQ <- Matrix::diag(invQ)[iperm]
  }
  else {
    invQ <- invQ[iperm, iperm, drop = FALSE]
  }
  invQ
}

#' Get Joint Precision of TMB Fixed and Random
#'
#' This is a copy of `[TMB::sdreport]` that removes all computations of `ADREPORT()`ed variables to only return the joint precision.
#'
#' @inheritParams TMB::sdreport
#'
#' @keywords internal
sdreport_joint_precision <- function (obj, par.fixed = NULL, hessian.fixed = NULL,
                                      bias.correct = FALSE, bias.correct.control = list(sd = FALSE,
                                                                                        split = NULL, nsplit = NULL), ignore.parm.uncertainty = FALSE,
                                      skip.delta.method = FALSE)
{
  if (is.null(obj$env$ADGrad) & (!is.null(obj$env$random)))
    stop("Cannot calculate sd's without type ADGrad available in object for random effect models.")
  obj2 <- TMB::MakeADFun(obj$env$data, obj$env$parameters, type = "ADFun",
                         ADreport = TRUE, DLL = obj$env$DLL, silent = obj$env$silent)
  r <- obj$env$random
  if (is.null(par.fixed)) {
    par <- obj$env$last.par.best
    if (!is.null(r))
      par.fixed <- par[-r]
    else par.fixed <- par
    gradient.fixed <- obj$gr(par.fixed)
  }
  else {
    gradient.fixed <- obj$gr(par.fixed)
    par <- obj$env$last.par
  }
  if (length(par.fixed) == 0)
    ignore.parm.uncertainty <- TRUE
  if (ignore.parm.uncertainty) {
    hessian.fixed <- NULL
    pdHess <- TRUE
    Vtheta <- matrix(0, length(par.fixed), length(par.fixed))
  }
  else {
    if (is.null(hessian.fixed)) {
      hessian.fixed <- stats::optimHess(par.fixed, obj$fn, obj$gr)
    }
    pdHess <- !is.character(try(chol(hessian.fixed), silent = TRUE))
    Vtheta <- try(solve(hessian.fixed), silent = TRUE)
    if (methods::is(Vtheta, "try-error"))
      Vtheta <- hessian.fixed * NaN
  }
  if (!is.null(r)) {
    hessian.random <- obj$env$spHess(par, random = TRUE)
    L <- obj$env$L.created.by.newton
    if (!is.null(L)) {
      updateCholesky(L, hessian.random)
      hessian.random@factors <- list(SPdCholesky = L)
    }
  }
  ADGradForward0Initialized <- FALSE
  ADGradForward0Initialize <- function() {
    obj$env$f(par, order = 0, type = "ADGrad")
    ADGradForward0Initialized <<- TRUE
  }
  if (!is.null(r)) {
    if (methods::is(L, "dCHMsuper")) {
      diag.term1 <- solveSubset(L = L, diag = TRUE)
      if (ignore.parm.uncertainty) {
        diag.term2 <- 0
      }
      else {
        f <- obj$env$f
        w <- rep(0, length(par))
        if (!ADGradForward0Initialized)
          ADGradForward0Initialize()
        reverse.sweep <- function(i) {
          w[i] <- 1
          f(par, order = 1, type = "ADGrad", rangeweight = w,
            doforward = 0)[r]
        }
        nonr <- setdiff(seq_along(par), r)
        tmp <- sapply(nonr, reverse.sweep)
        if (!is.matrix(tmp))
          tmp <- matrix(tmp, ncol = length(nonr))
        A <- solve(hessian.random, tmp)
        diag.term2 <- rowSums((A %*% Vtheta) * A)
      }
        if (length(par.fixed) == 0) {
          jointPrecision <- hessian.random
        }
        else if (!ignore.parm.uncertainty) {
          G <- hessian.random %*% A
          G <- as.matrix(G)
          M1 <- cbind2(hessian.random, G)
          M2 <- cbind2(t(G), as.matrix(t(A) %*% G) +
                             hessian.fixed)
          M <- rbind2(M1, M2)
          M <- Matrix::forceSymmetric(M, uplo = "L")
          dn <- c(names(par)[r], names(par[-r]))
          dimnames(M) <- list(dn, dn)
          p <- Matrix::invPerm(c(r, (1:length(par))[-r]))
          jointPrecision <- M[p, p]
        }
        else {
          warning("ignore.parm.uncertainty ==> No joint precision available")
        }
    }
    else {
      warning("Could not report sd's of full randomeffect vector.")
    }
  }

  jointPrecision
}
