fit_tmbstan <- function(tmb_input,
                        chains = 4,
                        iterations = 500,
                        refresh = 0,
                        rng_seed = NULL,
                        laplace = FALSE,
                        cores = parallel::detectCores()) {

  if(!requireNamespace("tmbstan", quietly = TRUE))
    stop("fit_tmbstan() requires 'tmbstan' to be installed.")
  
  ## Zero iteration fit to get TMB object
  fit0 <- withCallingHandlers(
    fit_tmb(tmb_input, outer_verbose = FALSE, max_iter = 0),
    warning = function(w) {
      if(grepl("convergence error: iteration limit reached without convergence \\(10\\)", w$message))
        invokeRestart("muffleWarning")
    }
  )

  if(is.null(rng_seed))
    rng_seed <- sample.int(.Machine$integer.max, 1)
  
  f <- tmbstan::tmbstan(fit0$obj, chains = chains, iter = iterations,
                        refresh = refresh, seed = rng_seed, cores = cores,
                        laplace = laplace)
  fit <- list(stanfit = f, tmbobj = fit0$obj)
  class(fit) <- "naomi_stanfit"

  fit
}

sample_tmbstan <- function(naomi_stanfit, verbose = FALSE) {

  stopifnot(methods::is(naomi_stanfit, "naomi_stanfit"))

  smp <- as.matrix(naomi_stanfit$stanfit, pars = "lp__", include = FALSE)
  obj <- naomi_stanfit$tmbobj


  if(ncol(smp) == length(obj$env$last.par) - length(obj$env$random)) {
    if(verbose) print("Simulating full parameters from Laplace fit")
    smp <- t(apply(smp, 1, sample_one_par_full, obj))
  }

  if(verbose) print("Simulating outputs")
  sim <- apply(smp, 1, obj$report)

  r <- obj$report()

  if(verbose) print("Returning sample")
  sample_out <- Map(vapply, list(sim), "[[", lapply(lengths(r), numeric), names(r))
  is_vector <- vapply(sample_out, class, character(1)) == "numeric"
  sample_out[is_vector] <- lapply(sample_out[is_vector], as.matrix, nrow = 1)
  names(sample_out) <- names(r)

  naomi_stanfit$sample <- sample_out
  
  naomi_stanfit
}


sample_one_par_full <- function(par_fixed, obj) {

  random_idx <- obj$env$random
  val <- obj$fn(par_fixed)
  par_full <- obj$env$last.par

  stopifnot(all.equal(par_fixed, par_full[-random_idx]))

  par_random <- par_full[random_idx]
  hess_random <- obj$env$spHess(par_full, random = TRUE)
  smp_random <- rmvnorm_sparseprec(1, par_random, hess_random)

  smp_full <- numeric(length(par_full))
  smp_full[random_idx] <- smp_random
  smp_full[-random_idx] <- par_fixed
  names(smp_full)[random_idx] <- colnames(smp_random)
  names(smp_full)[-random_idx] <- names(par_fixed)

  smp_full
}
