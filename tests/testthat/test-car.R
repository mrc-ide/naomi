context("utils")

test_that("scale_gmrf_precision() returns expected values", {

  adj <- Matrix::sparseMatrix(i = c(1, 1, 2, 3), j = c(2, 4, 4, 5),
                              x = 1, dims = c(6, 6), symmetric = TRUE)
  Q <- Matrix::diag(x = Matrix::rowSums(adj)) - adj

  Qscaled <- Q
  Qscaled[c(1,2,4), c(1,2,4)] <- 2/9 * Q[c(1,2,4), c(1,2,4)]
  Qscaled[c(3,5), c(3,5)] <- 1/4 * Q[c(3,5), c(3,5)]
  Qscaled[6,6] <- 1

  res <- scale_gmrf_precision(Q)

  expect_is(res, "dsCMatrix")
  expect_equal(res@i, Qscaled@i)
  expect_equal(res@x, Qscaled@x)
})

test_that("create adjacency matrix from shapefile", {

  demo_areas2 <- dplyr::filter(demo_area_hierarchy, area_level == 2)
  demo_areas2 <- dplyr::left_join(demo_areas2, demo_area_boundaries, by = "area_id")
  demo_areas2 <- sf::st_as_sf(demo_areas2)

  adj2 <- create_adj_matrix(demo_areas2)

  edges <- cbind(c(1, 2, 2, 3, 3, 3, 4, 4, 5, 5),
                 c(2, 1, 3, 2, 4, 5, 3, 5, 3, 4))
  no_edges <- cbind(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5),
                    c(1, 3, 4, 5, 2, 4, 5, 1, 3, 1, 2, 4, 1, 2, 5))

  expect_equal(adj2[edges], rep(1, 10))
  expect_equal(adj2[no_edges], rep(0, 15))
})
  
