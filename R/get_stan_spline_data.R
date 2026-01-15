
get_stan_spline_data <- function(num_knots, spline_degree){
  knots <- c(seq(0, 1, length.out = num_knots), 1000)
  grid <- c(seq(from = 0, to = 1, by = .02), 1000) # generating inputs
  B <- t(splines::bs(grid, knots = knots, degree = spline_degree, intercept = FALSE))
  B <- B[1:(nrow(B) - 1), ]
  num_grid <- length(grid)
  num_basis <- nrow(B)
  ext_knots <- c(rep(knots[1], spline_degree), knots, rep(knots[length(knots)], spline_degree))
  stan_spline_data <- list(
    num_knots = length(knots),
    knots = knots,
    num_grid = num_grid,
    spline_degree = spline_degree,
    grid = grid,
    B = B,
    num_basis = length(knots) + spline_degree - 1,
    k = num_basis - (spline_degree+1)  #num_basis - num_constrained_zero
  #  num_constrained_zero = spline_degree + 1
  )
  return(stan_spline_data)
}
