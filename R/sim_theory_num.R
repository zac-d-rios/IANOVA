#' Simulate theoretical distribution
#'
#' This function simulates the theoretical limiting distribution for IV-ANOVA.
#'  It requires the distribution of the data and the original parameters.
#'
#' @param g Number of groups
#' @param corr Correlation between the center and radius
#' @param dist Distribution of the original data, used in getting variance
#' @param params1 First set of parameters for the data of original distribution
#' @param params2 Second set of parameters for the data of original distribution
#' @param omega Weighting parameter for radii
#' @param trials Number of observations used to construct distribution
#'
#' @return A vector of values that simulate the limiting distribution
#'
#' @export
sim_theory_num <- function(g, corr, dist, params1, params2, omega = 1, trials){

  # Set parameters for the normal distribution used in the limiting case
  t_params1 <- c(0, 0)
  if (dist == "normal" || dist == "cauchy"){
    t_params2 <- params2
  }
  if (dist == "gamma"){
    t_params2 <- vector(mode = "numeric", length = 2)
    for (i in 1:2){
      temp_SR <- gammaGetShapeRate(params1[i], params2[i])
      t_params2[i] <- temp_SR$shape/(temp_SR$rate^2)
    }
  }
  if (dist == "uniform"){
    t_params2 <- (params2 - params1)^2/12
  }
  if (dist == "poisson"){
    t_params2 <- params1
  }
  if (dist == "binary"){
    t_params2 <- c(params1*(1-params1), params2[1]/params2[2]^2)
  }

  # Simulate the normal distribution used in the limiting case
  theory_null <- vector(mode = "list", length = g-1)
  for (i in 1:(g-1)){
    theory_null[[i]] <- genCorGen(trials, 2,
                                  params1 = t_params1, params2 = t_params2,
                                  dist = "normal",
                                  corMatrix = matrix(c(1, corr, corr, 1), nrow=2),
                                  wide = TRUE)
  }

  # Combine the radii and center distributions
  nulls <- c()
  for (i in 1:(g-1)){
    nulls <- cbind(nulls, theory_null[[i]][,2:3])
  }
  nulls <- nulls^2
  even_ind <- 2*(1:(g-1))
  odd_ind <- even_ind - 1
  centers <- nulls[,..odd_ind]
  radius <- nulls[,..even_ind]

  output <- (rowSums(centers) + omega*rowSums(radius))/(g-1)

  return(output)

}
