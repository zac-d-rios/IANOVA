#' Simulate denominator for limiting distribution
#'
#' This function allows the user to get the denominator of the limiting distribution
#'
#' @param dist Original distribution for data
#' @param params1 Original parameters for data
#' @param params2 Original parameters for data
#' @param omega Weighting parameter for radii
#'
#' @return One number, a linear combination of the variances
sim_theory_den <- function(dist, params1, params2, omega = 1){

  #Get variance parameters depending on distribution
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

  # Create combination of variances
  denom <- t_params2[1] + omega*t_params2[2]
  return(denom)

}
