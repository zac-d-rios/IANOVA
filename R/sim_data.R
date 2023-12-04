#' Simulate Data for IV-ANOVA
#'
#' This function allows the user to simulate interval data with different
#'  distributions and center/range correlations.
#'
#' @param group_sizes Vector of group sizes
#' @param dist Distribution to generate data from (string)
#' @param corr Correlation between center and radius
#' @param params1 First set of parameters for the distribution
#' @param params2 Second set of parameters for the distribution
#' @param ... Additional parameters to be passed into genCorGen
#'
#' @return A list, where each element is the data for a group
sim_data <- function(group_sizes, dist, corr, params1, params2, ...){
  g <- length(group_sizes)
  data <- vector(mode = "list", length = g)
  if (dist == "poisson"){
    params2 <- NULL
  }

  # Generate groups as different elements in the list
  if (dist == "cauchy") {
    for(i in 1:g){
      data[[i]] <- genCorGen(group_sizes[i], 2,
                             params1 = c(0, 0), params2 = c(1, 1),
                             dist = "normal",
                             corMatrix = matrix(c(1, corr, corr, 1), nrow = 2),
                             wide = TRUE)
      data[[i]]$V1 <- qcauchy(pnorm(data[[i]]$V1), location = params1[1], scale = params2[1])
      data[[i]]$V2 <- qcauchy(pnorm(data[[i]]$V2), location = params1[2], scale = params2[2])
    }

  }
  else if (dist == "binary") {
    for(i in 1:g){
      data[[i]] <- genCorGen(group_sizes[i], 2,
                             params1 = c(0, 0), params2 = c(1, 1),
                             dist = "normal",
                             corMatrix = matrix(c(1, corr, corr, 1), nrow = 2),
                             wide = TRUE)
      data[[i]]$V1 <- qbinom(pnorm(data[[i]]$V1), size = 1, prob = params1)
      data[[i]]$V2 <- qgamma(pnorm(data[[i]]$V2), shape = params2[1], rate = params2[2])
    }

  }
  else {
    for(i in 1:g){
      data[[i]] <- genCorGen(group_sizes[i], 2,
                             params1 = params1, params2 = params2,
                             dist = dist,
                             corMatrix = matrix(c(1, corr, corr, 1), nrow = 2),
                             wide = TRUE)
    }
  }
  return(data)
}
