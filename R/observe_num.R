#' Observe the numerator
#'
#' This will return the numerator of the test statistic for data.
#'
#' @param sim Simulated data from sim_data
#' @param omega Weighting parameter for the radius
#'
#' @return The numerator of the test statistic
observe_num <- function(sim, omega = 1){
  # Pre-allocate vectors and number of groups
  g <- length(sim)
  group_sizes <- vector(mode = "numeric", length = g)
  center_means <- vector(mode = "numeric", length = g)
  radius_means <- vector(mode = "numeric", length = g)
  centers <- c()
  radius <- c()

  # Calculate means of the groups and create a vector of all centers/radii
  for (i in 1:g){
    center_means[i] <- mean(unlist(sim[[i]][,2]))
    radius_means[i] <- mean(unlist(sim[[i]][,3]))
    group_sizes[i] <- nrow(sim[[i]])
    centers <- rbind(centers, sim[[i]][,2])
    radius <- rbind(radius, sim[[i]][,3])
  }

  # Calculate the numerator
  grand_center <- mean(unlist(centers))
  grand_radius <- mean(unlist(radius))
  observed_null <- (sum(group_sizes*(center_means - grand_center)^2) +
                      omega*sum(group_sizes*(radius_means - grand_radius)^2))/(g-1)
  return(observed_null)
}
