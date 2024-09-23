observe_den_boot <- function(sim, omega = 1){

  # Pre-allocate vectors and number of groups
  g <- length(sim)
  group_sizes <- vector(mode = "numeric", length = g)
  center_means <- vector(mode = "numeric", length = g)
  radius_means <- vector(mode = "numeric", length = g)
  deviations_center <- c()
  deviations_radius <- c()

  # Calculate all the deviations from group means
  for (i in 1:g){
    center_means[i] <- mean(unlist(sim[[i]][,2]))
    radius_means[i] <- mean(unlist(sim[[i]][,3]))
    group_sizes[i] <- nrow(sim[[i]])
    deviations_center <- rbind(deviations_center,
                               (sim[[i]][,2] - center_means[i])^2)
    deviations_radius <- rbind(deviations_radius,
                               (sim[[i]][,3] - radius_means[i])^2)
  }

  # Scale and combine the sum of the deviations
  var_center <- sum(deviations_center)
  var_radius <- sum(deviations_radius)
  obs_null <- var_center + omega*var_radius
  return(obs_null)
}

