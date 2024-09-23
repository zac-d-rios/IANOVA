observe_stat_boot <- function(data, indices) {
  d <- data[indices,]

  #convert to list of data.tables for functions
  data <- split(d, 1:3)

  #return statistic
  return(observe_num_boot(data)/observe_den_boot(data))
}
