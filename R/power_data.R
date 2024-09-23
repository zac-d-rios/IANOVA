sim_power_analysis_data <- function(alternative, ...){
  data1 <- sim_data(params1=alternative[1], ...)
  data2 <- sim_data(params1=alternative[2], ...)

  return(list(data1, data2))
}
