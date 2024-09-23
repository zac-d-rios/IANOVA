set.seed(1539) # swap seeds to get original results from 100
time_start <- Sys.time()
powers1 = rep(NA, 6)
for (d in 1:6) {
  tests = rep(NA, 1000)
  for (i in 1:1000) {
    sim1 <- sim_data(group_sizes = c(30, 30),
                     dist = "gamma", corr = 0,
                     params1 = params1_pow, params2 = params2_pow)

    sim2 <-  sim_data(group_sizes = c(30),
                      dist = "gamma", corr = 0,
                      params1 = (params1_pow + d*0.4*params2_pow), params2 = params2_pow)

    temp_data <- c(sim1, sim2)
    stat <- observe_num_boot(temp_data)/observe_den_boot(temp_data)
    bootstrap <- boot(rbindlist(temp_data), observe_stat_boot, 1000)


    cutoff <- quantile(bootstrap$t, probs = 0.95)
    tests[i] <- (stat > cutoff)
  }
  powers1[d] = sum(tests)
  print('power1')
  print(d)
  print(powers1[d])
}
time_end <- Sys.time()
time_taken <- time_start - time_end
powers1

set.seed(90210)
time_start <- Sys.time()
powers2 = rep(NA, 6)
for (d in 1:6) {
  tests = rep(NA, 1000)
  for (i in 1:1000) {
    sim1 <- sim_data(group_sizes = c(30, 30),
                     dist = "gamma", corr = 0,
                     params1 = params1_pow, params2 = params2_pow)

    sim2 <-  sim_data(group_sizes = c(30),
                      dist = "gamma", corr = 0,
                      params1 = c((2+0.4*d), 2), params2 = params2_pow)

    temp_data <- c(sim1, sim2)
    stat <- observe_num_boot(temp_data)/observe_den_boot(temp_data)
    bootstrap <- boot(rbindlist(temp_data), observe_stat_boot, 1000)

    cutoff <- quantile(bootstrap$t, probs = 0.95)
    tests[i] <- (stat > cutoff)
  }
  powers2[d] = sum(tests)
  print('power2')
  print(d)
  print(powers2[d])
}
time_end <- Sys.time()
time_taken <- time_start - time_end
powers2
