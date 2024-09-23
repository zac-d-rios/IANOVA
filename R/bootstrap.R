bootstrap_dist = rep(NA, 1000)
tests = rep(NA, 1000)

data <- sim_data(group_sizes = c(30, 30, 30),
                dist = "gamma", corr = 0.5,
                params1 = c(5, 10), params2 = c(2, 1))
tempo <- rbindlist(data)

data1  <- split(tempo, 1:3)

bleh <- boot(tempo, observe_stat_boot, 100)

observe_stat_boot(tempo)

set.seed(90210)
tests = rep(NA, 1000)
time_start <- Sys.time()
for (i in 1:1000) {
  temp_data <- sim_data(group_sizes = c(8, 8, 8),
                        dist = "gamma", corr = 0,
                        params1 = c(2, 2), params2 = c(1, 1))
  bootstrap <- boot(rbindlist(temp_data), observe_stat_boot, 1000)

  cutoff <- quantile(bootstrap$t, probs = 0.95)
  tests[i] <- (bootstrap$t0 > cutoff)
}
time_end <- Sys.time()
time_taken <- time_start - time_end

set.seed(9015)
tests = rep(NA, 1000)
time_start <- Sys.time()
for (i in 1:1000) {
  temp_data <- sim_data(group_sizes = c(15, 15, 15),
                        dist = "gamma", corr = 0,
                        params1 = c(2, 2), params2 = c(1, 1))
  bootstrap <- boot(rbindlist(temp_data), observe_stat_boot, 1000)

  cutoff <- quantile(bootstrap$t, probs = 0.95)
  tests[i] <- (bootstrap$t0 > cutoff)
}
time_end <- Sys.time()
time_taken <- time_start - time_end

set.seed(1540)
tests = rep(NA, 1000)
time_start <- Sys.time()
for (i in 1:1000) {
  temp_data <- sim_data(group_sizes = c(30, 30, 30),
                        dist = "gamma", corr = 0,
                        params1 = c(2, 2), params2 = c(1, 1))
  bootstrap <- boot(rbindlist(temp_data), observe_stat_boot, 1000)

  cutoff <- quantile(bootstrap$t, probs = 0.95)
  tests[i] <- (bootstrap$t0 > cutoff)
}
time_end <- Sys.time()
time_taken <- time_start - time_end

set.seed(8675309)
time_start <- Sys.time()
hist_theory <- sim_theory_num(g = 3, dist = "gamma", corr = 0,
                              params1 = c(2, 2), params2 = c(1, 1), trials = 1000000,
                              omega = 1)
num_theory <- as.data.frame(hist_theory)
den_theory <- sim_theory_den(dist = "gamma", params1 = c(2, 2), params2 = c(1, 1), omega = 1)
limit <- as.data.frame(hist_theory/den_theory)
cutoff <- quantile(limit$`hist_theory/den_theory`, probs = seq(0.9, 1, 0.01))[6]
tests_asymp = rep(NA, 1000)
for (i in 1:1000) {
  temp_data <- sim_data(group_sizes = c(8, 8, 8),
                        dist = "gamma", corr = 0,
                        params1 = c(2, 2), params2 = c(1, 1))
  obs <- observe_num(temp_data)/observe_den(temp_data)
  tests_asymp[i] <- (obs > cutoff)

}
time_end <- Sys.time()
time_taken <- time_start - time_end
sum(tests_asymp)
time_taken

set.seed(8675)
time_start <- Sys.time()
hist_theory <- sim_theory_num(g = 3, dist = "gamma", corr = 0,
                              params1 = c(2, 2), params2 = c(1, 1), trials = 1000000,
                              omega = 1)
num_theory <- as.data.frame(hist_theory)
den_theory <- sim_theory_den(dist = "gamma", params1 = c(2, 2), params2 = c(1, 1), omega = 1)
limit <- as.data.frame(hist_theory/den_theory)
cutoff <- quantile(limit$`hist_theory/den_theory`, probs = seq(0.9, 1, 0.01))[6]
tests_asymp = rep(NA, 1000)
for (i in 1:1000) {
  temp_data <- sim_data(group_sizes = c(15, 15, 15),
                        dist = "gamma", corr = 0,
                        params1 = c(2, 2), params2 = c(1, 1))
  obs <- observe_num(temp_data)/observe_den(temp_data)
  tests_asymp[i] <- (obs > cutoff)

}
time_end <- Sys.time()
time_taken <- time_start - time_end
sum(tests_asymp)
time_taken

set.seed(2024)
time_start <- Sys.time()
hist_theory <- sim_theory_num(g = 3, dist = "gamma", corr = 0,
                              params1 = c(2, 2), params2 = c(1, 1), trials = 1000000,
                              omega = 1)
num_theory <- as.data.frame(hist_theory)
den_theory <- sim_theory_den(dist = "gamma", params1 = c(2, 2), params2 = c(1, 1), omega = 1)
limit <- as.data.frame(hist_theory/den_theory)
cutoff <- quantile(limit$`hist_theory/den_theory`, probs = seq(0.9, 1, 0.01))[6]
tests_asymp = rep(NA, 1000)
for (i in 1:1000) {
  temp_data <- sim_data(group_sizes = c(30, 30, 30),
                        dist = "gamma", corr = 0,
                        params1 = c(2, 2), params2 = c(1, 1))
  obs <- observe_num(temp_data)/observe_den(temp_data)
  tests_asymp[i] <- (obs > cutoff)

}
time_end <- Sys.time()
time_taken <- time_start - time_end
sum(tests_asymp)
time_taken


