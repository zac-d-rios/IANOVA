# install.packages("ggpubr")
# install.packages("latex2exp")
library(reshape2)
library(ggpubr)
library(latex2exp)
library(dplyr)
set.seed(8675)

hist_data5N <- rep(NA, 10000)
hist_data5D <- rep(NA, 10000)
for(i in 1:10000){
  sim <- sim_data(group_sizes = c(3, 4, 3, 3, 4),
                  dist = "gamma", corr = 0.5,
                  params1 = c(5, 10), params2 = c(2, 1))
  hist_data5N[i] <- observe_num(sim, omega = 1)
  hist_data5D[i] <- observe_den(sim, omega = 1)
}

hist_data100N <- rep(NA, 10000)
hist_data100D <- rep(NA, 10000)
for(i in 1:10000){
  sim <- sim_data(group_sizes = c(8, 7, 9, 8, 8),
                  dist = "gamma", corr = 0.5,
                  params1 = c(5, 10), params2 = c(2, 1))
  hist_data100N[i] <- observe_num(sim, omega = 1)
  hist_data100D[i] <- observe_den(sim, omega = 1)
}


hist_data200N <- rep(NA, 10000)
hist_data200D <- rep(NA, 10000)
for(i in 1:10000){
  sim <- sim_data(group_sizes = c(15, 14, 13, 15, 17),
                  dist = "gamma", corr = 0.5,
                  params1 = c(5, 10), params2 = c(2, 1))
  hist_data200N[i] <- observe_num(sim, omega = 1)
  hist_data200D[i] <- observe_den(sim, omega = 1)
}

hist_data500N <- rep(NA, 10000)
hist_data500D <- rep(NA, 10000)
for(i in 1:10000){
  sim <- sim_data(group_sizes = c(30, 33, 35, 30, 29),
                  dist = "gamma", corr = 0.5,
                  params1 = c(5, 10), params2 = c(2, 1))
  hist_data500N[i] <- observe_num(sim, omega = 1)
  hist_data500D[i] <- observe_den(sim, omega = 1)
}

hist_theory <- sim_theory_num(g = 5, dist = "gamma", corr = 0.5,
                              params1 = c(5, 10), params2 = c(2, 1), trials = 100000, 
                              omega = 1)

num_theory <- as.data.frame(hist_theory)

den_theory <- sim_theory_den(dist = "gamma", params1 = c(5, 10), params2 = c(2, 1), omega = 1)

numerator <- as.data.frame(cbind(hist_data5N, hist_data100N, hist_data200N, hist_data500N))

names(numerator) <- c("approx_3", "approx_8", "approx_15", "approx_30")

melted_num <- melt(numerator, id.vars = NULL)

ggplot(data=melted_num, aes(x=value)) +
  geom_histogram(aes(y=after_stat(density)), bins = 60, color = "darkblue", fill = "lightblue") +
  geom_density(data = num_theory, aes(x=hist_theory), color="darkred") +
  facet_wrap(~variable) +
  theme_bw() +
  xlim(0, 1000)

approx_3 <- hist_data5N/hist_data5D
approx_8 <- hist_data100N/hist_data100D
approx_15 <- hist_data200N/hist_data200D
approx_30 <- hist_data500N/hist_data500D
limit <- as.data.frame(hist_theory/den_theory)
names(limit) <- c("limit")

full_stat <- as.data.frame(cbind(approx_3, approx_8, approx_15, approx_30))

melted_full <- melt(full_stat, id.vars = NULL)

df2 <- melted_full %>%
  group_by(variable) %>%
  summarise(quant_95 = quantile(value, 0.95))


ggplot(data=melted_full, aes(x=value)) +
  geom_histogram(aes(y=after_stat(density)), bins = 65, color = "darkblue", fill = "lightblue") +
  geom_density(data = limit, aes(x = limit), color="darkred", size = 0.9) +
  facet_wrap(~variable) +
  theme_bw() +
  xlim(0, 6) +
  geom_vline(data = df2, mapping = aes(xintercept = quant_95, color = "Data"), linetype="dashed") +
  geom_vline(data = limit, mapping = aes(xintercept = quantile(limit, 0.95), color = "Limit"), linetype="solid") +
  scale_color_manual(name = "95th Quantile", values = c(Data = "blue", Limit = "red"))

