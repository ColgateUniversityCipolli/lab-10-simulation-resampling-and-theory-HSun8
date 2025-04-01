# Henry Sun
# Lab 10 
#
library(tidyverse)
################################################################################
# Basic Simulation
sample.size <- 1004
polls <- 10000
basic.sim <- rbinom(n=polls, size = sample.size, prob = 0.39)/sample.size

ggplot()+
  geom_histogram(aes(x=basic.sim, y = after_stat(density)))+
  geom_density(aes(x=basic.sim))

# histogram + density is approximately normal
middle.95 <- quantile(x=basic.sim, 0.975) - quantile(x=basic.sim, 0.025)
moe <- 0.5*middle.95
# moe is ~3%, compared to Gallup's 4%

# again 
sample.size.new <- sample.size * 2
double.sim <- rbinom(n=polls, size = sample.size.new, prob = 0.39)/sample.size.new

ggplot()+
  geom_histogram(aes(x=double.sim, y = after_stat(density)))+
  geom_density(aes(x=double.sim))

d.middle.95 <- quantile(x=double.sim, 0.975) - quantile(x=double.sim, 0.025)
d.moe <- 0.5*d.middle.95
# moe is ~ 2.17%, compared to Gallup's 2% 
