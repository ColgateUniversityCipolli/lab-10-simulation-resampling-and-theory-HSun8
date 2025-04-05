# Henry Sun
# Lab 10 
#
library(tidyverse)
library(patchwork)
################################################################################
# Basic Simulation
sample.size <- 1004
polls <- 10000
basic.sim <- rbinom(n=polls, size = sample.size, prob = 0.39)/sample.size
# need to divide by sample size (turn into percent)
ggplot()+
  geom_histogram(aes(x=basic.sim, y = after_stat(density)))+
  geom_density(aes(x=basic.sim))+
  geom_hline()

# histogram + density is approximately normal
middle.95 <- quantile(x=basic.sim, 0.975) - quantile(x=basic.sim, 0.025)
moe <- 0.5*middle.95
# moe is ~3%, compared to Gallup's 4%

# again
sample.size.new <- sample.size * 2
double.sim <- rbinom(n=polls, size = sample.size.new, prob = 0.39)/sample.size.new

ggplot()+
  geom_histogram(aes(x=double.sim, y = after_stat(density)))+
  geom_density(aes(x=double.sim))+
  geom_hline(yintercept=0)+
  theme_bw()

d.middle.95 <- quantile(x=double.sim, 0.975) - quantile(x=double.sim, 0.025)
d.moe <- 0.5*d.middle.95
# moe is ~ 2.17%, compared to Gallup's 2% 

################################################################################
# resampling
# create a tibble with gallup data: .39 satisfied, .59 unsatisified, .02 no answer
satisfied <- rep(1, times = round(0.39 * sample.size))
unsatisfied <- rep(0, times = round(0.59 * sample.size))
no.opinion <- rep(NA, times = round(0.02 * sample.size))
gallup.data <- tibble(gallup = c(satisfied, unsatisfied, no.opinion))

resamples <- 1000
resamples.data <- tibble(p.hat = numeric(resamples))
for (i in 1:resamples){
  curr.resample <- sample(x = gallup.data$gallup,
                          size = nrow(gallup.data),
                          replace = T)
  # compute the stat on the resample
  resamples.data$p.hat[i] <- mean(curr.resample, na.rm = T)
  
}
ggplot(data = resamples.data)+
  geom_histogram(aes(x = p.hat, y = after_stat(density)))+
  geom_density(aes(x=p.hat))

resample.middle95 <- quantile(x=resamples.data$p.hat, 0.975) - 
                     quantile(x=resamples.data$p.hat, 0.025)
resample.moe <- 0.5 * resample.middle95

