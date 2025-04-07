# Henry Sun
# Lab 10 
#
library(tidyverse)
library(patchwork)
library(e1071)
################################################################################
# Basic Simulation
# parameters
sample.size <- 1004
num.polls <- 10000

# want to get simulation as a percent of how many Americans are satisified
# with US (divide by sample size)
basic.sim <- rbinom(n=num.polls, size = sample.size, prob = 0.39)/sample.size

# plot data
first.sim <- ggplot()+
  geom_histogram(aes(x=basic.sim, y = after_stat(density)))+
  geom_density(aes(x=basic.sim))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept=0.39, color = "red")+
  xlab("p")+
  theme_bw()+
  ggtitle("Sampling Distribution for p, n = 1004")

# histogram + density is approximately normal
# basic.sim.data <- summarize(tibble(x=basic.sim), 
#                             mean = mean(x),
#                             variance = var(x),
#                             skewness = skewness(x),
#                             e.kurtosis = kurtosis(x))

middle.95 <- quantile(x=basic.sim, 0.975) - quantile(x=basic.sim, 0.025)
moe <- 0.5*middle.95
# moe is ~3%, compared to Gallup's 4%

# again
sample.size.new <- sample.size * 2
double.sim <- rbinom(n=num.polls, size = sample.size.new, prob = 0.39)/sample.size.new

second.sim <- ggplot()+
  geom_histogram(aes(x=double.sim, y = after_stat(density)))+
  geom_density(aes(x=double.sim))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0.39, color = "red")+
  xlab("p")+
  theme_bw()+
  ggtitle("Sampling Distribution for p, n = 2008")

d.middle.95 <- quantile(x=double.sim, 0.975) - quantile(x=double.sim, 0.025)
d.moe <- 0.5*d.middle.95
# moe is ~ 2.17%, compared to Gallup's 2% 

# histogram + density still normal, however, variance is smaller
# moe and middle 95 also smaller for larger sample esize

task1.sims <- first.sim / second.sim

################################################################################
# Resampling

# create a tibble with gallup data: .39 satisfied, .59 unsatisified, .02 no answer
# round to catch all samples
satisfied <- rep(1, times = round(0.39 * sample.size))
unsatisfied <- rep(0, times = round(0.59 * sample.size))
no.opinion <- rep(NA, times = round(0.02 * sample.size))


# combine into tibble
gallup.data <- tibble(gallup = c(satisfied, unsatisfied, no.opinion))

# resampling
resamples <- 1000
resamples.data <- tibble(p.hat = numeric(resamples))
for (i in 1:resamples){
  curr.resample <- sample(x = gallup.data$gallup,
                          size = nrow(gallup.data),
                          replace = T)
  # compute the stat on the resample
  resamples.data$p.hat[i] <- mean(curr.resample, na.rm = T)
}

resampling.plot <- ggplot(data = resamples.data)+
  geom_histogram(aes(x = p.hat, y = after_stat(density)))+
  geom_density(aes(x=p.hat))+
  theme_bw()+
  geom_vline(xintercept = 0.39, color = "red")+
  geom_vline(xintercept = mean(resamples.data$p.hat), color = "green")+
  ggtitle("Sampling Distribution via Resampling, n = 1004")

resample.middle95 <- quantile(x=resamples.data$p.hat, 0.975) - 
                     quantile(x=resamples.data$p.hat, 0.025)
resample.moe <- 0.5 * resample.middle95

# shape is once again normal
# range of middle 95 is ~6.02%, moe is ~ 3%
# the margin of error by gallup is actually overestimated (4% compared to 3%)

################################################################################
# Simulation over n and p
n <- 3000
p <- 0.99
num.polls <- 10000
n.loop = seq(100, n, by = 10)
p.loop <- seq(0.01, p, by = 0.01)

np.sim.data <- tibble(n = numeric(), 
                      p = numeric(), 
                      moe.np = numeric())
for (n in n.loop){
  for(p in p.loop){
    new.sim <- rbinom(n=num.polls, size = n, prob = p)/n
    new.middle95 <- quantile(x=new.sim, 0.975) - quantile(x=new.sim, 0.025)
    new.moe <- 0.5*new.middle95
    np.sim.data <- bind_rows(np.sim.data, tibble(n=n, p=p, moe.np = new.moe))
  }
}

sim.np.plot <- ggplot(data = np.sim.data) +
  geom_raster(aes(x=p, y=n, fill=moe.np))+
  scale_fill_distiller("Margin of Error", palette = "Accent") +
  theme_bw() +
  geom_vline(xintercept = 0.39)+
  geom_hline(yintercept = 1004, color = "darkred") +
  geom_hline(yintercept = 2008, color = "green")

################################################################################
# Actual Margin of Error
z <- rnorm(n=10000)

(z.pinned1 <- quantile(x=z, 0.975) - quantile(x=z, 0.025))

# actual code
(z.pinned <- qnorm(0.975))

n.wilson <- 3000
n.loop.wilson <- seq(100, n.wilson, by = 10)

np.sim.data.wilson <- tibble(n.wilson = numeric(), 
                      p.wilson = numeric(), 
                      moe.wilson = numeric())
for (n in n.loop.wilson){
  for(p in p.loop){
    new.moe <- z.pinned * 
      ((sqrt(n*p*(1-p) + (z.pinned^2)/4)) / (n+z.pinned^2))
    np.sim.data.wilson <- bind_rows(np.sim.data.wilson, 
                             tibble(n.wilson=n, p.wilson=p, 
                                    moe.wilson = new.moe))
  }
}
wilson.np.plot <- ggplot(data = np.sim.data.wilson)+
  geom_raster(aes(x=p.wilson, y=n.wilson, fill = moe.wilson))+
  scale_fill_viridis_c("Wilson Margin of Error", option = "H")+
  theme_bw()+
  ylab("n")+
  geom_vline(xintercept = 0.39)+
  geom_hline(yintercept = 1004, color = "darkred")+
  geom_hline(yintercept = 2008, color = "green")

raster.plots <- (sim.np.plot / wilson.np.plot)  

