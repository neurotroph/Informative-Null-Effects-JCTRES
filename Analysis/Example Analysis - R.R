# Proving the Null: Statistical Techniques and Inferential Frameworks
# Supplemental R script
# Christopher Harms & Daniël Lakens
#
# Twitter:  @chrisharms, @lakens
# GitHub:   neurotroph,  Lakens
# Email:    christopher.harms@uni-bonn.de
#           DLakens@tue.nl
#
# This script contains the analyses presented in the article with some
# additional comments.
# The following packages are required:
#   pwr, TOSTER, BEST, dplyr, hypergeo, ggplot2
# Install with the following command to your R console:
#   install.packages(c('pwr', 'TOSTER', 'BEST', 'dplyr', 'hypergeo', 'ggplot2'))
#
# Please make sure to set the current working directory to the Analysis folder,
# e.g. by entering
#   setwd('/Users/.../Downloads/JCTRES-Supplementals/Analysis/')
# into your R console.

# Load packages ----------------------------------------------------------------
library(pwr)
library(TOSTER)
library(BEST)
library(dplyr)
library(ggplot2)
source('../R-Scripts/BF10_tTest_Informed.R')

# Power analysis for imaginary study -------------------------------------------
# Smallest Effect Size of Interest (Cohen's d)
d.sesoi <- 0.3
# Alpha level
alpha.level <- .01
# Desired power
priori.power <- .90

# Power analysis
pwr.t.test(d = d.sesoi, sig.level = alpha.level, power = priori.power)

# Load example data ------------------------------------------------------------
df <- read.csv('../Example Data/Example_Data.csv')

# Visualize data and descriptive results ---------------------------------------
# Simple box plot of results
plot(DV ~ Group, data = df)

# Descriptives
summarise(group_by(df, Group),
          mean = mean(DV),
          sd = sd(DV),
          count = n(),
          min = min(DV),
          max = max(DV))

# Analysis I: Traditional Significance Test ------------------------------------
t.test(DV ~ Group, data = df, alternative = "two.sided")

# Analysis II: Equivalence Test ------------------------------------------------
TOSTtwo.raw(mean(df[df$Group == "Meditation",]$DV),
            mean(df[df$Group == "Waiting List",]$DV),
            sd(df[df$Group == "Meditation",]$DV),
            sd(df[df$Group == "Waiting List",]$DV),
            length(df[df$Group == "Meditation",]$DV),
            length(df[df$Group == "Meditation",]$DV),
            # Equivalence Bounds
            low_eqbound = -9,
            high_eqbound = +9,
            # Significance Level
            alpha = 0.01,
            var.equal = F, plot = T)

# Analysis III: Bayesian Estimation --------------------------------------------
# Perform Bayesian estimation via Monte Carlo sampling
best <- BESTmcmc(df[df$Group == "Meditation",]$DV,
                 df[df$Group == "Waiting List",]$DV,
                 rnd.seed = 20180309, verbose = FALSE,
                 numSavedSteps = 1e4,
                 parallel = TRUE)

# Get posterior difference between the two groups
df.best <- data.frame(diff = (best$mu1 - best$mu2))
summary(best)
posterior.mode <- summary(best)["muDiff","mode"]

# Construct 95% Posterior HDI
hdi <- hdi(df.best$diff, credMass = 0.95)
hdi['lower']
hdi['upper']

# Create plot of Posterior difference between the two groups
ggplot(data=df.best, aes(x = diff)) +
  # Histogram and Density estimation for Posterior difference between means
  geom_histogram(aes(y = ..density..), fill = "white", color = "darkgrey",
                 binwidth = .25) +
  geom_density(color = "darkgrey") +
  # Bounds of Region of Practical Equivalence
  geom_vline(xintercept = -9, linetype = "dashed") +
  geom_vline(xintercept = 9, linetype = "dashed") +
  # Difference of exactly zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  # 95% Highest Density Interval
  geom_segment(x = hdi['lower'], xend = hdi['upper'], y = 0, yend = 0,
               size = 1.1) +
  # Posterior Mode: Most credible point estimate
  geom_point(x = posterior.mode, y = 0, color = "black", shape = 15,
             size = 3) +
  # Label axes
  scale_x_continuous(name = expression(paste(mu[1], " - ", mu[2]))) +
  scale_y_continuous(name = "Density")

# Analysis IV: Bayes factor ----------------------------------------------------
# Constructing a prior for the alternative model based on the meta-analysis by
# Hofmann et al. (2007): d = .62 (95% CI [.25;.98])
# The goal is to contruct a normal distribution as prior with mean .62 and a 
# standard deviation derived from the Confidence Interval to express the
# meta-analytic uncertainty
bf.alt.mu <- .62
bf.alt.sd <- (.98 - .25)/(2 * qnorm(0.975)) * sqrt(4)

# Calculate Bayes factor for t-test with informed alternative
# The function used here is taken from the JASP-Engine source code (released 
# under GPLv2, see source-code in BF10_tTest_Informed.R)
bf10 <- BayesFactor_InformedNormal(# t-value from t-test
                                   abs(t.test(DV ~ Group, data = df,
                                              alternative = "two.sided")$statistic),
                                   # Group sizes
                                   nrow(df)/2, nrow(df)/2,
                                   # Alternative prior
                                   prior.mean = bf.alt.mu,
                                   prior.variance = bf.alt.sd^2)
# BF10: Bayes factor in favour of the alternative
bf10
# BF01: Bayes factor in favour of the null model (reciprocal of BF10)
1/bf10

