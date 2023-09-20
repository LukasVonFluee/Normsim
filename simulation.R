# Title: Simulation for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.

################################################################################

# package dependencies

library(truncnorm)
library(ggplot2)

################################################################################

# Coordination game payoff matrix:

#       SQ      Alt
# SQ    a+x_i   b+x_i
# Alt   a       d
# where: a+x_i > a, b+x_i<d, for each agent.

################################################################################

# Initialization

N <- 1000  # Number of agents

# Parameters specifying payoffs in coordination game:

a <- 1.5
b <- 1
d <- 2    

# Draw x_i values from a left-skewed beta distribution such that for a majority 
# of agents it is true that x_i > (d-b)/2:

alpha <- 3        
beta <- 2         

agent <- data.frame(x_i = rbeta(N, shape1 = alpha, shape2 = beta) * (d - b), choice = rep(NA, N), payoff = rep(NA, N))

# Create a density plot of x_i

ggplot(data = data.frame(x = agent$x_i), aes(x)) +
  geom_density() +
  labs(title = "Density Plot of x_i", x = "x_i", y = "Density")





################################################################################

# session information
xfun::session_info()

# cite R
citation()
