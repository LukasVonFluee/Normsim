# Title: Threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.

# Simulation will be a function with the following parameters as arguments:

# phi -> which we might want to vary: 0.25, 0.5, 0.75
# a   -> which we might wan to vary: 0.25, 0.75
# t_max
# h, g -> where h > g. these are the payoffs for choosing Alt / SQ respectively.
# alpha, beta -> beta distribution parameters.

################################################################################

rm(list=ls())

# package dependencies

library(ggplot2)

################################################################################

# Coordination game payoff matrix:

#       SQ      Alt
# SQ    a+x_i   b+x_i
# Alt   a       d
# where: a+x_i > a, b+x_i<d, for each agent.

a <- 0.75 # will be one of the parameters we vary

################################################################################

# Parameter values for testing before writing simulation as a function:

t_max <- 100
a <- 0.25
phi <- 0.25
target <- 0 # target=0 means we target amenable, target=1 we target resistant

################################################################################

# Note: All of the following should go into the simulation function

# Initialization

N <- 100  # Will use it as an argument in the simulation function

# Parameters specifying payoffs in coordination game:

b <- 0
d <- 1    

# Post int payoffs for targeted agents who actually respond and switch:

g <- 0
h <- 2

# Define agents and their traits. Note, "exp_sq" and "exp_alt" refer to the 
# expected payoffs associated with choosing SQ and Alt respectively.
# For "choice" we define choice=0 as choosing SQ and choice=1 as choosing Alt.
# "q" is belief that next randomly selected partner will play Alt.

agent <- data.frame(x_i = rep(NA, N), q = rep(NA, N), exp_sq = rep(NA, N), 
                    exp_alt = rep(NA, N), choice = rep(0, N), payoff = rep(NA, N))

# Draw x_i values from a left-skewed beta distribution such that for a majority 
# of agents it is true that x_i > (d-b)/2.
# Might want to vary those values (in paper we also have: alpha <- 8, beta <- 2)

alpha <- 3  # Will use it as an argument in the simulation function      
beta <- 2   # Will use it as an argument in the simulation function    

x_values <- rbeta(N, shape1 = alpha, shape2 = beta) * (d - b)

agent$x_i <- x_values

# Density plot of x_i values:

ggplot(data = data.frame(x = agent$x_i), aes(x)) +
   geom_density() +
   labs(title = "Density Plot of x_i", x = "x_i", y = "Density")

# Given we assume everyone chooses SQ before intervention, we assign initial 
# payoffs accordingly (note: payoff for coordinating on SQ: a+x_i):

agent$payoff <- a + agent$x_i

################################################################################
################################################################################

# Simulation function.

# Notes:

# INTERVENTION: As discussed with Charles, we do intervention before first 
# period because we are only interested in dynamics after the intervention.
# Agents switch with a probability = 1-(x_i/(d-b)). Note, q=x_i/(d-b) is simply
# the fraction of agents choosing Alt that makes an agent indifferent between
# choosing Alt and SQ. Further in the paper it is assumed that a given agent 
# switches to Alt if the expected payoff of Alt is >= expected payoff of SQ.

sim <- function (N, t_max, alpha, beta, target, phi, a, g, h) {
  
  # Initialize data frame in which we store coordination results
  
  coordination <- data.frame(freq_coord_sq=rep(NA,t_max+1), freq_coord_alt=rep(NA,t_max+1))
  
  # Before intervention, we assume everyone chooses SQ:
  
  coordination$freq_coord_sq[1] <- N/2
  coordination$freq_coord_alt[1] <- 0
  
  # Initialize agents (note, we assume agents start from an equilibrium where 
  # everyone chooses SQ (choice=0)):
  
  agent <- data.frame(x_i = rep(NA, N), q = rep(NA, N), exp_sq = rep(NA, N), 
                      exp_alt = rep(NA, N), choice = rep(0, N), payoff = rep(NA, N))
  
  # Draw x_i values from a left-skewed beta distribution such that for a majority 
  # of agents it is true that x_i > (d-b)/2.
  # Might want to vary those values (in paper we also have: alpha <- 8, beta <- 2)
  
  alpha <- 3  # Will use it as an argument in the simulation function      
  beta <- 2   # Will use it as an argument in the simulation function    
  
  x_values <- rbeta(N, shape1 = alpha, shape2 = beta) * (d - b)
  agent$x_i <- x_values
  
  # Initialize array to record agents' traits in every period:
  
  num_traits <- length(agent[1,])   # number of traits per agent
  output <- array(NA, dim = c(N, num_traits, 1+t_max))
  
  # rename columns
  
  # Set dimnames
  dimnames(output) <- list(
    agent = as.character(1:N),
    trait = c("x_i", "q", "exp_sq", "exp_alt", "choice", "payoff"),
    period = 1:(1+t_max)
  )
  
  # Check the dimnames
  # dimnames(output)
  
  #############################################################################  

  # Intervention (note, if target=0, we target amenable, else resistant)
  
  if (target==0) {
    # IMPORTANT: NEED TO ORDER AGENTS ACCORDING x_i VALUES FIRST.
    # If we target most amenable, we order in increasing order:
    agent <- agent[order(agent$x_i), ]
    # Response to intervention is probabilistic and proportional to threshold.
    # Note, an agent's threshold value is given by; agent$x_i[1:(N*phi)]/(d-b)
    # We make response a decreasing function of threshold values and define
    # the probability to switch as; 1-agent$x_i[1:(N*phi)]/(d-b)
    # If target=0, that means we target most amenable agents. We do so in an 
    # increasing order, i.e. we first target most amenable
    prob_draw <- runif(N*phi)
    agent$choice[1:(N*phi)] <- ifelse((agent$choice[1:(N*phi)]==0) & (prob_draw>(1-(agent$x_i[1:(N*phi)]/(d-b)))), 1, 0)  
    } else if(target==1) {
      # If we target most resistant, we order in decreasing order:
      agent <- agent[order(agent$x_i, decreasing = TRUE), ]
      prob_draw <- runif(N*phi)
      agent$choice[1:(N*phi)] <- ifelse((agent$choice[1:(N*phi)]==0) & (prob_draw>(1-(agent$x_i[1:(N*phi)]/(d-b)))), 1, 0)  
  }
  
  # Before first period, register agents' x_i values and choices:
  
  output[,,1] <- as.matrix(agent)
    
  #############################################################################  
  
  for (t in 1:t_max) {
    
    # copy agent to previous_agent data-frame
    
    previous_agent <- agent  
    
    # Determine beliefs:
    
    agent$q <- mean(previous_agent$choice)
    
    agent$exp_sq <- (1-agent$q[1])*(a + agent$x_i) + agent$q[1]*(b+agent$x_i)
    
    agent$exp_alt <- (1-agent$q[1])*a + agent$q[1]*d
    
    # Determine choice based on expected payoffs. Note: An agent chooses Alt if
    # exp_alt >= exp_sq:
    
    agent$choice <- ifelse(agent$exp_alt>=agent$exp_sq, 1, 0)
    
    # Match agents to play coordination game and assign corresponding payoffs:
    
    shuffled_indices <- sample(N)
    
    # Split shuffled indices into two halves
    player_1 <- shuffled_indices[1:(N/2)]
    player_2 <- shuffled_indices[((N/2)+1):N]
    
    # Retrieve choice values for each half
    choice_player_1 <- agent$choice[player_1]
    choice_player_2 <- agent$choice[player_2]
    
    # Compare choice values and record coordination
    both_sq <- choice_player_1 == 0 & choice_player_2 == 0
    both_alt <- choice_player_1 == 1 & choice_player_2 == 1
    freq_coord_sq <- sum(both_sq)
    freq_coord_alt <- sum(both_alt)
    
    # Store results in 'coordination' data frame:
    coordination$freq_coord_sq[1+t] <- freq_coord_sq
    coordination$freq_coord_alt[1+t] <- freq_coord_alt
    
    # Calculate payoffs:
    payoff_first <- numeric(N/2)
    payoff_second <- numeric(N/2)
    
    payoff_first[both_sq] <- a + agent$x_i[player_1]
    payoff_second[both_alt] <- a + x
    
    
    # Record agent data:
    
    output[,,1+t] <- as.matrix(agent)
    
  }
  
################################################################################
  
# Plot results
  
  # Notes: For coordination results, we recorded coordination on SQ and on Alt
  # We can get coordination overall by adding those frequencies and we can get
  # miscoordination frequency with: 
  # (N/2) - (coordination$freq_coord_sq[t] + coordination$freq_coord_alt[t])
  
}



################################################################################
################################################################################

# session information
xfun::session_info()

# cite R
citation()
