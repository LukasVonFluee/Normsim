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

# library(ggplot2)

################################################################################

# Coordination game payoff matrix:

#       SQ      Alt
# SQ    a+x_i   b+x_i
# Alt   a       d
# where: a+x_i > a, b+x_i<d, for each agent.

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

sim <- function (N, t_max, alpha, target, phi, a, h) {
  
  # Parameters specifying payoffs in coordination game:
  b <- 0
  d <- 1    
  g <- 0
  
  # Initialize data frame in which we store coordination results
  coordination <- data.frame(freq_coord_sq=rep(NA,t_max+1), freq_coord_alt=rep(NA,t_max+1))
  
  # Before intervention, we assume everyone chooses SQ:
  coordination$freq_coord_sq[1] <- N/2
  coordination$freq_coord_alt[1] <- 0
  
  # Initialize agents (note, we assume agents start from an equilibrium where 
  # everyone chooses SQ (choice=0)):
  agent <- data.frame(x_i = rep(NA, N), q = rep(NA, N), respond = rep(NA, N), 
                      exp_sq = rep(NA, N), exp_alt = rep(NA, N), 
                      choice = rep(0, N), payoff = rep(NA, N))
  
  # Draw x_i values from a left-skewed beta distribution such that for a majority 
  # of agents it is true that x_i > (d-b)/2.
  # Might want to vary those values (in paper we also have: alpha <- 8, beta <- 2)
  
  # alpha <- 3 # Will use it as an argument in the simulation function      
  beta <- 2 # we don't vary beta, so we define it here  
  
  x_values <- rbeta(N, shape1 = alpha, shape2 = beta) * (d - b)
  agent$x_i <- x_values

  # Given we assume everyone chooses SQ before intervention, we assign initial 
  # payoffs accordingly (note: payoff for coordinating on SQ: a+x_i):
  agent$payoff <- a + agent$x_i
  
  # Initialize array to record agents' traits in every period:
  num_traits <- length(agent[1,])   # number of traits per agent
  output <- array(NA, dim = c(N, num_traits, 1+t_max))
  
  # rename columns
  
  # Set dimnames
  dimnames(output) <- list(
    agent = as.character(1:N),
    trait = c("x_i", "q", "respond", "exp_sq", "exp_alt", "choice", "payoff"),
    period = 1:(1+t_max)
  )
  
  # Check the dimnames
  # dimnames(output)
  
  #############################################################################  

  # Intervention (note, if target=0, we target amenable, else resistant)
  if (target==0) {
    print("target=0")
    # We order agents according to their x_i values first and target N*phi first rows of agents
    # If we target most amenable, we order in increasing order:
    agent <- agent[order(agent$x_i), ]
    # Response to intervention is probabilistic and proportional to threshold.
    # Note, an agent's threshold value is given by; agent$x_i[1:(N*phi)]/(d-b)
    # We make response a decreasing function of threshold values and define
    # the probability to switch as; 1-agent$x_i[1:(N*phi)]/(d-b)
    # If target=0, that means we target most amenable agents. We do so in an 
    # increasing order, i.e. we first target most amenable
    prob_draw <- runif(N*phi)
    agent$choice[1:(N*phi)] <- ifelse(prob_draw<=(1-(agent$x_i[1:(N*phi)]/(d-b))), 1, 0)  
  } else if(target==1) {
    print("target=1")
    # If we target most resistant, we order in decreasing order:
    agent <- agent[order(agent$x_i, decreasing = TRUE), ]
    prob_draw <- runif(N*phi)
    agent$choice[1:(N*phi)] <- ifelse(prob_draw<=(1-(agent$x_i[1:(N*phi)]/(d-b))), 1, 0)  
  }
  
  # For those agents who switched, expected payoff for SQ will be 'g' for the rest
  # of periods, and expected payoff for Alt will be 'h'. Current solution to 
  # always assign those agents these expected payoffs: Mark those agents who 
  # actually responded to intervention and switched to Alt
  
  agent$respond <- agent$choice
  
  # Record agent data frame with x values and payoffs before intervention:
  output[,,1] <- as.matrix(agent)
    
  #############################################################################  
  
  for (t in 1:t_max) {
    
    # Determine belief q based on choice distribution of previous period:
    agent$q <- mean(agent$choice)
    
    # Calculate expected payoffs:
    
    # Note: agent$respond=1 refers to those agents who were targeted by 
    # intervention and actually switched choice. For them, we have that:
    # exp_sq <- g and exp_alt <- h
    agent[which(agent$respond==1),"exp_sq"] <- g
    agent[which(agent$respond==1),"exp_alt"] <- h
    
    # Expected payoffs for all other agents:
    agent[which(agent$respond==0),"exp_sq"] <- (1-agent[which(agent$respond==0),"q"])*(a + agent[which(agent$respond==0),"x_i"]) + agent[which(agent$respond==0),"q"]*(b+agent[which(agent$respond==0),"x_i"])
    agent[which(agent$respond==0),"exp_alt"] <- (1-agent[which(agent$respond==0),"q"])*a + agent[which(agent$respond==0),"q"]*d
    
    # Determine choice based on expected payoffs. Note: An agent chooses Alt if
    # exp_alt >= exp_sq:
    agent$choice <- ifelse(agent$exp_alt>=agent$exp_sq, 1, 0)
    
    # Match agents to play coordination game and assign corresponding payoffs:
    
    # Create array of randomized indices
    shuffled_indices <- sample(N)
    
    # Split shuffled indices into two halves
    player_1 <- shuffled_indices[1:(N/2)]
    player_2 <- shuffled_indices[((N/2)+1):N]
    
    # Retrieve choice values for each half
    choice_player_1 <- agent$choice[player_1]
    choice_player_2 <- agent$choice[player_2]
    
    # Compare choice values and record frequency of coordination
    both_sq <- choice_player_1 == 0 & choice_player_2 == 0
    both_alt <- choice_player_1 == 1 & choice_player_2 == 1
    freq_coord_sq <- sum(both_sq)
    freq_coord_alt <- sum(both_alt)
    
    # Store results in 'coordination' data frame:
    coordination$freq_coord_sq[1+t] <- freq_coord_sq
    coordination$freq_coord_alt[1+t] <- freq_coord_alt
    
    # Calculate payoffs:
    payoff_player_1 <- numeric(N/2)
    payoff_player_2 <- numeric(N/2)
    
    # If both coordinate on SQ, payoff is: a + x_i 
    payoff_player_1[both_sq] <- a + agent$x_i[player_1][both_sq]
    payoff_player_2[both_sq] <- a + agent$x_i[player_1][both_sq]
    
    # If both coordinate on Alt, payoff is 'd' for non-targeted agents and 
    # for agents who were targeted but didn't switch:
    payoff_player_1[both_alt] <- d
    payoff_player_2[both_alt] <- d
    
    # If player 1 chooses SQ and player 2 chooses Alt:
    payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- agent$x_i[player_1][choice_player_1 == 0 & choice_player_2 == 1] + b
    payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
    
    # If player 1 chooses Alt and player 2 chooses SQ:
    payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
    payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- agent$x_i[player_1][choice_player_1 == 1 & choice_player_2 == 0] + b
    
    # Register payoffs in agent data frame
    agent[player_1,"payoff"] <- payoff_player_1
    agent[player_2,"payoff"] <- payoff_player_2
    
    # Note: All agents that initially responded to intervention always choose
    # Alt and receive h:
    agent[which(agent$respond==1),"payoff"] <- h
  
    # Record agent data:
    output[,,1+t] <- as.matrix(agent)
    
  }
  
  # Return the required data frames, and some parameter values in a list:
  return(list(coordination=coordination, output=output, N=N, t_max=t_max))
  
}

################################################################################
################################################################################
# The different parameter combinations that we go through:

param_combinations <- expand.grid(
  alpha = c(3,8),
  target = c(0, 1),
  phi = c(0.25, 0.5, 0.75),
  a = c(0.25, 0.75),
  h = c(2,3)
)


################################################################################
################################################################################
# Run simulations 100 times per parameter combination to account for the random 
# x values drawn from the beta distribution, which could significantly influence 
# the results of a given simulation.

N <- 100
t_max <- 100
num_traits <- 7 # nr. of agents' traits needed to initialize averaged data frame

results_list <- list()  # to store results for each parameter combination

for (i in 1:nrow(param_combinations)) {
  current_params <- param_combinations[i, ]
  
  avg_coordination <- data.frame(freq_coord_sq=rep(0,t_max+1), freq_coord_alt=rep(0,t_max+1))
  avg_output <- array(0, dim = c(N, num_traits, 1+t_max))
  
  for (j in 1:100) {
    results <- sim(N=N, t_max=t_max,
                   alpha=current_params$alpha,
                   target=current_params$target, 
                   phi=current_params$phi, 
                   a=current_params$a, 
                   h=current_params$h)
    
    avg_coordination$freq_coord_sq <- avg_coordination$freq_coord_sq + results$coordination$freq_coord_sq
    avg_coordination$freq_coord_alt <- avg_coordination$freq_coord_alt + results$coordination$freq_coord_alt
    avg_output <- avg_output + results$output
  }
  
  avg_coordination$freq_coord_sq <- avg_coordination$freq_coord_sq / 100
  avg_coordination$freq_coord_alt <- avg_coordination$freq_coord_alt / 100
  avg_output <- avg_output / 100
  
  # Store the average results in results_list:
  results_list[[i]] <- list(coordination = avg_coordination, output = avg_output, N=N, t_max=t_max)
}

################################################################################
################################################################################
# Create names for files with parameter combinations:
name_combination <- function(row) {
  paste0("alpha_", row["alpha"],
         "_target_", row["target"], 
         "_phi_", row["phi"], 
         "_a_", row["a"],
         "_h_", row["h"])
}

names(results_list) <- apply(param_combinations, 1, name_combination)

results_dir <- getwd() # Directory where results will be saved

# Iterate through the results_list and save each result separately
for(i in 1:length(results_list)) {
  # File name based on the names of results_list
  file_name <- paste0(names(results_list)[i], ".RData")
  
  # Full path to where the file will be saved
  file_path <- file.path(results_dir, file_name)
  
  # Convert list item to an environment
  e <- list2env(list(result = results_list[[i]]))
  
  # Save the environment
  save(list = "result", envir = e, file = file_path)
}

################################################################################
################################################################################

# session information
xfun::session_info()

# cite R
citation()
