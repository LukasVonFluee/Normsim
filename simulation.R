################################################################################
# Title: Threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
################################################################################
rm(list=ls())
setwd("~/Desktop/n_sim10000_more_par")
################################################################################
################################################################################

# Package dependencies:

# To calculate Gini coefficient:

# if (!requireNamespace("DescTools", quietly = TRUE)) {
#   install.packages("DescTools")
# }
# 
# library(DescTools)

# Gini function: "Gini()"

# Gini function (write my own function):
# References: Gini (1912), Schmidt & Wichardt (2019):

my_gini <- function(x) {
  n <- length(x)  # Number of observations
  
  sum_x <- sum(x)  # Sum of observations
  
  sorted_x <- sort(x)  # Sort observations
  
  i <- 1:n
  numerator <- sum(2 * i * sorted_x)
  denominator <- n * sum_x
  
  gini <- numerator / denominator - (n + 1) / n
  
  return(gini)
}

################################################################################
################################################################################
################################################################################

# Simulation will be a function with the following parameters as arguments:
#  alpha = c(3,8): beta distribution parameter
#  target = c(0, 1): target=0 = target N*phi most amenable, where N = nr. agents
#  phi = c(0.25, 0.5, 0.75): fraction of targeted agents
#  a = c(0.25, 0.75): payoff parameter
#  h = c(2,3): payoff for choosing Alt for targeted agents who actually switch

################################################################################
################################################################################
################################################################################

# Coordination game payoff matrix:

#       SQ      Alt
# SQ    a+x_i   b+x_i
# Alt   a       d
# where: a+x_i > a, b+x_i<d, for each agent.

################################################################################
################################################################################

# Simulation function:

################################################################################
################################################################################

# Notes:

# INTERVENTION: As discussed with Charles, we do intervention before first 
# period because we are only interested in dynamics after the intervention.
# Agents switch with a probability = 1-(x_i/(d-b)). Note, q=x_i/(d-b) is simply
# the fraction of agents choosing Alt that makes an agent indifferent between
# choosing Alt and SQ. Further in the paper it is assumed that a given agent 
# switches to Alt if the expected payoff of Alt is >= expected payoff of SQ.

sim <- function (N, t_max, alpha, target, phi, a, h) {
  
  # Parameters specifying payoffs in coordination game that we don't vary:
  b <- 0
  d <- 1    
  g <- 0 # payoff for choosing SQ for targeted agents who switch
  
  # Initialize data frame in which we store coordination results
  summary_results <- data.frame(freq_coord_sq=rep(0,t_max), 
                                freq_coord_alt=rep(0,t_max),
                                miscoordination=rep(0,t_max),
                                freq_sq=rep(0,t_max),
                                freq_alt=rep(0,t_max),
                                avg_payoff_sq=rep(0,t_max),
                                avg_payoff_alt=rep(0,t_max),
                                avg_payoff=rep(0,t_max),
                                gini_coefficient=rep(0,t_max),
                                exp_sq=rep(0,t_max),
                                exp_alt=rep(0,t_max))
  
  # Before intervention, we assume everyone chooses SQ:
  summary_results$freq_coord_sq[1] <- N/2
  summary_results$freq_coord_alt[1] <- 0
  summary_results$miscoordination[1] <- 0
  summary_results$freq_sq[1] <- N
  summary_results$freq_alt[1] <- 0
  
  # Initialize agents (note, we assume agents start from an equilibrium where 
  # everyone chooses SQ (choice=0)).
  # "x_i" = idiosyncratic x value
  # "respond" = register whether an agent responded to intervention or not
  # "exp_sq" = expected payoff for choosing SQ
  # "exp_alt" = expected payoff for choosing Alt
  # "choice" = choice between SQ and Alt, where choice=0 means choosing SQ
  # "payoff" = payoff after playing coordination game in a given period
  agent <- data.frame(x_i = rep(0, N), respond = rep(0, N), 
                      exp_sq = rep(0, N), exp_alt = rep(0), 
                      choice = rep(0, N), payoff = rep(0, N), q=rep(0, N))
  
  # Draw x_i values from a left-skewed beta distribution such that for a majority 
  # of agents it is true that x_i > (d-b)/2.
  # alpha # Will use it as an argument in the simulation function      
  beta <- 2 # we don't vary beta, so we define it here  
  x_values <- rbeta(N, shape1 = alpha, shape2 = beta) * (d - b)
  agent$x_i <- x_values
  
  # Density plot of x_i values:
  # ggplot(data = data.frame(x = agent$x_i), aes(x)) +
  #   geom_density() +
  #   labs(title = "Density Plot of x_i", x = "x_i", y = "Density")
  
  # Initialize array to record agents' traits over all periods:
  num_traits <- length(agent[1,])   # number of traits
  agent_output <- array(0, dim = c(N, num_traits, t_max))
  
  # rename columns
  dimnames(agent_output) <- list(
    agent = as.character(1:N),
    trait = c("x_i", "respond", "exp_sq", "exp_alt", "choice", "payoff", "q"),
    period = 1:t_max
  )
  
  # Check the dimnames
  # dimnames(agent_output)
  
  # In an SQ equilibrium, agent$q=0 
  # We let them play the coordination game:
  # This is t=1
  t <- 1
  
  # Beliefs
  agent$q <- ifelse(agent$choice==0, sum(agent$choice)/(N-1), ((sum(agent$choice)-1)/(N-1)))
  
  #Expected payoffs:
  agent$exp_sq <- ((1-agent$q)*(a + agent$x_i)) + (agent$q*(b+agent$x_i))
  agent$exp_alt <- (1-agent$q)*a + agent$q*d
  
  # Record average expected payoffs of this period:
  summary_results$exp_sq[t] <- sum(agent$exp_sq)/N
  summary_results$exp_alt[t] <- sum(agent$exp_alt)/N
  
  # Determine choice based on expected payoffs. Note: An agent chooses Alt if
  # exp_alt >= exp_sq:
  agent$choice <- ifelse(agent$exp_alt>=agent$exp_sq, 1, 0)
  
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
  # Store results in 'summary_results' data frame:
  summary_results$freq_coord_sq[t] <- freq_coord_sq
  summary_results$freq_coord_alt[t] <- freq_coord_alt
  summary_results$miscoordination[t] <- (N/2) - freq_coord_sq - freq_coord_alt
  
  # Calculate payoffs:
  payoff_player_1 <- numeric(N/2)
  payoff_player_2 <- numeric(N/2)
  
  # If both coordinate on SQ, payoff is: a + x_i 
  payoff_player_1[both_sq] <- a + agent$x_i[player_1][both_sq]
  payoff_player_2[both_sq] <- a + agent$x_i[player_2][both_sq]
  
  # If both coordinate on Alt, payoff is 'd' for non-targeted agents and 
  # for agents who were targeted but didn't switch:
  payoff_player_1[both_alt] <- d
  payoff_player_2[both_alt] <- d
  
  # If player 1 chooses SQ and player 2 chooses Alt:
  payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- b + agent$x_i[player_1][choice_player_1 == 0 & choice_player_2 == 1] 
  payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
  
  # If player 1 chooses Alt and player 2 chooses SQ:
  payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
  payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- b + agent$x_i[player_2][choice_player_1 == 1 & choice_player_2 == 0] 
  
  # Register payoffs in agent data frame
  agent[player_1,"payoff"] <- payoff_player_1
  agent[player_2,"payoff"] <- payoff_player_2
  
  # Record agent data:
  agent_output[,,t] <- as.matrix(agent)
  
  # Record summary results
  
  # Record frequencies of choices:
  summary_results$freq_sq[t] <- length(agent[which(agent$choice==0),"choice"])
  summary_results$freq_alt[t] <- length(agent[which(agent$choice==1),"choice"])
  
  # Record average payoffs:
  num_agents_sq <- length(agent[which(agent$choice==0),"choice"])
  summary_results$avg_payoff_sq[t] <- ifelse(num_agents_sq>0,sum(agent[which(agent$choice==0),"payoff"]) / num_agents_sq,NA) 
  
  num_agents_alt <- length(agent[which(agent$choice==1),"choice"])
  summary_results$avg_payoff_alt[t] <- ifelse(num_agents_alt>0,sum(agent[which(agent$choice==1),"payoff"]) / num_agents_alt,NA) 
  
  summary_results$avg_payoff[t] <- sum(agent[,"payoff"])/N
  
  # Calculate (unweighted) Gini Coefficient:
  summary_results$gini_coefficient[t] <- my_gini(agent[,"payoff"])
  
  #############################################################################  
  
  # Intervention (note, if target=0, we target amenable, else resistant)
  if (target==0) {
    # print("target=0")
    # We order agents in increasing order according to their x_i values, and 
    # target N*phi agents with lowest x_i values:
    agent <- agent[order(agent$x_i), ]
    # Response to intervention is probabilistic and proportional to threshold.
    # Note, an agent's threshold value is given by; agent$x_i/(d-b)
    # We make response a decreasing function of threshold values and define
    # the probability to switch as; 1-(agent$x_i/(d-b))
    prob_draw <- runif(N*phi)
    agent$choice[1:(N*phi)] <- ifelse(prob_draw<=(1-(agent$x_i[1:(N*phi)]/(d-b))), 1, 0)  
  } else if(target==1) {
    # print("target=1")
    # If we target most resistant, we order in decreasing order:
    agent <- agent[order(agent$x_i, decreasing = TRUE), ]
    prob_draw <- runif(N*phi)
    agent$choice[1:(N*phi)] <- ifelse(prob_draw<=(1-(agent$x_i[1:(N*phi)]/(d-b))), 1, 0)  
  }
  
  # For those agents who switched, expected payoff for choosing SQ will be 'g' 
  # for the rest of periods, and expected payoff for choosing Alt will be 'h'. 
  # Current solution to always assign those agents these expected payoffs: 
  # Mark those agents who actually responded to intervention and switched to Alt
  agent$respond <- agent$choice
  
  #############################################################################  
  
  for (t in 2:t_max) {
    
    # Determine belief q based on choice distribution of previous period:
    agent$q <- ifelse(agent$choice==0, sum(agent$choice)/(N-1), ((sum(agent$choice)-1)/(N-1)))
    
    # Calculate expected payoffs:
    
    # Note: agent$respond=1 refers to those agents who were targeted by 
    # intervention and actually switched choice. For them, we have that:
    # exp_sq <- g and exp_alt <- h
    agent[which(agent$respond==1),"exp_sq"] <- g
    agent[which(agent$respond==1),"exp_alt"] <- h
    
    # Expected payoffs for all other agents:
    agent[which(agent$respond==0),"exp_sq"] <- ((1-agent[which(agent$respond==0),"q"])*(a + agent[which(agent$respond==0),"x_i"])) + (agent[which(agent$respond==0),"q"]*(b+agent[which(agent$respond==0),"x_i"]))
    agent[which(agent$respond==0),"exp_alt"] <- (1-agent[which(agent$respond==0),"q"])*a + agent[which(agent$respond==0),"q"]*d
    
    # Record average expected payoffs of this period:
    summary_results$exp_sq[t] <- sum(agent$exp_sq)/N
    summary_results$exp_alt[t] <- sum(agent$exp_alt)/N
    
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
    # Store results in 'summary_results' data frame:
    summary_results$freq_coord_sq[t] <- freq_coord_sq
    summary_results$freq_coord_alt[t] <- freq_coord_alt
    summary_results$miscoordination[t] <- (N/2) - freq_coord_sq - freq_coord_alt
    
    # Calculate payoffs:
    payoff_player_1 <- numeric(N/2)
    payoff_player_2 <- numeric(N/2)
    
    # If both coordinate on SQ, payoff is: a + x_i 
    payoff_player_1[both_sq] <- a + agent$x_i[player_1][both_sq]
    payoff_player_2[both_sq] <- a + agent$x_i[player_2][both_sq]
    
    # If both coordinate on Alt, payoff is 'd' for non-targeted agents and 
    # for agents who were targeted but didn't switch:
    payoff_player_1[both_alt] <- d
    payoff_player_2[both_alt] <- d
    
    # If player 1 chooses SQ and player 2 chooses Alt:
    payoff_player_1[choice_player_1 == 0 & choice_player_2 == 1] <- b + agent$x_i[player_1][choice_player_1 == 0 & choice_player_2 == 1] 
    payoff_player_2[choice_player_1 == 0 & choice_player_2 == 1] <- a
    
    # If player 1 chooses Alt and player 2 chooses SQ:
    payoff_player_1[choice_player_1 == 1 & choice_player_2 == 0] <- a
    payoff_player_2[choice_player_1 == 1 & choice_player_2 == 0] <- b + agent$x_i[player_2][choice_player_1 == 1 & choice_player_2 == 0] 
    
    # Register payoffs in agent data frame
    agent[player_1,"payoff"] <- payoff_player_1
    agent[player_2,"payoff"] <- payoff_player_2
    
    # Note: All agents that initially responded to intervention always choose
    # Alt and receive h in each period:
    agent[which(agent$respond==1),"payoff"] <- h
    
    # Record agent data:
    agent_output[,,t] <- as.matrix(agent)
    
    # Record summary results
    
    # Record frequencies of choices:
    summary_results$freq_sq[t] <- length(agent[which(agent$choice==0),"choice"])
    summary_results$freq_alt[t] <- length(agent[which(agent$choice==1),"choice"])
    
    # Record average payoffs:
    num_agents_sq <- length(agent[which(agent$choice==0),"choice"])
    summary_results$avg_payoff_sq[t] <- ifelse(num_agents_sq>0,sum(agent[which(agent$choice==0),"payoff"]) / num_agents_sq,NA) 
    
    num_agents_alt <- length(agent[which(agent$choice==1),"choice"])
    summary_results$avg_payoff_alt[t] <- ifelse(num_agents_alt>0,sum(agent[which(agent$choice==1),"payoff"]) / num_agents_alt,NA) 
    
    summary_results$avg_payoff[t] <- sum(agent[,"payoff"])/N
    
    # Calculate (unweighted) Gini Coefficient:
    summary_results$gini_coefficient[t] <- my_gini(agent[,"payoff"])
    
  }
  
  # Return the required data frames, and some parameter values in a list:
  return(list(summary_results=summary_results, # agent_output=agent_output, 
              N=N, t_max=t_max, alpha=alpha, target=target, phi=phi, a=a, h=h))
  
}

################################################################################
################################################################################
# The different parameter values to create all parameter combinations:

param_combinations <- expand.grid(
  alpha = c(2.5, 2.75, 3, 8),
  target = c(0,1),
  phi = c(0.25,0.5,0.75),
  a = c(0.25,0.75),
  h = c(2,3)
)

################################################################################
################################################################################

# RUN SIMULATION:

################################################################################
################################################################################

# Run simulations 100 times per parameter combination to account for the random 
# x values drawn from the beta distribution, which could significantly influence 
# the results of a given simulation.
# It also accounts for another random process in the simulation function, namely
# where we draw random values from a uniform distribution to determine which 
# agents actually respond to the intervention.

N <- 1000
t_max <- 100
num_traits <- 7 # nr. of agents' traits needed to initialize averaged data frame
n_sim <- 1000 # nr. of simulation runs

results_list <- list()  # to store results for each parameter combination

# Go through all the different parameter combinations
for (i in 1:nrow(param_combinations)) {
  
  current_params <- param_combinations[i, ]
  
  avg_summary_results <- data.frame(freq_coord_sq=rep(0,t_max), 
                                    low_ci_freq_coord_sq=rep(0,t_max),
                                    high_ci_freq_coord_sq=rep(0,t_max),
                                    freq_coord_alt=rep(0,t_max),
                                    low_ci_freq_coord_alt=rep(0,t_max),
                                    high_ci_freq_coord_alt=rep(0,t_max),
                                    miscoordination=rep(0,t_max),
                                    low_ci_miscoordination=rep(0,t_max),
                                    high_ci_miscoordination=rep(0,t_max),
                                    freq_sq=rep(0,t_max),
                                    low_ci_freq_sq=rep(0,t_max),
                                    high_ci_freq_sq=rep(0,t_max),
                                    freq_alt=rep(0,t_max),
                                    low_ci_freq_alt=rep(0,t_max),
                                    high_ci_freq_alt=rep(0,t_max),
                                    avg_payoff_sq=rep(0,t_max),
                                    avg_payoff_alt=rep(0,t_max),
                                    avg_payoff=rep(0,t_max),
                                    low_ci_avg_payoff=rep(0,t_max),
                                    high_ci_avg_payoff=rep(0,t_max),
                                    gini_coefficient=rep(0,t_max),
                                    low_ci_gini_coefficient=rep(0,t_max),
                                    high_ci_gini_coefficient=rep(0,t_max),
                                    exp_sq=rep(0,t_max),
                                    exp_alt=rep(0,t_max))
  
  # Create data frame to store the following measures for each individual simulation run:
  # freq_sq, freq_alt, avg_payoff, gini_coefficient (others may or may not be necessary)
  # We do so, to use this data later to calculate confidence intervals:
  
  freq_coord_sq_n_sim <- matrix(0,t_max,n_sim)
  freq_coord_alt_n_sim <- matrix(0,t_max,n_sim)
  miscoordination_n_sim <- matrix(0,t_max,n_sim)
  freq_sq_n_sim <- matrix(0,t_max,n_sim)
  freq_alt_n_sim <- matrix(0,t_max,n_sim)
  # avg_payoff_sq_n_sim <- matrix(0,t_max,n_sim)
  # avg_payoff_alt_n_sim <- matrix(0,t_max,n_sim)
  avg_payoff_n_sim <- matrix(0,t_max,n_sim)
  gini_coefficient_n_sim <- matrix(0,t_max,n_sim)
  # exp_sq_n_sim <- matrix(0,t_max,n_sim)
  # exp_alt_n_sim <- matrix(0,t_max,n_sim)
  
  # Run n_sim simulations:
  for (j in 1:n_sim) {
    results <- sim(N=N, t_max=t_max,
                   alpha=current_params$alpha,
                   target=current_params$target, 
                   phi=current_params$phi, 
                   a=current_params$a, 
                   h=current_params$h)
    
    # Density plot of x values:
    # ggplot(data = data.frame(x = results$agent_output[,"x_i",1]), aes(x)) +
    #   geom_density() +
    #   labs(title = "Density Plot of x_i", x = "x_i", y = "Density")
    
    N <- N
    t_max <- t_max
    alpha <- current_params$alpha
    target <- current_params$target 
    phi <- current_params$phi 
    a <- current_params$a 
    h <- current_params$h
    
    # Record following measures for all n_sim simulation runs
    freq_sq_n_sim[,j] <- results$summary_results$freq_sq
    freq_alt_n_sim[,j] <- results$summary_results$freq_alt
    freq_coord_sq_n_sim[,j] <- results$summary_results$freq_coord_sq
    freq_coord_alt_n_sim[,j] <- results$summary_results$freq_coord_alt
    miscoordination_n_sim[,j] <- results$summary_results$miscoordination
    avg_payoff_n_sim[,j] <- results$summary_results$avg_payoff
    gini_coefficient_n_sim[,j] <- results$summary_results$gini_coefficient
    
  }
  
  # After running n_sim simulations for a given parameter combination, we take
  # averages of all the measures:
  avg_summary_results$freq_coord_sq <- rowMeans(freq_coord_sq_n_sim)
  avg_summary_results$freq_coord_alt <- rowMeans(freq_coord_alt_n_sim)
  avg_summary_results$miscoordination <- rowMeans(miscoordination_n_sim)
  avg_summary_results$freq_sq <- rowMeans(freq_sq_n_sim)
  avg_summary_results$freq_alt <- rowMeans(freq_alt_n_sim)
  avg_summary_results$avg_payoff <- rowMeans(avg_payoff_n_sim)
  avg_summary_results$gini_coefficient <- rowMeans(gini_coefficient_n_sim)
  
  # Now that we have the mean values, averaged over the n_sim simulation runs, 
  # we can calculate the bootstrapped confidence intervals:
  
  all_measures <- list()
  
  all_measures[[1]] <- freq_sq_n_sim
  all_measures[[2]] <- freq_alt_n_sim
  all_measures[[3]] <- freq_coord_sq_n_sim
  all_measures[[4]] <- freq_coord_alt_n_sim
  all_measures[[5]] <- miscoordination_n_sim
  all_measures[[6]] <- avg_payoff_n_sim
  all_measures[[7]] <- gini_coefficient_n_sim
  
  all_ci_values <- array(0, dim = c(t_max, 2, length(all_measures))) 
  
  # rename columns
  dimnames(all_ci_values) <- list(
    period = 1:t_max,
    ci_values = c("lower_bound", "upper_bound"),
    measures = c("freq_sq_n_sim", "freq_alt_n_sim", "freq_coord_sq_n_sim", "freq_coord_alt_n_sim", "miscoordination_n_sim", "avg_payoff_n_sim", "gini_coefficient_n_sim")
  )
  
  for (k in 1:length(all_measures)) {
    
    # Number of bootstrap replicates
    R <- 999  
    
    # Create an empty matrix to store the confidence intervals
    conf_intervals <- matrix(NA, nrow = t_max, ncol = 2)
    
    for (period in 1:t_max) {
      # Go through the "all_measures" list and for each measure, extract the
      # data for current period
      data_to_bootstrap <- all_measures[[k]][period, ]
      
      # Initialize an empty vector to store bootstrap means
      bootstrap_means <- numeric(R)
      
      # Perform bootstrapping R times
      for (l in 1:R) {
        # Resample the data with replacement
        resampled_data <- sample(data_to_bootstrap, replace = TRUE)
        
        # Calculate the mean for the resampled data
        bootstrap_means[l] <- mean(resampled_data)
      }
      
      mean_and_boot <- rep(0,1+R)
      
      mean_and_boot[1:R] <- bootstrap_means
      mean_and_boot[R+1] <- rowMeans(all_measures[[5]])[period]
        
      ordered_means <- sort(mean_and_boot)
      
      index_low <- floor(0.025 * length(ordered_means))
      lower_bound <- ordered_means[index_low]
      
      index_high <- ceiling(0.975 * length(ordered_means))
      upper_bound <- ordered_means[index_high]
      
      # Store the confidence interval in the "all_ci_values" matrix
      all_ci_values[period,1,k] <- lower_bound
      all_ci_values[period,2,k] <- upper_bound
      
    }
    
    # print(all_ci_values)
    
  }  
  
  # Record confidence intervals in avg_summary_results data frame
  avg_summary_results$low_ci_freq_sq <- all_ci_values[,"lower_bound","freq_sq_n_sim"]
  avg_summary_results$high_ci_freq_sq <- all_ci_values[,"upper_bound","freq_sq_n_sim"]
  avg_summary_results$low_ci_freq_alt <- all_ci_values[,"lower_bound","freq_alt_n_sim"]
  avg_summary_results$high_ci_freq_alt <- all_ci_values[,"upper_bound","freq_alt_n_sim"]
  avg_summary_results$low_ci_freq_coord_sq <- all_ci_values[,"lower_bound","freq_coord_sq_n_sim"]
  avg_summary_results$high_ci_freq_coord_sq <- all_ci_values[,"upper_bound","freq_coord_sq_n_sim"]
  avg_summary_results$low_ci_freq_coord_alt <- all_ci_values[,"lower_bound","freq_coord_alt_n_sim"]
  avg_summary_results$high_ci_freq_coord_alt <- all_ci_values[,"upper_bound","freq_coord_alt_n_sim"]
  avg_summary_results$low_ci_miscoordination <- all_ci_values[,"lower_bound","miscoordination_n_sim"]
  avg_summary_results$high_ci_miscoordination <- all_ci_values[,"upper_bound","miscoordination_n_sim"]
  avg_summary_results$low_ci_avg_payoff <- all_ci_values[,"lower_bound","avg_payoff_n_sim"]
  avg_summary_results$high_ci_avg_payoff <- all_ci_values[,"upper_bound","avg_payoff_n_sim"]
  avg_summary_results$low_ci_gini_coefficient <- all_ci_values[,"lower_bound","gini_coefficient_n_sim"] 
  avg_summary_results$high_ci_gini_coefficient <- all_ci_values[,"upper_bound","gini_coefficient_n_sim"] 
  
  # Store averaged results and parameter values of given parameter combination in results_list:
  results_list[[i]] <- list(summary_results = avg_summary_results, # agent_output = all_agent_output, 
                            n_sim=n_sim, N=N, t_max=t_max, num_traits=num_traits, alpha=alpha, target=target,
                            phi=phi, a=a, h=h)
}

################################################################################
################################################################################
# Create names for files according to parameter combinations:
name_combination <- function(row) {
  paste0("alpha_", row["alpha"],
         "_target_", row["target"], 
         "_phi_", row["phi"], 
         "_a_", row["a"],
         "_h_", row["h"])
}

names(results_list) <- apply(param_combinations, 1, name_combination)

# Replace '.' and '_' with '' (remove them)
cleaned_names <- lapply(names(results_list), function(name) {
  return(gsub("[._]", "", name))
})

cleaned_names <- unlist(cleaned_names)

# get current working directory
results_dir <- getwd() 

# Iterate through the results_list and save each result separately in 
# subdirectory corresponding to parameter combinations:
for(i in 1:length(results_list)) {
  # Name for the subdirectory (and the file) based on the names of results_list
  folder_and_file_name <- cleaned_names[i]
  
  # Check if directory exists and create if it doesn't
  if (!dir.exists(folder_and_file_name)) {
    dir.create(folder_and_file_name)
  }
  
  # File name with .RData extension
  file_name <- paste0(folder_and_file_name, ".RData")
  
  # Full path to where the file will be saved (inside the subdirectory)
  file_path <- file.path(results_dir, folder_and_file_name, file_name)
  
  # Convert list item to an environment
  e <- list2env(list(result = results_list[[i]]))
  
  # save the environment
  save(list = "result", envir = e, file = file_path)
}

################################################################################
################################################################################

# session information
xfun::session_info()

# cite R
citation()
