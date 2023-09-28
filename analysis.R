###################################################################################
# Title: Analysis of threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
###################################################################################

# Parameters specifying payoffs in coordination game that we don't vary:
b <- 0
d <- 1    
g <- 0 

# Package dependencies:

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  # Install ggplot2 if not already installed
  install.packages("ggplot2")
}

library(ggplot2)

###################################################################################
# Load data with file names corresponding to parameter combinations
###################################################################################

base_dir <- getwd()

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Initialize a list to store all loaded results
all_results <- list()

# Loop through each subdirectory and load the .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Load each .RData file and store in the all_results list
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Extract just the parameter combination name from the full path
    param_name <- basename(dirname(rdata_file))
    
    # Save the result with the parameter name
    all_results[[param_name]] <- result
  }
}

###################################################################################
# Load results:

length(all_results)

for (i in 1:length(all_results)) {
  
  current_output <- all_results[[i]]
  
  print(paste("The current parameter combination being analyzed is:", names(all_results)[i]))
  
  ###################################################################################
  # Create additional variables of interest
  ###################################################################################
  
  # Fraction of targeted agents who actually switched:
  response_fract <- sum(current_output$agent_output[,"respond",1,]) / (N*phi) / n_sim
  
  # Frequency of miscoordination
  current_output$summary_results$miscoordination <- 
    (N/2) - 
    current_output$summary_results$freq_coord_sq - 
    current_output$summary_results$freq_coord_alt
  
  ###################################################################################
  ###################################################################################
  # Plots
  ###################################################################################
  ###################################################################################
  
  ###################################################################################
  # Plotting frequencies of coordination and miscoordination
  ###################################################################################
  freq_coordination <- current_output$summary_results %>%
    pivot_longer(cols = c("freq_coord_alt", "freq_coord_sq", "miscoordination"), names_to = "type", values_to = "value")
  
  plot <- ggplot(freq_coordination, aes(x = rep(1:(1+t_max), times = 3), y = value, color = type)) +
    geom_line() +
    labs(title = "Frequency of (mis)coordination", x = "Period", y = "Frequency") +
    scale_color_manual(name = "Norm coordination",
                       values = c("green", "blue","red"), # adjust the colors if needed
                       labels = c("Coordination on Alt", "Coordination on SQ", "Miscoordination")) +
    theme_minimal()
  
  # Construct the filename with directory path
  filename <- file.path(names(all_results)[i], "freq_coordination.png")
  
  # Save the plot
  ggsave(filename = filename, plot = plot)
  
  ###################################################################################
  # Plotting frequencies of coordination and miscoordination as fractions
  ###################################################################################
  current_output$summary_results$fract_coord_alt <- 
    current_output$summary_results$freq_coord_alt / (current_output$summary_results$freq_coord_alt 
                                                                  + current_output$summary_results$freq_coord_sq
                                                                  + current_output$summary_results$miscoordination)
  
  current_output$summary_results$fract_coord_sq <- 
    current_output$summary_results$freq_coord_sq / (current_output$summary_results$freq_coord_alt 
                                                                 + current_output$summary_results$freq_coord_sq
                                                                 + current_output$summary_results$miscoordination)
  
  current_output$summary_results$fract_miscoordination <- 
    current_output$summary_results$miscoordination / (current_output$summary_results$freq_coord_alt 
                                                                   + current_output$summary_results$freq_coord_sq
                                                                   + current_output$summary_results$miscoordination)
  
  fract_coordination <- current_output$summary_results %>%
    pivot_longer(cols = c("fract_coord_alt", "fract_coord_sq", "fract_miscoordination"), names_to = "type", values_to = "value")
  
  plot <- ggplot(fract_coordination, aes(x = rep(1:(1+t_max), times = 3), y = value, color = type)) +
    geom_line() +
    labs(title = "Fractions of (mis)coordination", x = "Period", y = "Fractions") +
    scale_color_manual(name = "Norm coordination",
                       values = c("green", "blue","red"), # adjust the colors if needed
                       labels = c("Coordination on Alt", "Coordination on SQ", "Miscoordination")) +
    theme_minimal()
  
  # Construct the filename with directory path
  filename <- file.path(names(all_results)[i], "fract_coordination.png")
  
  # Save the plot
  ggsave(filename = filename, plot = plot)
  
  ###################################################################################
  # Plotting frequencies of choices
  ###################################################################################
  
  freq_choices <- current_output$summary_results %>%
    pivot_longer(cols = c("freq_alt", "freq_sq"), names_to = "type", values_to = "value")
  
  plot <- ggplot(freq_choices, aes(x = rep(1:(1+t_max), times = 2), y = value, color = type)) +
    geom_line() +
    labs(title = "Frequencies of SQ and Alt choices", x = "Period", y = "Frequency") +
    scale_color_manual(name = "Norm choice",
                       values = c("green", "blue"), # adjust the colors if needed
                       labels = c("Frequency of Alt", "Frequency of SQ")) +
    theme_minimal()
  
  # Construct the filename with directory path
  filename <- file.path(names(all_results)[i], "freq_choices.png")
  
  # Save the plot
  ggsave(filename = filename, plot = plot)
  
  ###################################################################################
  # Plotting frequencies of choices as fractions
  ###################################################################################
  current_output$summary_results$fract_alt <- current_output$summary_results$freq_alt / N
  current_output$summary_results$fract_sq <- current_output$summary_results$freq_sq / N
  
  fract_choices <- current_output$summary_results %>%
    pivot_longer(cols = c("fract_alt", "fract_sq"), names_to = "type", values_to = "value")
  
  plot <- ggplot(fract_choices, aes(x = rep(1:(1+t_max), times = 2), y = value, color = type)) +
    geom_line() +
    labs(title = "Fractions of SQ and Alt choices", x = "Period", y = "Fraction") +
    scale_color_manual(name = "Norm choice",
                       values = c("green", "blue"), # adjust the colors if needed
                       labels = c("Fraction of Alt", "Fraction of SQ")) +
    theme_minimal()
  
}











###################################################################################
# Plotting expected payoffs
###################################################################################

# Note, in paper, expected payoffs are shown in equations (3) and (4). 
# There are several differences in the agent-based model compared to the analytic
# model in the paper: First, targeted agents do not switch automatically. 
# Instead, they respond with a probability proportional to their threshold values.
# Hence, instead of using "phi" for the expected payoff calculations, we could
# use the fraction of targeted agents who actually switched ("response_fract").
# Further, in equations (3) and (4), the expectations of x_i values is taken
# from the agents who were not targeted and stayed with choosing SQ: (SQ,NT).
# However, given that not all agents who are targeted in this agent-based model
# might switch, there is a new group of agents that was non-existent in the paper,
# namely targeted agents that did not switch: (SQ,T). Hence, I suggest to calculate
# expected x_i values of (SQ,NT)+(SQ,T). Hence, instead of analytically calulating
# these values, I calculate actual expected x_i values observed in the simulation.
# Given that we run a number = n_sim of simulations, we should get a good 
# approximation to expected x_i values drawn from the given beta distributions.
# Further, in equations (3) and (4), the \hat{q}_{A} and \hat{q}_{R} refer to 
# the proportions of populations choosing Alt stabilising in the long-run.
# I approximate those long-term fractions by averaging the fractions of agents
# choosing Alt over the last "long_t <- t_max/10" periods.


# from one simulation run out of n_sim (have to summ all those and average over n_sim)

# I calculate expected x-value of all agents still choosing SQ in last period
# exp_x <- sum(alpha3_target0_phi75_a75_h2$agent_output[which(alpha3_target0_phi75_a75_h2$agent_output[,"choice",101,1]==0),"x_i",101,1])/length(alpha3_target0_phi75_a75_h2$agent_output[which(alpha3_target0_phi75_a75_h2$agent_output[,"choice",101,1]==0),"x_i",101,1])
# 
# 
# alpha3_target0_phi75_a75_h2$agent_output[which(alpha3_target0_phi75_a75_h2$agent_output[,"choice",2,1]==0),"choice",1,1]

# Long-run fractions of agents choosing Alt vs SQ (I calculate average of "long_t" last periods)
# long_t <- t_max / 10
# long_run_fract_alt <- mean(alpha3_target0_phi75_a75_h2$summary_results$fract_alt[(t_max - long_t + 1):t_max])
# long_run_fract_sq <- mean(alpha3_target0_phi75_a75_h2$summary_results$fract_sq[(t_max - long_t + 1):t_max])
# 
# 
# exp_payoff <- response_fract*h + a*(1-response_fract)*(1-long_run_fract_alt) +
#   b*long_run_fract_alt*(1-long_run_fract_alt) + d*long_run_fract_alt*(long_run_fract_alt-response_fract) +
#   (1-long_run_fract_alt)*exp_x


# CODE ABOVE TO CALCULATE EXPECTED PAYOFF STUFF DOESN'T WORK IF ALL AGENTS CHOOSE ALT IN LAST PERIOD



