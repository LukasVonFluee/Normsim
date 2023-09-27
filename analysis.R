###################################################################################
# Title: Analysis of threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
###################################################################################

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

alpha3_target0_phi75_a75_h2 <- all_results[["alpha_3_target_0_phi_0.75_a_0.75_h_2"]]

###################################################################################
# Create additional variables of interest
###################################################################################

# Fraction of targeted agents who actually switched:
response_fract_alpha3_target0_phi75_a75_h2 <- sum(alpha3_target0_phi75_a75_h2$agent_output[,"respond",1,]) / (N*phi) / n_sim

# Frequency of miscoordination
alpha3_target0_phi75_a75_h2$summary_results$miscoordination <- 
  (N/2) - 
  alpha3_target0_phi75_a75_h2$summary_results$freq_coord_sq - 
  alpha3_target0_phi75_a75_h2$summary_results$freq_coord_alt

# Long-run fractions of agents choosing Alt vs SQ (I calculate average of "long_t" last periods)
long_t <- t_max / 10
long_run_fract_alt <- mean(alpha3_target0_phi75_a75_h2$summary_results$freq_alt[(t_max - long_t + 1):t_max])
long_run_fract_sq <- mean(alpha3_target0_phi75_a75_h2$summary_results$freq_sq[(t_max - long_t + 1):t_max])


# Expected payoff:


###################################################################################
###################################################################################
# Plots
###################################################################################
###################################################################################

###################################################################################
# Plotting frequencies of coordination and miscoordination
###################################################################################
freq_coordination <- alpha3_target0_phi75_a75_h2$summary_results %>%
  pivot_longer(cols = c(freq_coord_alt, freq_coord_sq, miscoordination), names_to = "type", values_to = "value")

ggplot(freq_coordination, aes(x = rep(1:101, times = 3), y = value, color = type)) +
  geom_line() +
  labs(title = "Frequency of (mis)coordination", x = "Period", y = "Frequency") +
  theme_minimal()

###################################################################################
# Plotting frequencies of coordination and miscoordination as fractions
###################################################################################
alpha3_target0_phi75_a75_h2$summary_results$fract_coord_alt <- 
  alpha3_target0_phi75_a75_h2$summary_results$freq_coord_alt / (alpha3_target0_phi75_a75_h2$summary_results$freq_coord_alt 
                                                                + alpha3_target0_phi75_a75_h2$summary_results$freq_coord_sq
                                                                +alpha3_target0_phi75_a75_h2$summary_results$miscoordination)

alpha3_target0_phi75_a75_h2$summary_results$fract_coord_sq <- 
  alpha3_target0_phi75_a75_h2$summary_results$freq_coord_sq / (alpha3_target0_phi75_a75_h2$summary_results$freq_coord_alt 
                                                                 + alpha3_target0_phi75_a75_h2$summary_results$freq_coord_sq
                                                                 +alpha3_target0_phi75_a75_h2$summary_results$miscoordination)

alpha3_target0_phi75_a75_h2$summary_results$fract_miscoordination <- 
  alpha3_target0_phi75_a75_h2$summary_results$miscoordination / (alpha3_target0_phi75_a75_h2$summary_results$freq_coord_alt 
                                                                 + alpha3_target0_phi75_a75_h2$summary_results$freq_coord_sq
                                                                 +alpha3_target0_phi75_a75_h2$summary_results$miscoordination)

fract_coordination <- alpha3_target0_phi75_a75_h2$summary_results %>%
  pivot_longer(cols = c(fract_coord_alt, fract_coord_sq, fract_miscoordination), names_to = "type", values_to = "value")

ggplot(fract_coordination, aes(x = rep(1:101, times = 3), y = value, color = type)) +
  geom_line() +
  labs(title = "Fractions", x = "Period", y = "Fractions (mis)coordination") +
  theme_minimal()

###################################################################################
# Plotting frequencies of choices
###################################################################################
freq_choices <- alpha3_target0_phi75_a75_h2$summary_results %>%
  pivot_longer(cols = c(freq_alt, freq_sq), names_to = "type", values_to = "value")

ggplot(freq_choices, aes(x = rep(1:101, times = 2), y = value, color = type)) +
  geom_line() +
  labs(title = "Frequencies of SQ and Alt choices", x = "Period", y = "Frequency") +
  theme_minimal()

###################################################################################
# Plotting frequencies of choices as fractions
###################################################################################
alpha3_target0_phi75_a75_h2$summary_results$fract_alt <- alpha3_target0_phi75_a75_h2$summary_results$freq_alt / N
alpha3_target0_phi75_a75_h2$summary_results$fract_sq <- alpha3_target0_phi75_a75_h2$summary_results$freq_sq / N

fract_choices <- alpha3_target0_phi75_a75_h2$summary_results %>%
  pivot_longer(cols = c(fract_alt, fract_sq), names_to = "type", values_to = "value")

ggplot(fract_choices, aes(x = rep(1:101, times = 2), y = value, color = type)) +
  geom_line() +
  labs(title = "Fractions of SQ and Alt choices", x = "Period", y = "Fractions") +
  theme_minimal()


###################################################################################
# Plotting expected payoffs
###################################################################################






