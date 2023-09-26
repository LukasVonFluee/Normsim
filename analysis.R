library(ggplot2)

# Base directory where subdirectories are located
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
    
    # "result" is the name of the object saved in the .RData file
    all_results[[rdata_file]] <- result
  }
}

# Plots:

# To start, let's compare two specific results from the loaded 'all_results'
param_comb1 <- all_results[["/Users/lukasvonflue/Desktop/Normsim/alpha_3_target_0_phi_0.75_a_0.75_h_2/alpha_3_target_0_phi_0.75_a_0.75_h_2.RData"]] 
param_comb2 <- all_results[["/Users/lukasvonflue/Desktop/Normsim/alpha_3_target_1_phi_0.75_a_0.75_h_2/alpha_3_target_1_phi_0.75_a_0.75_h_2.RData"]] 

# Extract the necessary arrays and compute miscoordination
extract_data <- function(result) {
  summary_results <- result$summary_results
  
  freq_coord_alt <- summary_results$freq_coord_alt
  freq_coord_sq <- summary_results$freq_coord_sq
  freq_alt <- summary_results$freq_alt
  freq_sq <- summary_results$freq_sq
  avg_payoff_alt <- summary_results$avg_payoff_alt
  avg_payoff_sq <- summary_results$avg_payoff_sq
  
  miscoordination <- (N/2) - freq_coord_alt - freq_coord_sq
  
  return(data.frame(freq_coord_alt = freq_coord_alt,
                    freq_coord_sq = freq_coord_sq,
                    freq_alt = freq_alt,
                    freq_sq = freq_sq,
                    avg_payoff_alt = avg_payoff_alt,
                    avg_payoff_sq = avg_payoff_sq,
                    miscoordination = miscoordination))
}

data1 <- extract_data(param_comb1)
data1$param_comb <- "param_comb1"
data2 <- extract_data(param_comb2)
data2$param_comb <- "param_comb2"

# Combine the data for the two parameter combinations
combined_data <- rbind(data1, data2)

# Reshape the data for ggplot
data_long <- tidyr::gather(combined_data, measure, value, -param_comb)

# Add a new time_step variable to the data_long dataframe
data_long$time_step <- rep(1:nrow(data1), times = length(unique(data_long$measure)) * 2)

# Plotting with ggplot2
ggplot(data_long, aes(x = time_step, y = value, color = param_comb, group = interaction(param_comb, measure))) +
  geom_line() +
  facet_wrap(~ measure, scales = "free_y") + 
  labs(y = "Value", x = "Period", title = "Comparison of Two Parameter Combinations") +
  theme_minimal()
