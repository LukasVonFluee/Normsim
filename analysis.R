###################################################################################
# Title: Analysis of threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
###################################################################################

# Package dependencies:

###################################################################################

# Parameters specifying payoffs in coordination game that we don't vary:
b <- 0
d <- 1    
g <- 0 

###################################################################################
# Load data with file names corresponding to parameter combinations
###################################################################################

base_dir <- getwd()

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Loop through each subdirectory and load and process the .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Initialize a list to store the results for the current subdirectory
  subdirectory_results <- list()
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Extract just the parameter combination name from the full path
    param_name <- basename(dirname(rdata_file))
    
    # Create additional variables of interest
    result$summary_results$miscoordination <- 
      ((result$N)/2) - 
      result$summary_results$freq_coord_sq - 
      result$summary_results$freq_coord_alt
    
    # Store result in subdirectory_results list
    subdirectory_results[[param_name]] <- result
  }
  
  # Now, you can iterate through the processed results for this subdirectory
  # and create plots or perform any other analysis you need
  
  for (param_name in names(subdirectory_results)) {
    current_output <- subdirectory_results[[param_name]]
    
    ###################################################################################
    # Plotting frequencies of coordination and miscoordination
    ###################################################################################
    
    # Create the filename for this parameter combination
    filename <- file.path(subdir, "freq_coordination.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth, height = windowHeight, file = filename)
    
    # Create the initial line plot with coordination on SQ
    # (You can modify this part to create your plots as needed)
    plot(1:current_output$t_max, current_output$summary_results$freq_coord_sq, type = "l", 
         xlab = "Period", ylab = "Frequency",
         main = "Frequency of (mis)coordination",
         col = "blue", ylim = c(0, max(current_output$summary_results$freq_coord_sq, na.rm = TRUE)))
    
    # Add coordination on Alt
    lines(1:current_output$t_max, current_output$summary_results$freq_coord_alt, col = "green", lty = 1)
    
    # Add miscoordination
    lines(1:current_output$t_max, current_output$summary_results$miscoordination, col = "red", lty = 1)
    
    # Add a legend
    legend("topright", legend = c("SQ", "Alt", "Miscoordination"), col = c("blue", "green", "red"), lty = 1)
    
    # Close the graphics device and save the plot as a PDF file
    dev.off()
    
    ###################################################################################
    # Plotting fractions of coordination and miscoordination as fractions
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
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "fract_coordination.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create the initial line plot with coordination on SQ
    plot(1:current_output$t_max, current_output$summary_results$fract_coord_sq, type = "l", 
         xlab = "Period", ylab = "Fraction",
         main = "Fraction of (mis)coordination",
         col = "blue", ylim = c(0, 1))
    
    # Add coordination on Alt
    lines(1:current_output$t_max, current_output$summary_results$fract_coord_alt, col = "green", lty = 1)
    
    # Add miscoordination
    lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination, col = "red", lty = 1)
    
    # Add a legend
    legend("topright", legend = c("SQ", "Alt", "Miscoordination"), col = c("blue", "green", "red"), lty = 1)
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting frequencies of choices
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "freq_choices.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create the initial line plot with SQ choices
    plot(1:current_output$t_max, current_output$summary_results$freq_sq, type = "l", 
         xlab = "Period", ylab = "Frequency",
         main = "Frequency of choices",
         col = "blue", ylim = c(0, current_output$N))
    
    # Add Alt choices
    lines(1:current_output$t_max, current_output$summary_results$freq_alt, col = "green", lty = 1)
    
    # Add a legend
    legend("topright", legend = c("SQ", "Alt"), col = c("blue", "green"), lty = 1)
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting choice fractions
    ###################################################################################
    current_output$summary_results$fract_alt <- current_output$summary_results$freq_alt / current_output$N
    current_output$summary_results$fract_sq <- current_output$summary_results$freq_sq / current_output$N
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "fract_choices.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create the initial line plot with SQ choices
    plot(1:current_output$t_max, current_output$summary_results$fract_sq, type = "l", 
         xlab = "Period", ylab = "Fraction",
         main = "Fraction of choices",
         col = "blue", ylim = c(0, 1))
    
    # Add Alt choices
    lines(1:current_output$t_max, current_output$summary_results$fract_alt, col = "green", lty = 1)
    
    # Add a legend
    legend("topright", legend = c("SQ", "Alt"), col = c("blue", "green"), lty = 1)
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting expected payoffs
    ###################################################################################
    
    # If we want to add the average expected payoff:
    # current_output$summary_results$expected_payoff <- 
    #   (current_output$summary_results$exp_sq + current_output$summary_results$exp_alt)/2
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "expected_payoff.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create the initial line plot with coordination on SQ
    plot(1:current_output$t_max, current_output$summary_results$exp_sq, type = "l", 
         xlab = "Period", ylab = "Expected payoff",
         main = "Expected payoffs",
         col = "blue", ylim = c(0, current_output$h))
    
    # Add coordination on Alt
    lines(1:current_output$t_max, current_output$summary_results$exp_alt, col = "green", lty = 1)
    
    # If we want to add the average expected payoff, we would add:
    # lines(1:100, current_output$summary_results$expected_payoff, col = "red", lty = 1)
    
    # Add a legend
    legend("topright", legend = c("SQ", "Alt"), col = c("blue", "green"), lty = 1)
    
    # If we want to add the average expected payoff, we can instead do:
    # legend("topright", legend = c("SQ", "Alt", "Average expected payoff"), col = c("blue", "green", "red"), lty = 1)
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting average payoffs
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "average_payoff.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create plot with average payoffs
    plot(1:current_output$t_max, current_output$summary_results$avg_payoff, type = "l", 
         xlab = "Period", ylab = "Average payoff",
         main = "Average payoffs",
         col = "blue", ylim = c(0, current_output$h))
    
    # If we only show overall avg payoffs:
    legend("topright", legend = c("Average payoff"), col = c("blue"), lty = 1)
    
    # If we add average payoff Alt
    # lines(1:current_output$t_max, current_output$summary_results$avg_payoff_alt, col = "green", lty = 1)
    
    # If we add average payoff SQ
    # lines(1:current_output$t_max, current_output$summary_results$avg_payoff_sq, col = "red", lty = 1)
    
    # Add a legend (if we add avg payoffs for Alt and SQ)
    # legend("topright", legend = c("Average payoff", "Average payoff Alt", "Average payoff SQ"), col = c("blue", "green","red"), lty = 1)
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting Gini Coefficients
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "gini_coefficient")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create plot with gini coefficients
    plot(1:current_output$t_max, current_output$summary_results$gini_coefficient, type = "l", 
         xlab = "Period", ylab = "Gini Coefficient",
         main = "Gini Coefficient",
         col = "blue", ylim = c(0, 1))
    
    # If we only show overall avg payoffs:
    legend("topright", legend = c("Gini Coefficient"), col = c("blue"), lty = 1)
    
    # If we add average payoff Alt
    # lines(1:current_output$t_max, current_output$summary_results$avg_payoff_alt, col = "green", lty = 1)
    
    # If we add average payoff SQ
    # lines(1:current_output$t_max, current_output$summary_results$avg_payoff_sq, col = "red", lty = 1)
    
    # Add a legend (if we add avg payoffs for Alt and SQ)
    # legend("topright", legend = c("Average payoff", "Average payoff Alt", "Average payoff SQ"), col = c("blue", "green","red"), lty = 1)
    
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
  }
}
