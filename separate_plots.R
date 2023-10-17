###################################################################################
# Title: Analysis of threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
###################################################################################

# This code produces separate plots for each parameter combination.
# It creates a subdirectory for each parameter combination and stores the plots
# in those subdirectories.

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
    
    # Store result in subdirectory_results list
    subdirectory_results[[param_name]] <- result
  }
  
  ###################################################################################
  
  # Plotting parameters:
  
  windowWidth <- 12
  windowHeight <- 11
  
  cexEquil <- 2
  lwdEquil <- 2
  
  cexNum <- 1.5
  cexWord <- 2
  
  ###################################################################################
  ###################################################################################
  
  # Plots
  
  ###################################################################################
  ###################################################################################
  
  for (param_name in names(subdirectory_results)) {
    current_output <- subdirectory_results[[param_name]]

    ###################################################################################
    # Plotting frequencies of coordination and miscoordination
    ###################################################################################

    # Create the filename for this parameter combination
    filename <- file.path(subdir, "freq_coordination.pdf")

    pdf(width = windowWidth, height = windowHeight, file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))

    # Create the initial line plot with coordination on SQ
    plot(1:current_output$t_max, current_output$summary_results$freq_coord_sq, type = "l",  lwd = 3,
         xlab = "Period", ylab = "Frequency",
         # main = "Frequency of (mis)coordination",
         col = "blue", ylim = c(0, max(current_output$summary_results$freq_coord_sq, na.rm = TRUE)),
         cex.axis = cexNum,cex.lab = cexWord)

    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_sq, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_sq, col = "blue", lty = 2)

    # # Add coordination on Alt
    lines(1:current_output$t_max, current_output$summary_results$freq_coord_alt, col = "darkgreen", lty = 1, lwd = 3)

    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_alt, col = "darkgreen", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_alt, col = "darkgreen", lty = 2)

    # # Add miscoordination
    lines(1:current_output$t_max, current_output$summary_results$miscoordination, col = "red", lty = 1, lwd = 3)

    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_miscoordination, col = "red", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_miscoordination, col = "red", lty = 2)

    # # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt", "Miscoordination", "95% CI Miscoord."), 
    #        col = c("blue","blue", "darkgreen", "darkgreen", "red", "red"), lty = c(1, 2, 1, 2, 1, 2))
    #  
    # # Close the graphics device and save the plot as a PDF file
    dev.off()

    ###################################################################################
    # Plotting choice fractions
    ###################################################################################
    current_output$summary_results$fract_alt <- current_output$summary_results$freq_alt / current_output$N
    current_output$summary_results$fract_sq <- current_output$summary_results$freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_sq <- current_output$summary_results$low_ci_freq_sq / current_output$N
    current_output$summary_results$fract_high_ci_freq_sq <- current_output$summary_results$high_ci_freq_sq / current_output$N
    current_output$summary_results$fract_low_ci_freq_alt <- current_output$summary_results$low_ci_freq_alt / current_output$N
    current_output$summary_results$fract_high_ci_freq_alt <- current_output$summary_results$high_ci_freq_alt / current_output$N

    # Construct the filename with directory path
    filename <- file.path(subdir, "fract_choices.pdf")

    windowWidth <- 12
    windowHeight <- 11

    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))

    # Create the initial line plot with SQ choices
    plot(1:current_output$t_max, current_output$summary_results$fract_sq, type = "l", lwd = 3,
          xlab = "Period", ylab = "Fraction",
          # main = "Fraction of choices",
          col = "blue", ylim = c(0, 1),
          cex.axis = cexNum,cex.lab = cexWord)

    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_sq, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_sq, col = "blue", lty = 2)

    # Add Alt choices
    lines(1:current_output$t_max, current_output$summary_results$fract_alt, col = "darkgreen", lty = 1, lwd = 3)

    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)

    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI", "Alt", "95% CI"), 
    #        col = c("blue", "blue", "darkgreen", "darkgreen"), lty = c(1, 2, 1, 2))

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
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create plot with average payoffs
    plot(1:current_output$t_max, current_output$summary_results$avg_payoff, type = "l", lwd = 3,
         xlab = "Period", ylab = "Average payoff",
         main = "Average payoffs",
         col = "blue", ylim = c(0, current_output$h),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_avg_payoff, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_avg_payoff, col = "blue", lty = 2)
    
    # # If we only show overall avg payoffs:
    # legend("topright", legend = c("Average payoff", "95% CI"), col = c("blue", "blue"), lty = c(1, 2))
    # 
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    ###################################################################################
    # Plotting Gini Coefficients
    ###################################################################################
    
    # Construct the filename with directory path
    filename <- file.path(subdir, "gini_coefficient.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    par(mar = c(6,7,2,2),mgp = c(3.5,1,0))
    
    # Create plot with gini coefficients
    plot(1:current_output$t_max, current_output$summary_results$gini_coefficient, type = "l", lwd = 3,
         xlab = "Period", ylab = "Gini Coefficient",
         # main = "Gini Coefficient",
         col = "blue", ylim = c(0, 1),
         cex.axis = cexNum,cex.lab = cexWord)
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_gini_coefficient, col = "blue", lty = 2)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_gini_coefficient, col = "blue", lty = 2)
    
    # # If we only show overall avg payoffs:
    # legend("topright", legend = c("Gini Coefficient", "95% CI"), col = c("blue", "blue"), lty = c(1, 2))
    # 
    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
  }
}

