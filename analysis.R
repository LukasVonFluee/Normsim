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

    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_sq, col = "lightblue", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_sq, col = "lightblue", lty = 1)

    # # Add coordination on Alt
    lines(1:current_output$t_max, current_output$summary_results$freq_coord_alt, col = "darkgreen", lty = 1)

    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_alt, col = "lightgreen", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_alt, col = "lightgreen", lty = 1)

    # # Add miscoordination
    lines(1:current_output$t_max, current_output$summary_results$miscoordination, col = "red", lty = 1)

    # # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_miscoordination, col = "pink", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_miscoordination, col = "pink", lty = 1)

    # # Add a legend
    legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt", "Miscoordination", "95% CI Miscoord."), col = c("blue","lightblue", "green", "lightgreen", "red", "pink"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PDF file
    dev.off()
    
    
    # ###################################################################################
    # # Plotting frequencies of coordination on SQ
    # ###################################################################################
    # 
    # # Create the filename for this parameter combination
    # filename <- file.path(subdir, "freq_coordination_sq.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth, height = windowHeight, file = filename)
    # 
    # # Create the initial line plot with coordination on SQ
    # # (You can modify this part to create your plots as needed)
    # plot(1:current_output$t_max, current_output$summary_results$freq_coord_sq, type = "l", 
    #      xlab = "Period", ylab = "Frequency",
    #      main = "Frequency of coordination on SQ",
    #      col = "blue", ylim = c(0, current_output$N/2))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_sq, col = "lightblue", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_sq, col = "lightblue", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("Coordination on SQ", "95% CI"), col = c("blue", "lightblue"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PDF file
    # dev.off()
    # 
    # ###################################################################################
    # # Plotting frequencies of coordination on Alt
    # ###################################################################################
    # 
    # # Create the filename for this parameter combination
    # filename <- file.path(subdir, "freq_coordination_alt.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth, height = windowHeight, file = filename)
    # 
    # # Create the initial line plot with coordination on SQ
    # # (You can modify this part to create your plots as needed)
    # plot(1:current_output$t_max, current_output$summary_results$freq_coord_alt, type = "l", 
    #      xlab = "Period", ylab = "Frequency",
    #      main = "Frequency of coordination on Alt",
    #      col = "darkgreen", ylim = c(0, current_output$N/2))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_coord_alt, col = "lightgreen", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_coord_alt, col = "lightgreen", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("Coordination on Alt", "95% CI"), col = c("darkgreen", "lightgreen"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PDF file
    # dev.off()
    # 
    # ###################################################################################
    # # Plotting frequencies miscoordination
    # ###################################################################################
    # 
    # # Create the filename for this parameter combination
    # filename <- file.path(subdir, "freq_miscoordination.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth, height = windowHeight, file = filename)
    # 
    # # Create the initial line plot with coordination on SQ
    # # (You can modify this part to create your plots as needed)
    # plot(1:current_output$t_max, current_output$summary_results$miscoordination, type = "l", 
    #      xlab = "Period", ylab = "Frequency",
    #      main = "Frequency of miscoordination",
    #      col = "red", ylim = c(0, current_output$N/2))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_miscoordination, col = "pink", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_miscoordination, col = "pink", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("Miscoordination", "95% CI"), col = c("red", "pink"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PDF file
    # dev.off()
    # 

    # ###################################################################################
    # # Plotting fractions of coordination and miscoordination as fractions
    # ###################################################################################
    # current_output$summary_results$fract_coord_sq <-
    #   current_output$summary_results$freq_coord_sq / (current_output$summary_results$freq_coord_alt
    #                                                   + current_output$summary_results$freq_coord_sq
    #                                                   + current_output$summary_results$miscoordination)
    # 
    # current_output$summary_results$low_ci_fract_coord_sq <- current_output$summary_results$low_ci_freq_coord_sq / (current_output$N/2)
    # current_output$summary_results$high_ci_fract_coord_sq <- current_output$summary_results$high_ci_freq_coord_sq / (current_output$N/2)
    # 
    # current_output$summary_results$fract_coord_alt <-
    #   current_output$summary_results$freq_coord_alt / (current_output$summary_results$freq_coord_alt
    #                                                    + current_output$summary_results$freq_coord_sq
    #                                                    + current_output$summary_results$miscoordination)
    # 
    # current_output$summary_results$low_ci_fract_coord_alt <- current_output$summary_results$low_ci_freq_coord_alt / (current_output$N/2)
    # current_output$summary_results$high_ci_fract_coord_alt <- current_output$summary_results$high_ci_freq_coord_alt / (current_output$N/2)
    # 
    # current_output$summary_results$fract_miscoordination <-
    #   current_output$summary_results$miscoordination / (current_output$summary_results$freq_coord_alt
    #                                                     + current_output$summary_results$freq_coord_sq
    #                                                     + current_output$summary_results$miscoordination)
    # 
    # current_output$summary_results$low_ci_fract_miscoordination <- current_output$summary_results$low_ci_miscoordination / (current_output$N/2)
    # current_output$summary_results$high_ci_fract_miscoordination <- current_output$summary_results$high_ci_miscoordination / (current_output$N/2)
    # 
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "fract_coordination.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with coordination on SQ
    # plot(1:current_output$t_max, current_output$summary_results$fract_coord_sq, type = "l",
    #      xlab = "Period", ylab = "Fraction",
    #      main = "Fraction of (mis)coordination",
    #      col = "blue", ylim = c(0, 1))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_fract_coord_sq, col = "lightblue", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_fract_coord_sq, col = "lightblue", lty = 1)
    # 
    # # Add coordination on Alt
    # lines(1:current_output$t_max, current_output$summary_results$fract_coord_alt, col = "darkgreen", lty = 1)
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_fract_coord_alt, col = "lightgreen", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_fract_coord_alt, col = "lightgreen", lty = 1)
    # 
    # # Add miscoordination
    # lines(1:current_output$t_max, current_output$summary_results$fract_miscoordination, col = "red", lty = 1)
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_fract_miscoordination, col = "pink", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_fract_miscoordination, col = "pink", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt", "Miscoordination", "95% CI Miscoord."), col = c("blue","lightblue", "green", "lightgreen", "red", "pink"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()

    # ###################################################################################
    # # Plotting frequencies of choices
    # ###################################################################################
    # 
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "freq_choices.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with SQ choices
    # plot(1:current_output$t_max, current_output$summary_results$freq_sq, type = "l", 
    #      xlab = "Period", ylab = "Frequency",
    #      main = "Frequency of choices",
    #      col = "blue", ylim = c(0, current_output$N))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_sq, col = "lightblue", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_sq, col = "lightblue", lty = 1)
    # 
    # # Add Alt choices
    # lines(1:current_output$t_max, current_output$summary_results$freq_alt, col = "darkgreen", lty = 1)
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_alt, col = "lightgreen", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_alt, col = "lightgreen", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt"), col = c("blue", "lightblue", "darkgreen", "lightgreen"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()
    
    # 
    # ###################################################################################
    # # Plotting frequencies of SQ choices
    # ###################################################################################
    # 
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "freq_choices_sq.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with SQ choices
    # plot(1:current_output$t_max, current_output$summary_results$freq_sq, type = "l", 
    #      xlab = "Period", ylab = "Frequency",
    #      main = "Frequency of choices",
    #      col = "blue", ylim = c(0, current_output$N))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_sq, col = "lightblue", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_sq, col = "lightblue", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ"), col = c("blue", "lightblue"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()
    # 
    # ###################################################################################
    # # Plotting frequencies of Alt choices
    # ###################################################################################
    # 
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "freq_choices_alt.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with SQ choices
    # plot(1:current_output$t_max, current_output$summary_results$freq_alt, type = "l", 
    #      xlab = "Period", ylab = "Frequency",
    #      main = "Frequency of choices",
    #      col = "darkgreen", ylim = c(0, current_output$N))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$low_ci_freq_alt, col = "lightgreen", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$high_ci_freq_alt, col = "lightgreen", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("Alt", "95% CI Alt"), col = c("darkgreen", "lightgreen"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()
    # 
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

    # Create the initial line plot with SQ choices
    plot(1:current_output$t_max, current_output$summary_results$fract_sq, type = "l",
          xlab = "Period", ylab = "Fraction",
          main = "Fraction of choices",
          col = "blue", ylim = c(0, 1))

    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_sq, col = "lightblue", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_sq, col = "lightblue", lty = 1)

    # Add Alt choices
    lines(1:current_output$t_max, current_output$summary_results$fract_alt, col = "darkgreen", lty = 1)

    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_alt, col = "lightgreen", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_alt, col = "lightgreen", lty = 1)

    # Add a legend
    legend("topright", legend = c("SQ", "95% CI SQ", "Alt", "95% CI Alt"), col = c("blue", "lightblue", "darkgreen", "lightgreen"), lty = 1)

    # Close the graphics device and save the plot as a PNG file
    dev.off()
    
    # ###################################################################################
    # # Plotting SQ choice fractions
    # ###################################################################################
    # 
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "fract_choices_sq.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with SQ choices
    # plot(1:current_output$t_max, current_output$summary_results$fract_sq, type = "l",
    #      xlab = "Period", ylab = "Fraction",
    #      main = "Fraction of SQ choices",
    #      col = "blue", ylim = c(0, 1))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_sq, col = "lightblue", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_sq, col = "lightblue", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("SQ", "95% CI SQ"), col = c("blue", "lightblue"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()
    # 
    # ###################################################################################
    # # Plotting Alt choice fractions
    # ###################################################################################
    # 
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "fract_choices_alt.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with SQ choices
    # plot(1:current_output$t_max, current_output$summary_results$fract_alt, type = "l",
    #      xlab = "Period", ylab = "Fraction",
    #      main = "Fraction of Alt choices",
    #      col = "darkgreen", ylim = c(0, 1))
    # 
    # # Add confidence intervals
    # lines(1:current_output$t_max, current_output$summary_results$fract_low_ci_freq_alt, col = "lightgreen", lty = 1)
    # lines(1:current_output$t_max, current_output$summary_results$fract_high_ci_freq_alt, col = "lightgreen", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("Alt", "95% CI Alt"), col = c("darkgreen", "lightgreen"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()
    
    ###################################################################################
    # Plotting expected payoffs
    ###################################################################################
    
    # If we want to add the average expected payoff:
    # current_output$summary_results$expected_payoff <- 
    #   (current_output$summary_results$exp_sq + current_output$summary_results$exp_alt)/2
    
    # # Construct the filename with directory path
    # filename <- file.path(subdir, "expected_payoff.pdf")
    # 
    # windowWidth <- 12
    # windowHeight <- 11
    # 
    # pdf(width = windowWidth,height = windowHeight,file = filename)
    # 
    # # Create the initial line plot with coordination on SQ
    # plot(1:current_output$t_max, current_output$summary_results$exp_sq, type = "l", 
    #      xlab = "Period", ylab = "Expected payoff",
    #      main = "Expected payoffs",
    #      col = "blue", ylim = c(0, current_output$h))
    # 
    # # Add coordination on Alt
    # lines(1:current_output$t_max, current_output$summary_results$exp_alt, col = "green", lty = 1)
    # 
    # # If we want to add the average expected payoff, we would add:
    # # lines(1:100, current_output$summary_results$expected_payoff, col = "red", lty = 1)
    # 
    # # Add a legend
    # legend("topright", legend = c("SQ", "Alt"), col = c("blue", "green"), lty = 1)
    # 
    # # If we want to add the average expected payoff, we can instead do:
    # # legend("topright", legend = c("SQ", "Alt", "Average expected payoff"), col = c("blue", "green", "red"), lty = 1)
    # 
    # # Close the graphics device and save the plot as a PNG file
    # dev.off()
    # 
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
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_avg_payoff, col = "lightblue", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_avg_payoff, col = "lightblue", lty = 1)
    
    # If we only show overall avg payoffs:
    legend("topright", legend = c("Average payoff", "95% CI"), col = c("blue", "lightblue"), lty = 1)
    
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
    filename <- file.path(subdir, "gini_coefficient.pdf")
    
    windowWidth <- 12
    windowHeight <- 11
    
    pdf(width = windowWidth,height = windowHeight,file = filename)
    
    # Create plot with gini coefficients
    plot(1:current_output$t_max, current_output$summary_results$gini_coefficient, type = "l", 
         xlab = "Period", ylab = "Gini Coefficient",
         main = "Gini Coefficient",
         col = "blue", ylim = c(0, 1))
    
    # Add confidence intervals
    lines(1:current_output$t_max, current_output$summary_results$low_ci_gini_coefficient, col = "lightblue", lty = 1)
    lines(1:current_output$t_max, current_output$summary_results$high_ci_gini_coefficient, col = "lightblue", lty = 1)
    
    # If we only show overall avg payoffs:
    legend("topright", legend = c("Gini Coefficient", "95% CI"), col = c("blue", "lightblue"), lty = 1)
    
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


###################################################################################
###################################################################################
###################################################################################
# Comparison plots
###################################################################################
###################################################################################
###################################################################################

# setwd("~/Desktop/tipsim")


###################################################################################
###################################################################################

# alpha3target0phi075a075h2 vs alpha3target1phi075a075h2

###################################################################################
###################################################################################

load("alpha3target0phi075a075h2/alpha3target0phi075a075h2.RData")

alpha3target0phi075a075h2 <- result

load("alpha3target1phi075a075h2/alpha3target1phi075a075h2.RData")

alpha3target1phi075a075h2 <- result


###################################################################################

# Fraction of choices

###################################################################################

# Calculate fraction of choices from frequencies

# alpha3target0phi075a075h2:

alpha3target0phi075a075h2$summary_results$fract_alt <- alpha3target0phi075a075h2$summary_results$freq_alt / alpha3target0phi075a075h2$N
alpha3target0phi075a075h2$summary_results$fract_sq <- alpha3target0phi075a075h2$summary_results$freq_sq / alpha3target0phi075a075h2$N
alpha3target0phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha3target0phi075a075h2$summary_results$low_ci_freq_sq / alpha3target0phi075a075h2$N
alpha3target0phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha3target0phi075a075h2$summary_results$high_ci_freq_sq / alpha3target0phi075a075h2$N
alpha3target0phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha3target0phi075a075h2$summary_results$low_ci_freq_alt / alpha3target0phi075a075h2$N
alpha3target0phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha3target0phi075a075h2$summary_results$high_ci_freq_alt / alpha3target0phi075a075h2$N

# alpha3target1phi075a075h2:

alpha3target1phi075a075h2$summary_results$fract_alt <- alpha3target1phi075a075h2$summary_results$freq_alt / alpha3target1phi075a075h2$N
alpha3target1phi075a075h2$summary_results$fract_sq <- alpha3target1phi075a075h2$summary_results$freq_sq / alpha3target1phi075a075h2$N
alpha3target1phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha3target1phi075a075h2$summary_results$low_ci_freq_sq / alpha3target1phi075a075h2$N
alpha3target1phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha3target1phi075a075h2$summary_results$high_ci_freq_sq / alpha3target1phi075a075h2$N
alpha3target1phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha3target1phi075a075h2$summary_results$low_ci_freq_alt / alpha3target1phi075a075h2$N
alpha3target1phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha3target1phi075a075h2$summary_results$high_ci_freq_alt / alpha3target1phi075a075h2$N

# Create the filename for this combined plot
combined_filename <- "choices_alpha3phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$fract_alt, type = "l",
     xlab = "Period", ylab = "Fraction",
     main = "Fraction of choices",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightblue", lty = 1)
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)

# Add dashed lines for coordination on Alt (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$fract_alt, col = "darkgreen", lty = 1)

# Add confidence intervals for Alt (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightgreen", lty = 1)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Average payoffs

###################################################################################

# Create the filename for this combined plot
combined_filename <- "payoffs_alpha3phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$avg_payoff, type = "l",
     xlab = "Period", ylab = "Payoff",
     main = "Average payoffs",
     col = "blue", ylim = c(0, alpha3target0phi075a075h2$h))

# Add confidence intervals
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightblue", lty = 1)
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$avg_payoff, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightgreen", lty = 1)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Miscoordination

###################################################################################

# Create the filename for this combined plot
combined_filename <- "miscoord_alpha3phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$miscoordination, type = "l",
     xlab = "Period", ylab = "Frequency",
     main = "Frequency of Miscoordination",
     col = "blue", ylim = c(0, alpha3target0phi075a075h2$N/2))

# Add confidence intervals
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$low_ci_miscoordination, col = "lightblue", lty = 1)
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$high_ci_miscoordination, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$miscoordination, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$low_ci_miscoordination, col = "lightgreen", lty = 1)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$high_ci_miscoordination, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Gini Coefficient

###################################################################################

# Create the filename for this combined plot
combined_filename <- "gini_alpha3phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$gini_coefficient, type = "l",
     xlab = "Period", ylab = "Gini Coefficient",
     main = "Gini Coefficient",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightblue", lty = 1)
lines(1:alpha3target0phi075a075h2$t_max, alpha3target0phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$gini_coefficient, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightgreen", lty = 1)
lines(1:alpha3target1phi075a075h2$t_max, alpha3target1phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()





###################################################################################
###################################################################################
###################################################################################

# alpha275target0phi075a075h2 vs alpha275target1phi075a075h2

###################################################################################
###################################################################################
###################################################################################

load("alpha275target0phi075a075h2/alpha275target0phi075a075h2.RData")

alpha275target0phi075a075h2 <- result

load("alpha275target1phi075a075h2/alpha275target1phi075a075h2.RData")

alpha275target1phi075a075h2 <- result


###################################################################################

# Fraction of choices

###################################################################################

# Calculate fraction of choices from frequencies

# alpha3target0phi075a075h2:

alpha275target0phi075a075h2$summary_results$fract_alt <- alpha275target0phi075a075h2$summary_results$freq_alt / alpha275target0phi075a075h2$N
alpha275target0phi075a075h2$summary_results$fract_sq <- alpha275target0phi075a075h2$summary_results$freq_sq / alpha275target0phi075a075h2$N
alpha275target0phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha275target0phi075a075h2$summary_results$low_ci_freq_sq / alpha275target0phi075a075h2$N
alpha275target0phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha275target0phi075a075h2$summary_results$high_ci_freq_sq / alpha275target0phi075a075h2$N
alpha275target0phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha275target0phi075a075h2$summary_results$low_ci_freq_alt / alpha275target0phi075a075h2$N
alpha275target0phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha275target0phi075a075h2$summary_results$high_ci_freq_alt / alpha275target0phi075a075h2$N

# alpha3target1phi075a075h2:

alpha275target1phi075a075h2$summary_results$fract_alt <- alpha275target1phi075a075h2$summary_results$freq_alt / alpha275target1phi075a075h2$N
alpha275target1phi075a075h2$summary_results$fract_sq <- alpha275target1phi075a075h2$summary_results$freq_sq / alpha275target1phi075a075h2$N
alpha275target1phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha275target1phi075a075h2$summary_results$low_ci_freq_sq / alpha275target1phi075a075h2$N
alpha275target1phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha275target1phi075a075h2$summary_results$high_ci_freq_sq / alpha275target1phi075a075h2$N
alpha275target1phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha275target1phi075a075h2$summary_results$low_ci_freq_alt / alpha275target1phi075a075h2$N
alpha275target1phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha275target1phi075a075h2$summary_results$high_ci_freq_alt / alpha275target1phi075a075h2$N


# Create the filename for this combined plot
combined_filename <- "choices_alpha275phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$fract_alt, type = "l",
     xlab = "Period", ylab = "Fraction",
     main = "Fraction of choices",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightblue", lty = 1)
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)

# Add dashed lines for coordination on Alt (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$fract_alt, col = "darkgreen", lty = 1)

# Add confidence intervals for Alt (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightgreen", lty = 1)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()


###################################################################################

# Average payoffs

###################################################################################

# Create the filename for this combined plot
combined_filename <- "payoffs_alpha275phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$avg_payoff, type = "l",
     xlab = "Period", ylab = "Payoff",
     main = "Average payoffs",
     col = "blue", ylim = c(0, alpha275target0phi075a075h2$h))

# Add confidence intervals
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightblue", lty = 1)
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$avg_payoff, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightgreen", lty = 1)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Miscoordination

###################################################################################

# Create the filename for this combined plot
combined_filename <- "miscoord_alpha275phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$miscoordination, type = "l",
     xlab = "Period", ylab = "Frequency",
     main = "Frequency of Miscoordination",
     col = "blue", ylim = c(0, alpha275target0phi075a075h2$N/2))

# Add confidence intervals
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$low_ci_miscoordination, col = "lightblue", lty = 1)
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$high_ci_miscoordination, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$miscoordination, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$low_ci_miscoordination, col = "lightgreen", lty = 1)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$high_ci_miscoordination, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Gini Coefficient

###################################################################################

# Create the filename for this combined plot
combined_filename <- "gini_alpha275phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$gini_coefficient, type = "l",
     xlab = "Period", ylab = "Gini Coefficient",
     main = "Gini Coefficient",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightblue", lty = 1)
lines(1:alpha275target0phi075a075h2$t_max, alpha275target0phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$gini_coefficient, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightgreen", lty = 1)
lines(1:alpha275target1phi075a075h2$t_max, alpha275target1phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################
###################################################################################
###################################################################################

# alpha25target0phi075a075h2 vs alpha25target1phi075a075h2

###################################################################################
###################################################################################
###################################################################################

load("alpha25target0phi075a075h2/alpha25target0phi075a075h2.RData")

alpha25target0phi075a075h2 <- result

load("alpha25target1phi075a075h2/alpha25target1phi075a075h2.RData")

alpha25target1phi075a075h2 <- result

###################################################################################

# Fraction of choices

###################################################################################

# Calculate fraction of choices from frequencies

# alpha3target0phi075a075h2:

alpha25target0phi075a075h2$summary_results$fract_alt <- alpha25target0phi075a075h2$summary_results$freq_alt / alpha25target0phi075a075h2$N
alpha25target0phi075a075h2$summary_results$fract_sq <- alpha25target0phi075a075h2$summary_results$freq_sq / alpha25target0phi075a075h2$N
alpha25target0phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha25target0phi075a075h2$summary_results$low_ci_freq_sq / alpha25target0phi075a075h2$N
alpha25target0phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha25target0phi075a075h2$summary_results$high_ci_freq_sq / alpha25target0phi075a075h2$N
alpha25target0phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha25target0phi075a075h2$summary_results$low_ci_freq_alt / alpha25target0phi075a075h2$N
alpha25target0phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha25target0phi075a075h2$summary_results$high_ci_freq_alt / alpha25target0phi075a075h2$N

# alpha3target1phi075a075h2:

alpha25target1phi075a075h2$summary_results$fract_alt <- alpha25target1phi075a075h2$summary_results$freq_alt / alpha25target1phi075a075h2$N
alpha25target1phi075a075h2$summary_results$fract_sq <- alpha25target1phi075a075h2$summary_results$freq_sq / alpha25target1phi075a075h2$N
alpha25target1phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha25target1phi075a075h2$summary_results$low_ci_freq_sq / alpha25target1phi075a075h2$N
alpha25target1phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha25target1phi075a075h2$summary_results$high_ci_freq_sq / alpha25target1phi075a075h2$N
alpha25target1phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha25target1phi075a075h2$summary_results$low_ci_freq_alt / alpha25target1phi075a075h2$N
alpha25target1phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha25target1phi075a075h2$summary_results$high_ci_freq_alt / alpha25target1phi075a075h2$N

# Create the filename for this combined plot
combined_filename <- "choices_alpha25phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$fract_alt, type = "l",
     xlab = "Period", ylab = "Fraction",
     main = "Fraction of choices",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightblue", lty = 1)
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)

# Add dashed lines for coordination on Alt (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$fract_alt, col = "darkgreen", lty = 1)

# Add confidence intervals for Alt (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightgreen", lty = 1)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("right", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()

###################################################################################

# Average payoffs

###################################################################################

# Create the filename for this combined plot
combined_filename <- "payoffs_alpha25phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$avg_payoff, type = "l",
     xlab = "Period", ylab = "Payoff",
     main = "Average payoffs",
     col = "blue", ylim = c(0, alpha25target0phi075a075h2$h))

# Add confidence intervals
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightblue", lty = 1)
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$avg_payoff, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightgreen", lty = 1)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Miscoordination

###################################################################################

# Create the filename for this combined plot
combined_filename <- "miscoord_alpha25phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$miscoordination, type = "l",
     xlab = "Period", ylab = "Frequency",
     main = "Frequency of Miscoordination",
     col = "blue", ylim = c(0, alpha25target0phi075a075h2$N/2))

# Add confidence intervals
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$low_ci_miscoordination, col = "lightblue", lty = 1)
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$high_ci_miscoordination, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$miscoordination, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$low_ci_miscoordination, col = "lightgreen", lty = 1)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$high_ci_miscoordination, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Gini Coefficient

###################################################################################

# Create the filename for this combined plot
combined_filename <- "gini_alpha25phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$gini_coefficient, type = "l",
     xlab = "Period", ylab = "Gini coefficient",
     main = "Gini Coefficient",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightblue", lty = 1)
lines(1:alpha25target0phi075a075h2$t_max, alpha25target0phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$gini_coefficient, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightgreen", lty = 1)
lines(1:alpha25target1phi075a075h2$t_max, alpha25target1phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()




###################################################################################
###################################################################################
###################################################################################

# alpha8target0phi075a075h2 vs alpha8target1phi075a075h2

###################################################################################
###################################################################################
###################################################################################

load("alpha8target0phi075a075h2/alpha8target0phi075a075h2.RData")

alpha8target0phi075a075h2 <- result

load("alpha8target1phi075a075h2/alpha8target1phi075a075h2.RData")

alpha8target1phi075a075h2 <- result


###################################################################################

# Fraction of choices

###################################################################################

# Calculate fraction of choices from frequencies

# alpha3target0phi075a075h2:

alpha8target0phi075a075h2$summary_results$fract_alt <- alpha8target0phi075a075h2$summary_results$freq_alt / alpha8target0phi075a075h2$N
alpha8target0phi075a075h2$summary_results$fract_sq <- alpha8target0phi075a075h2$summary_results$freq_sq / alpha8target0phi075a075h2$N
alpha8target0phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha8target0phi075a075h2$summary_results$low_ci_freq_sq / alpha8target0phi075a075h2$N
alpha8target0phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha8target0phi075a075h2$summary_results$high_ci_freq_sq / alpha8target0phi075a075h2$N
alpha8target0phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha8target0phi075a075h2$summary_results$low_ci_freq_alt / alpha8target0phi075a075h2$N
alpha8target0phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha8target0phi075a075h2$summary_results$high_ci_freq_alt / alpha8target0phi075a075h2$N

# alpha3target1phi075a075h2:

alpha8target1phi075a075h2$summary_results$fract_alt <- alpha8target1phi075a075h2$summary_results$freq_alt / alpha8target1phi075a075h2$N
alpha8target1phi075a075h2$summary_results$fract_sq <- alpha8target1phi075a075h2$summary_results$freq_sq / alpha8target1phi075a075h2$N
alpha8target1phi075a075h2$summary_results$fract_low_ci_freq_sq <- alpha8target1phi075a075h2$summary_results$low_ci_freq_sq / alpha8target1phi075a075h2$N
alpha8target1phi075a075h2$summary_results$fract_high_ci_freq_sq <- alpha8target1phi075a075h2$summary_results$high_ci_freq_sq / alpha8target1phi075a075h2$N
alpha8target1phi075a075h2$summary_results$fract_low_ci_freq_alt <- alpha8target1phi075a075h2$summary_results$low_ci_freq_alt / alpha8target1phi075a075h2$N
alpha8target1phi075a075h2$summary_results$fract_high_ci_freq_alt <- alpha8target1phi075a075h2$summary_results$high_ci_freq_alt / alpha8target1phi075a075h2$N

# Create the filename for this combined plot
combined_filename <- "choices_alpha8phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$fract_alt, type = "l",
     xlab = "Period", ylab = "Fraction",
     main = "Fraction of choices",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightblue", lty = 1)
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)

# Add dashed lines for coordination on Alt (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$fract_alt, col = "darkgreen", lty = 1)

# Add confidence intervals for Alt (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$fract_low_ci_freq_alt, col = "lightgreen", lty = 1)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$fract_high_ci_freq_alt, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()

###################################################################################

# Average payoffs

###################################################################################

# Create the filename for this combined plot
combined_filename <- "payoffs_alpha8phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$avg_payoff, type = "l",
     xlab = "Period", ylab = "Payoff",
     main = "Average payoffs",
     col = "blue", ylim = c(0, alpha8target0phi075a075h2$h))

# Add confidence intervals
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightblue", lty = 1)
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$avg_payoff, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$low_ci_avg_payoff, col = "lightgreen", lty = 1)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$high_ci_avg_payoff, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Miscoordination

###################################################################################

# Create the filename for this combined plot
combined_filename <- "miscoord_alpha8phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$miscoordination, type = "l",
     xlab = "Period", ylab = "Frequency",
     main = "Frequency of Miscoordination",
     col = "blue", ylim = c(0, alpha8target0phi075a075h2$N/2))

# Add confidence intervals
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$low_ci_miscoordination, col = "lightblue", lty = 1)
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$high_ci_miscoordination, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$miscoordination, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$low_ci_miscoordination, col = "lightgreen", lty = 1)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$high_ci_miscoordination, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()



###################################################################################

# Gini Coefficient

###################################################################################

# Create the filename for this combined plot
combined_filename <- "gini_alpha8phi075a075h2.pdf"

windowWidth <- 12
windowHeight <- 11

pdf(width = windowWidth, height = windowHeight, file = combined_filename)


# Create the initial line plot with SQ choices
plot(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$gini_coefficient, type = "l",
     xlab = "Period", ylab = "Gini coefficient",
     main = "Gini Coefficient",
     col = "blue", ylim = c(0, 1))

# Add confidence intervals
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightblue", lty = 1)
lines(1:alpha8target0phi075a075h2$t_max, alpha8target0phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightblue", lty = 1)

# Second parameter combination with dashed lines (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$gini_coefficient, col = "darkgreen", lty = 1)

# Add confidence intervals (Resistant)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$low_ci_gini_coefficient, col = "lightgreen", lty = 1)
lines(1:alpha8target1phi075a075h2$t_max, alpha8target1phi075a075h2$summary_results$high_ci_gini_coefficient, col = "lightgreen", lty = 1)

# Add a legend with labels
legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
       col = c("blue", "lightblue", "darkgreen", "lightgreen"),
       lty = c(1, 1, 1, 1))

# Close the graphics device and save the combined plot as a PDF file
dev.off()

