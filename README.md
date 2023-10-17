# Normsim

## This is an Rproject associated with the paper "When norm change hurts" by  Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.

### This repository contains four relevant files: "Normsim.Rproj", "simulation.R", and "separate_plots.R", and "combined_plots.R". 
### "Normsim.Rproj" exists because this was created as an Rproject. Hence, it is easiest to download the whole project and save it into a local directory so that working directories do not have to be created manually. If the scripts are opened as an Rproject, the working directory will automatically be set to the directory in which the Rproject has been saved.
### The Rscript called "simulation.R" contains code to produce an agent-based simulation. This script has to be executed first. The order of the plot-producing scripts below doesn't matter.
### The script called "separate_plots.R" produces separate plots for each parameter combination. It creates a subdirectory for each parameter combination and stores the plots in those subdirectories.
### The script called "combined_plots.R" produces combined plots, which show the dynamics of Alt choices, miscoordination, average payoffs, and gini coefficients. Those four measures are shown as four subplots in the combined plots. Each combined plot compares amenable and resistant targets, for given parameter combinations.