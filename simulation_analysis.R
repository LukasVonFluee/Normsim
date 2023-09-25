install.packages("cowplot")
library(cowplot)
library(ggplot2)

# Load .RData of a given parameter combination
################################################################################
# Load results from parameter combination: alpha_3_target_1_phi_0.25_a_0.75_h_2
load("alpha_3_target_1_phi_0.25_a_0.75_h_2.RData")
ls()
N <- result$N
t_max <- result$t_max

coordination_alpha_3_target_1_phi_0.25_a_0.75_h_2 <- result$coordination
# Add miscoordination
coordination_alpha_3_target_1_phi_0.25_a_0.75_h_2$miscoordination <- (N/2) - coordination_alpha_3_target_1_phi_0.25_a_0.75_h_2$freq_coord_alt - coordination_alpha_3_target_1_phi_0.25_a_0.75_h_2$freq_coord_sq

################################################################################
# Load results from parameter combination: alpha_3_target_1_phi_0.75_a_0.75_h_2
load("alpha_3_target_1_phi_0.75_a_0.75_h_2.RData")
coordination_alpha_3_target_1_phi_0.75_a_0.75_h_2 <- result$coordination
# Add miscoordination
coordination_alpha_3_target_1_phi_0.75_a_0.75_h_2$miscoordination <- (N/2) - coordination_alpha_3_target_1_phi_0.75_a_0.75_h_2$freq_coord_alt - coordination_alpha_3_target_1_phi_0.75_a_0.75_h_2$freq_coord_sq


################################################################################
# Load results from parameter combination: alpha_3_target_1_phi_0.75_a_0.75_h_2
load("alpha_3_target_0_phi_0.25_a_0.75_h_2.RData")
coordination_alpha_3_target_0_phi_0.25_a_0.75_h_2 <- result$coordination
# Add miscoordination
coordination_alpha_3_target_0_phi_0.25_a_0.75_h_2$miscoordination <- (N/2) - coordination_alpha_3_target_0_phi_0.25_a_0.75_h_2$freq_coord_alt - coordination_alpha_3_target_0_phi_0.25_a_0.75_h_2$freq_coord_sq



################################################################################
# Try some plots:
coordination_alpha_3_target_0_phi_0.25_a_0.75_h_2$identifier <- "alpha=3, target=amenable, phi=0.25, a=0.75, h=2"
coordination_alpha_3_target_1_phi_0.75_a_0.75_h_2$identifier <- "alpha=3, target=resistant, phi=0.75, a=0.75, h=2"

# Convert to long format
long_df1 <- coordination_alpha_3_target_0_phi_0.25_a_0.75_h_2 %>%
  pivot_longer(cols = c(freq_coord_alt, freq_coord_sq, miscoordination), names_to = "type", values_to = "value")

long_df2 <- coordination_alpha_3_target_1_phi_0.75_a_0.75_h_2 %>%
  pivot_longer(cols = c(freq_coord_alt, freq_coord_sq, miscoordination), names_to = "type", values_to = "value")

# Combine the two data frames
combined_long_df <- rbind(long_df1, long_df2)

# Plot
ggplot(combined_long_df, aes(x = rep(1:101, times = 6), y = value, color = type, linetype = identifier)) +
  geom_line() +
  labs(title = "Coordination Data", x = "Time", y = "Value") +
  theme_minimal()
