# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Data Import and Column Renaming
data_raw <- read.csv('../data/ethnocentrism-proj experiment-new-strategies-table.csv', skip=6)

# Rename columns for simplicity
colnames(data_raw) <- colnames(data_raw) %>%
  gsub("count\\.turtles\\.with\\.\\.type\\.agent\\.\\.\\.\\.|\\.\\.and\\.tag\\.\\.\\.", "", .) %>%
  gsub("Altruist", "altruist", .) %>%
  gsub("Ethnocentrist", "ethnocentrist", .) %>%
  gsub("Cosmopolitan", "cosmopolitan", .) %>%
  gsub("Egoist", "egoist", .) %>%
  gsub("Undefined", "undefined", .) %>%
  gsub("majority", "majority", .) %>%
  gsub("minority", "minority", .) %>%
  gsub("\\.", "", .)

# Step 2: Facet Plot (Dominant Agent Type by Gamma and W)

# all
dominance_data <- data_raw %>%
  group_by(gamma, W) %>%
  summarise(
    altruist = mean(altruistmajority + altruistminority),
    ethnocentrist = mean(ethnocentristmajority + ethnocentristminority),
    cosmopolitan = mean(cosmopolitanmajority + cosmopolitanminority),
    egoist = mean(egoistmajority + egoistminority),
    undefined = mean(undefinedmajority + undefinedminority)
  ) %>%
  rowwise() %>%
  mutate(
    max_value = max(altruist, ethnocentrist, cosmopolitan, egoist, undefined),
    dominant_type = case_when(
      max_value == altruist ~ "Altruist",
      max_value == ethnocentrist ~ "Ethnocentrist",
      max_value == cosmopolitan ~ "Cosmopolitan",
      max_value == egoist ~ "Egoist",
      max_value == undefined ~ "Undefined"
    ),
    dominance_level = max_value / (altruist + ethnocentrist + cosmopolitan + egoist + undefined)
  )

# majority
dominance_data_majority <- data_raw %>%
  group_by(gamma, W) %>%
  summarise(
    altruist = mean(altruistmajority),
    ethnocentrist = mean(ethnocentristmajority),
    cosmopolitan = mean(cosmopolitanmajority),
    egoist = mean(egoistmajority),
    undefined = mean(undefinedmajority)
  ) %>%
  rowwise() %>%
  mutate(
    max_value = max(altruist, ethnocentrist, cosmopolitan, egoist, undefined),
    dominant_type = case_when(
      max_value == altruist ~ "Altruist",
      max_value == ethnocentrist ~ "Ethnocentrist",
      max_value == cosmopolitan ~ "Cosmopolitan",
      max_value == egoist ~ "Egoist",
      max_value == undefined ~ "Undefined"
    ),
    dominance_level = max_value / (altruist + ethnocentrist + cosmopolitan + egoist + undefined)
  )

# minority
dominance_data_minority <- data_raw %>%
  group_by(gamma, W) %>%
  summarise(
    altruist = mean(altruistminority),
    ethnocentrist = mean(ethnocentristminority),
    cosmopolitan = mean(cosmopolitanminority),
    egoist = mean(egoistminority),
    undefined = mean(undefinedminority)
  ) %>%
  rowwise() %>%
  mutate(
    max_value = max(altruist, ethnocentrist, cosmopolitan, egoist, undefined),
    dominant_type = case_when(
      max_value == altruist ~ "Altruist",
      max_value == ethnocentrist ~ "Ethnocentrist",
      max_value == cosmopolitan ~ "Cosmopolitan",
      max_value == egoist ~ "Egoist",
      max_value == undefined ~ "Undefined"
    ),
    dominance_level = max_value / (altruist + ethnocentrist + cosmopolitan + egoist + undefined)
  )

# plot all
facet_all <- ggplot(dominance_data, aes(x = gamma, y = W)) +
  geom_tile(aes(fill = dominant_type, alpha = dominance_level)) +
  geom_text(aes(label = round(dominance_level * 100, 0)), color = "black", size = 3) +
  scale_fill_manual(values = c("Altruist" = "green", 
                               "Ethnocentrist" = "yellow", 
                               "Cosmopolitan" = "blue", 
                               "Egoist" = "orange",
                               "Undefined" = "darkgrey")) +
  scale_alpha_continuous(name = "Proportion (%)", range = c(0.2, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Final Dominant Agent Type (All Groups)",
       x = "Gamma", y = "W", fill = "Dominant Type") +
  theme_minimal() +
  theme(panel.grid = element_blank()) + 
  coord_fixed()

# majority group
facet_majority <- ggplot(dominance_data_majority, aes(x = gamma, y = W)) +
  geom_tile(aes(fill = dominant_type, alpha = dominance_level)) +
  geom_text(aes(label = round(dominance_level * 100, 0)), color = "black", size = 3) +
  scale_fill_manual(values = c("Altruist" = "green", 
                               "Ethnocentrist" = "yellow", 
                               "Cosmopolitan" = "blue", 
                               "Egoist" = "orange", 
                               "Undefined" = "darkgrey")) +
  scale_alpha_continuous(name = "Proportion (%)", range = c(0.2, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Final Dominant Agent Type (Majority)",
       x = "Gamma", y = "W", fill = "Dominant Type") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_fixed()

# minority group
facet_minority <- ggplot(dominance_data_minority, aes(x = gamma, y = W)) +
  geom_tile(aes(fill = dominant_type, alpha = dominance_level)) +
  geom_text(aes(label = round(dominance_level * 100, 0)), color = "black", size = 3) +
  scale_fill_manual(values = c("Altruist" = "green", 
                               "Ethnocentrist" = "yellow", 
                               "Cosmopolitan" = "blue", 
                               "Egoist" = "orange", 
                               "Undefined" = "darkgrey")) +
  scale_alpha_continuous(name = "Proportion (%)", range = c(0.2, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Final Dominant Agent Type (Minority)",
       x = "Gamma", y = "W", fill = "Dominant Type") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_fixed()

# Save Facet Plot
ggsave("../plot/strategies_facet_plot_all_groups.png", plot = facet_all, width = 8, height = 6)
ggsave("../plot/strategies_facet_plot_majority_groups.png", plot = facet_majority, width = 8, height = 6)
ggsave("../plot/strategies_facet_plot_minority_groups.png", plot = facet_minority, width = 8, height = 6)

# Step 3: Line Plots for Majority, Minority, and Combined

# Average data over gamma and W
average_data_gamma <- data_raw %>%
  group_by(gamma) %>%
  summarise(across(altruistmajority:egoistminority, mean, .names = "avg_{col}"))

average_data_W <- data_raw %>%
  group_by(W) %>%
  summarise(across(altruistmajority:egoistminority, mean, .names = "avg_{col}"))

plot_types <- function(data, x_var, group, title_suffix, file_name) {
  if (group == "all") {
    plot <- ggplot(data, aes_string(x = x_var)) +
      geom_line(aes(y = avg_altruistmajority + avg_altruistminority, color = "Altruist")) +
      geom_line(aes(y = avg_ethnocentristmajority + avg_ethnocentristminority, color = "Ethnocentrist")) +
      geom_line(aes(y = avg_cosmopolitanmajority + avg_cosmopolitanminority, color = "Cosmopolitan")) +
      geom_line(aes(y = avg_egoistmajority + avg_egoistminority, color = "Egoist")) +
      scale_color_manual(values = c("Altruist" = "green", 
                                    "Ethnocentrist" = "yellow", 
                                    "Cosmopolitan" = "blue", 
                                    "Egoist" = "orange")) +
      labs(title = paste("Effect of", x_var, "on Agent Types -", title_suffix),
           x = x_var, y = "Number of Agents", color = "Agent Type") +
      ylim(0, 80) + 
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "grey80", size = 0.5), # Make major grid lines lighter
        panel.grid.minor = element_line(color = "grey90", size = 0.25) # Make minor grid lines even lighter
      )
  } else {
    plot <- ggplot(data, aes_string(x = x_var)) +
      geom_line(aes_string(y = paste0("avg_altruist", group), color = "'Altruist'")) +
      geom_line(aes_string(y = paste0("avg_ethnocentrist", group), color = "'Ethnocentrist'")) +
      geom_line(aes_string(y = paste0("avg_cosmopolitan", group), color = "'Cosmopolitan'")) +
      geom_line(aes_string(y = paste0("avg_egoist", group), color = "'Egoist'")) +
      scale_color_manual(values = c("Altruist" = "green", 
                                    "Ethnocentrist" = "yellow", 
                                    "Cosmopolitan" = "blue", 
                                    "Egoist" = "orange")) +
      labs(title = paste("Effect of", x_var, "on Agent Types -", title_suffix),
           x = x_var, y = "Number of Agents", color = "Agent Type") +
      ylim(0, 80) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "grey80", size = 0.5), # Make major grid lines lighter
        panel.grid.minor = element_line(color = "grey90", size = 0.25) # Make minor grid lines even lighter
      )
  }
  
  # Save the plot
  ggsave(file_name, plot = plot, width = 8, height = 6)
}

# Plot and save for Gamma
plot_types(average_data_gamma, "gamma", "majority", "Majority Group Averaged Over W", "../plot/strategies_line_plot_majority_gamma.png")
plot_types(average_data_gamma, "gamma", "minority", "Minority Group Averaged Over W", "../plot/strategies_line_plot_minority_gamma.png")
plot_types(average_data_gamma, "gamma", "all", "All Groups Averaged Over W", "../plot/strategies_line_plot_all_gamma.png")

# Plot and save for W
plot_types(average_data_W, "W", "majority", "Majority Group Averaged Over Gamma", "../plot/strategies_line_plot_majority_W.png")
plot_types(average_data_W, "W", "minority", "Minority Group Averaged Over Gamma", "../plot/strategies_line_plot_minority_W.png")
plot_types(average_data_W, "W", "all", "All Groups Averaged Over Gamma", "../plot/strategies_line_plot_all_W.png")

# Step 4: MANOVA for All, Majority, and Minority Groups

# Combined group variables for MANOVA
manova_data <- data_raw %>%
  mutate(
    altruistall = altruistmajority + altruistminority,
    ethnocentristall = ethnocentristmajority + ethnocentristminority,
    cosmopolitanall = cosmopolitanmajority + cosmopolitanminority,
    egoistall = egoistmajority + egoistminority
  )

# Run and summarize MANOVA for each group

# MANOVA for Combined Group
manova_results_all <- manova(cbind(altruistall, ethnocentristall, cosmopolitanall, egoistall) ~ gamma * W, data = manova_data)
summary(manova_results_all)

# MANOVA for Majority Group
manova_results_majority <- manova(cbind(altruistmajority, ethnocentristmajority, cosmopolitanmajority, egoistmajority) ~ gamma * W, data = manova_data)
summary(manova_results_majority)

# MANOVA for Minority Group
manova_results_minority <- manova(cbind(altruistminority, ethnocentristminority, cosmopolitanminority, egoistminority) ~ gamma * W, data = manova_data)
summary(manova_results_minority)