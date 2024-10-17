# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Data Import and Column Renaming
data_raw <- read.csv('../data/ethnocentrism-proj experiment-likeness-table.csv', skip=6)

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

# Step 2: Facet Plot (Dominant Agent Type by In-group and Out-group Likeness)

# Combined group data for Facet Plot
dominance_data <- data_raw %>%
  group_by(initlikenessingroup, initlikenessoutgroup) %>%
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

# Facet Plot for All Groups
facet_all <- ggplot(dominance_data, aes(x = initlikenessingroup, y = initlikenessoutgroup)) +
  geom_tile(aes(fill = dominant_type, alpha = dominance_level)) +
  geom_text(aes(label = round(dominance_level * 100, 0)), color = "black", size = 3) +
  scale_fill_manual(values = c("Altruist" = "green", 
                               "Ethnocentrist" = "yellow", 
                               "Cosmopolitan" = "blue", 
                               "Egoist" = "orange", 
                               "Undefined" = "darkgrey")) +
  scale_alpha_continuous(name = "Proportion (%)", range = c(0.2, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Final Dominant Agent Type (All Groups)",
       x = "In-group Likeness", y = "Out-group Likeness", fill = "Dominant Type") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_fixed()

# Save Facet Plot for All Groups
ggsave("../plot/likeness_facet_plot_all_groups.png", plot = facet_all, width = 8, height = 6)

# Step 3: Line Plots for Majority, Minority, and Combined

# Average data over initlikenessingroup and initlikenessoutgroup
average_data_ingroup <- data_raw %>%
  group_by(initlikenessingroup) %>%
  summarise(across(altruistmajority:egoistminority, mean, .names = "avg_{col}"))

average_data_outgroup <- data_raw %>%
  group_by(initlikenessoutgroup) %>%
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
        panel.grid.major = element_line(color = "grey80", size = 0.5), 
        panel.grid.minor = element_line(color = "grey90", size = 0.25)
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
        panel.grid.major = element_line(color = "grey80", size = 0.5), 
        panel.grid.minor = element_line(color = "grey90", size = 0.25)
      )
  }
  
  # Save the plot
  ggsave(file_name, plot = plot, width = 8, height = 6)
}

# Plot and save for In-group Likeness
plot_types(average_data_ingroup, "initlikenessingroup", "majority", "Majority Group Averaged Over Out-group Likeness", "../plot/likeness_line_plot_majority_ingroup.png")
plot_types(average_data_ingroup, "initlikenessingroup", "minority", "Minority Group Averaged Over Out-group Likeness", "../plot/likeness_line_plot_minority_ingroup.png")
plot_types(average_data_ingroup, "initlikenessingroup", "all", "All Groups Averaged Over Out-group Likeness", "../plot/likeness_line_plot_all_ingroup.png")

# Plot and save for Out-group Likeness
plot_types(average_data_outgroup, "initlikenessoutgroup", "majority", "Majority Group Averaged Over In-group Likeness", "../plot/likeness_line_plot_majority_outgroup.png")
plot_types(average_data_outgroup, "initlikenessoutgroup", "minority", "Minority Group Averaged Over In-group Likeness", "../plot/likeness_line_plot_minority_outgroup.png")
plot_types(average_data_outgroup, "initlikenessoutgroup", "all", "All Groups Averaged Over In-group Likeness", "../plot/likeness_line_plot_all_outgroup.png")

# Step 4: MANOVA for All, Majority, and Minority Groups

# Combined group variables for MANOVA
manova_data <- data_raw %>%
  mutate(
    altruistall = altruistmajority + altruistminority,
    ethnocentristall = ethnocentristmajority + ethnocentristminority,
    cosmopolitanall = cosmopolitanmajority + cosmopolitanminority,
    egoistall = egoistmajority + egoistminority
  )

# MANOVA for Combined Group
manova_results_all <- manova(cbind(altruistall, ethnocentristall, cosmopolitanall, egoistall) ~ initlikenessingroup * initlikenessoutgroup, data = manova_data)
summary(manova_results_all)

# MANOVA for Majority Group
manova_results_majority <- manova(cbind(altruistmajority, ethnocentristmajority, cosmopolitanmajority, egoistmajority) ~ initlikenessingroup * initlikenessoutgroup, data = manova_data)
summary(manova_results_majority)

# MANOVA for Minority Group
manova_results_minority <- manova(cbind(altruistminority, ethnocentristminority, cosmopolitanminority, egoistminority) ~ initlikenessingroup * initlikenessoutgroup, data = manova_data)
summary(manova_results_minority)