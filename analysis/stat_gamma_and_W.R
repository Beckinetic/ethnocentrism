# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Data Import and Column Renaming
data_types <- c('baseline', 'perturbation', 'strategies', 'copy', 'pans')
for (data_type in data_types) {
  data_raw <-
    read.csv(paste0(
      '../data/ethnocentrism-proj experiment-',
      data_type,
      '-table.csv'
    ),
    skip = 6)
  
  # Rename columns for simplicity
  colnames(data_raw) <- colnames(data_raw) %>%
    gsub(
      "count\\.turtles\\.with\\.\\.type\\.agent\\.\\.\\.\\.|\\.\\.and\\.tag\\.\\.\\.",
      "",
      .
    ) %>%
    gsub("Altruist", "altruist", .) %>%
    gsub("Ethnocentrist", "ethnocentrist", .) %>%
    gsub("Cosmopolitan", "cosmopolitan", .) %>%
    gsub("Egoist", "egoist", .) %>%
    gsub("Undefined", "undefined", .) %>%
    gsub("majority", "majority", .) %>%
    gsub("minority", "minority", .) %>%
    gsub("\\.", "", .)
  
  # Step 2: Facet Plot (Dominant Agent Type by Gamma and W)
  
  # Define the color mapping for the agent types
  color_mapping <- c(
    "Altruist" = "#00CD6C",
    "Ethnocentrist" = "#F28522",
    "Cosmopolitan" = "#009ADE",
    "Egoist" = "#AF58BA"
  )
  
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
    scale_fill_manual(values = color_mapping) +
    scale_alpha_continuous(
      name = "Proportion (%)",
      range = c(0.2, 1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(title = "Final Dominant Agent Type (All Groups)",
         x = "Gamma",
         y = "W",
         fill = "Dominant Type") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_fixed()
  
  # majority group
  facet_majority <-
    ggplot(dominance_data_majority, aes(x = gamma, y = W)) +
    geom_tile(aes(fill = dominant_type, alpha = dominance_level)) +
    geom_text(aes(label = round(dominance_level * 100, 0)), color = "black", size = 3) +
    scale_fill_manual(values = color_mapping) +
    scale_alpha_continuous(
      name = "Proportion (%)",
      range = c(0.2, 1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(title = "Final Dominant Agent Type (Majority)",
         x = "Gamma",
         y = "W",
         fill = "Dominant Type") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_fixed()
  
  # minority group
  facet_minority <-
    ggplot(dominance_data_minority, aes(x = gamma, y = W)) +
    geom_tile(aes(fill = dominant_type, alpha = dominance_level)) +
    geom_text(aes(label = round(dominance_level * 100, 0)), color = "black", size = 3) +
    scale_fill_manual(values = color_mapping) +
    scale_alpha_continuous(
      name = "Proportion (%)",
      range = c(0.2, 1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(title = "Final Dominant Agent Type (Minority)",
         x = "Gamma",
         y = "W",
         fill = "Dominant Type") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_fixed()
  
  # Save Facet Plot
  ggsave(
    paste0("../plot/", data_type, "_facet_plot_all_groups.png"),
    plot = facet_all,
    width = 8,
    height = 6
  )
  ggsave(
    paste0("../plot/", data_type, "_facet_plot_majority_groups.png"),
    plot = facet_majority,
    width = 8,
    height = 6
  )
  ggsave(
    paste0("../plot/", data_type, "_facet_plot_minority_groups.png"),
    plot = facet_minority,
    width = 8,
    height = 6
  )
  
  # Step 3: Line Plots for Majority, Minority, and Combined
  
  # Function to calculate SE
  calculate_se <- function(x) {
    sd(x) / sqrt(length(x))
  }
  
  # Summarize data by gamma for illustration
  average_data_gamma <- data_raw %>%
    group_by(gamma) %>%
    summarise(
      # Averages for majority
      avg_altruistmajority = mean(altruistmajority),
      avg_ethnocentristmajority = mean(ethnocentristmajority),
      avg_cosmopolitanmajority = mean(cosmopolitanmajority),
      avg_egoistmajority = mean(egoistmajority),
      
      # Averages for minority
      avg_altruistminority = mean(altruistminority),
      avg_ethnocentristminority = mean(ethnocentristminority),
      avg_cosmopolitanminority = mean(cosmopolitanminority),
      avg_egoistminority = mean(egoistminority),
      
      # SE for majority
      se_altruistmajority = calculate_se(altruistmajority),
      se_ethnocentristmajority = calculate_se(ethnocentristmajority),
      se_cosmopolitanmajority = calculate_se(cosmopolitanmajority),
      se_egoistmajority = calculate_se(egoistmajority),
      
      # SE for minority
      se_altruistminority = calculate_se(altruistminority),
      se_ethnocentristminority = calculate_se(ethnocentristminority),
      se_cosmopolitanminority = calculate_se(cosmopolitanminority),
      se_egoistminority = calculate_se(egoistminority)
    ) %>%
    # Combined averages and SEs
    mutate(
      avg_altruistall = avg_altruistmajority + avg_altruistminority,
      avg_ethnocentristall = avg_ethnocentristmajority + avg_ethnocentristminority,
      avg_cosmopolitanall = avg_cosmopolitanmajority + avg_cosmopolitanminority,
      avg_egoistall = avg_egoistmajority + avg_egoistminority,
      
      se_altruistall = sqrt(se_altruistmajority ^ 2 + se_altruistminority ^
                              2),
      se_ethnocentristall = sqrt(se_ethnocentristmajority ^ 2 + se_ethnocentristminority ^
                                   2),
      se_cosmopolitanall = sqrt(se_cosmopolitanmajority ^ 2 + se_cosmopolitanminority ^
                                  2),
      se_egoistall = sqrt(se_egoistmajority ^ 2 + se_egoistminority ^ 2)
    )
  
  average_data_W <- data_raw %>%
    group_by(W) %>%
    summarise(
      # Averages for majority
      avg_altruistmajority = mean(altruistmajority),
      avg_ethnocentristmajority = mean(ethnocentristmajority),
      avg_cosmopolitanmajority = mean(cosmopolitanmajority),
      avg_egoistmajority = mean(egoistmajority),
      
      # Averages for minority
      avg_altruistminority = mean(altruistminority),
      avg_ethnocentristminority = mean(ethnocentristminority),
      avg_cosmopolitanminority = mean(cosmopolitanminority),
      avg_egoistminority = mean(egoistminority),
      
      # SE for majority
      se_altruistmajority = calculate_se(altruistmajority),
      se_ethnocentristmajority = calculate_se(ethnocentristmajority),
      se_cosmopolitanmajority = calculate_se(cosmopolitanmajority),
      se_egoistmajority = calculate_se(egoistmajority),
      
      # SE for minority
      se_altruistminority = calculate_se(altruistminority),
      se_ethnocentristminority = calculate_se(ethnocentristminority),
      se_cosmopolitanminority = calculate_se(cosmopolitanminority),
      se_egoistminority = calculate_se(egoistminority)
    ) %>%
    # Combined averages and SEs
    mutate(
      avg_altruistall = avg_altruistmajority + avg_altruistminority,
      avg_ethnocentristall = avg_ethnocentristmajority + avg_ethnocentristminority,
      avg_cosmopolitanall = avg_cosmopolitanmajority + avg_cosmopolitanminority,
      avg_egoistall = avg_egoistmajority + avg_egoistminority,
      
      se_altruistall = sqrt(se_altruistmajority ^ 2 + se_altruistminority ^
                              2),
      se_ethnocentristall = sqrt(se_ethnocentristmajority ^ 2 + se_ethnocentristminority ^
                                   2),
      se_cosmopolitanall = sqrt(se_cosmopolitanmajority ^ 2 + se_cosmopolitanminority ^
                                  2),
      se_egoistall = sqrt(se_egoistmajority ^ 2 + se_egoistminority ^ 2)
    )
  
  # Plotting Function
  plot_types <-
    function(data,
             x_var,
             group,
             title_suffix,
             file_name) {
      # Set up y-axis aesthetics depending on the group type
      if (group == "all") {
        y_vars <-
          c(
            "avg_altruistall",
            "avg_ethnocentristall",
            "avg_cosmopolitanall",
            "avg_egoistall"
          )
        se_vars <-
          c("se_altruistall",
            "se_ethnocentristall",
            "se_cosmopolitanall",
            "se_egoistall")
      } else {
        y_vars <-
          paste0("avg_",
                 c("altruist", "ethnocentrist", "cosmopolitan", "egoist"),
                 group)
        se_vars <-
          paste0("se_",
                 c("altruist", "ethnocentrist", "cosmopolitan", "egoist"),
                 group)
      }
      
      # Plot
      plot <- ggplot(data, aes_string(x = x_var)) +
        geom_ribbon(aes_string(
          ymin = paste0(y_vars[1], " - ", se_vars[1]),
          ymax = paste0(y_vars[1], " + ", se_vars[1])
        ),
        fill = color_mapping["Altruist"],
        alpha = 0.3) +
        geom_ribbon(aes_string(
          ymin = paste0(y_vars[2], " - ", se_vars[2]),
          ymax = paste0(y_vars[2], " + ", se_vars[2])
        ),
        fill = color_mapping["Ethnocentrist"],
        alpha = 0.3) +
        geom_ribbon(aes_string(
          ymin = paste0(y_vars[3], " - ", se_vars[3]),
          ymax = paste0(y_vars[3], " + ", se_vars[3])
        ),
        fill = color_mapping["Cosmopolitan"],
        alpha = 0.3) +
        geom_ribbon(aes_string(
          ymin = paste0(y_vars[4], " - ", se_vars[4]),
          ymax = paste0(y_vars[4], " + ", se_vars[4])
        ),
        fill = color_mapping["Egoist"],
        alpha = 0.3) +
        
        geom_line(aes_string(y = y_vars[1], color = "'Altruist'")) +
        geom_line(aes_string(y = y_vars[2], color = "'Ethnocentrist'")) +
        geom_line(aes_string(y = y_vars[3], color = "'Cosmopolitan'")) +
        geom_line(aes_string(y = y_vars[4], color = "'Egoist'")) +
        
        scale_color_manual(values = color_mapping) +
        labs(
          title = paste("Effect of", x_var, "on Agent Types -", title_suffix),
          x = x_var,
          y = "Number of Agents",
          color = "Agent Type"
        ) +
        ylim(0, 80) +
        theme_minimal() +
        theme(
          panel.grid.major = element_line(color = "grey80", size = 0.5),
          panel.grid.minor = element_line(color = "grey90", size = 0.25)
        )
      
      # Save the plot
      ggsave(file_name,
             plot = plot,
             width = 8,
             height = 6)
    }
  
  # Plot and save for Gamma
  plot_types(
    average_data_gamma,
    "gamma",
    "majority",
    "Majority Group Averaged Over W",
    paste0("../plot/", data_type, "_line_plot_majority_gamma.png")
  )
  
  plot_types(
    average_data_gamma,
    "gamma",
    "minority",
    "Minority Group Averaged Over W",
    paste0("../plot/", data_type, "_line_plot_minority_gamma.png")
  )
  
  plot_types(
    average_data_gamma,
    "gamma",
    "all",
    "All Groups Averaged Over W",
    paste0("../plot/", data_type, "_line_plot_all_gamma.png")
  )
  
  # Plot and save for W
  plot_types(
    average_data_W,
    "W",
    "majority",
    "Majority Group Averaged Over Gamma",
    paste0("../plot/", data_type, "_line_plot_majority_W.png")
  )
  
  plot_types(
    average_data_W,
    "W",
    "minority",
    "Minority Group Averaged Over Gamma",
    paste0("../plot/", data_type, "_line_plot_minority_W.png")
  )
  
  plot_types(
    average_data_W,
    "W",
    "all",
    "All Groups Averaged Over Gamma",
    paste0("../plot/", data_type, "_line_plot_all_W.png")
  )
  
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
  manova_results_all <-
    manova(cbind(altruistall, ethnocentristall, cosmopolitanall, egoistall) ~ gamma * W,
           data = manova_data)
  summary(manova_results_all)
  
  # MANOVA for Majority Group
  manova_results_majority <-
    manova(
      cbind(
        altruistmajority,
        ethnocentristmajority,
        cosmopolitanmajority,
        egoistmajority
      ) ~ gamma * W,
      data = manova_data
    )
  summary(manova_results_majority)
  
  # MANOVA for Minority Group
  manova_results_minority <-
    manova(
      cbind(
        altruistminority,
        ethnocentristminority,
        cosmopolitanminority,
        egoistminority
      ) ~ gamma * W,
      data = manova_data
    )
  summary(manova_results_minority)
}