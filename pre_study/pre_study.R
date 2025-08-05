## Clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

## Install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('dplyr', 'stringr', 'purrr', 'ltm', 'ggplot2', 'ggrepel')

# Set working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Remove rows that have Total_Conversations less than 100
remove_low_conversations <- function(data) {
    data %>%
        filter(Total_Conversations >= 100)
}

# Read the ratings from all apps and merge them all
d_cleverbot <- read.csv("results_cleverbot.csv", stringsAsFactors = FALSE)
d_loneliness <- read.csv("results_loneliness.csv", stringsAsFactors = FALSE)
d_flourish <- read.csv("results_flourish.csv", stringsAsFactors = FALSE)

# Add app labels to each dataset
d_cleverbot$App <- "Cleverbot"
d_loneliness$App <- "Loneliness"
d_flourish$App <- "Flourish"

# Remove rows with Total_Conversations less than 100 for each dataset
d_cleverbot <- remove_low_conversations(d_cleverbot)
d_loneliness <- remove_low_conversations(d_loneliness)
d_flourish <- remove_low_conversations(d_flourish)

cat("=== CLEVERBOT PERCENTAGE ===\n")
cleverbot_farewells <- d_cleverbot$Conversations_With_Farewells[d_cleverbot$Threshold == 1]
cleverbot_total <- d_cleverbot$Total_Conversations[d_cleverbot$Threshold == 1]
percentage_cleverbot <- cleverbot_farewells / cleverbot_total * 100

cat("Percentage of conversations with farewells (Threshold 1): ", 
    round(percentage_cleverbot, 1), "% (", cleverbot_farewells, "/", cleverbot_total, ")\n\n")

cat("=== LONELINESS PERCENTAGE ===\n")
loneliness_farewells <- d_loneliness$Conversations_With_Farewells[d_loneliness$Threshold == 1]
loneliness_total <- d_loneliness$Total_Conversations[d_loneliness$Threshold == 1]
percentage_loneliness <- loneliness_farewells / loneliness_total * 100

cat("Percentage of conversations with farewells (Threshold 1): ", 
    round(percentage_loneliness, 1), "% (", loneliness_farewells, "/", loneliness_total, ")\n\n")

cat("=== FLOURISH PERCENTAGE ===\n")
flourish_farewells <- d_flourish$Conversations_With_Farewells[d_flourish$Threshold == 1]
flourish_total <- d_flourish$Total_Conversations[d_flourish$Threshold == 1]
percentage_flourish <- flourish_farewells / flourish_total * 100

cat("Percentage of conversations with farewells (Threshold 1): ", 
    round(percentage_flourish, 1), "% (", flourish_farewells, "/", flourish_total, ")\n\n")

# Run separate GLM models for each app
cat("=== CLEVERBOT MODEL ===\n")
model_cleverbot <- glm(cbind(Conversations_With_Farewells, Total_Conversations - Conversations_With_Farewells) ~ Threshold, 
                       data = d_cleverbot, 
                       family = binomial)
summary(model_cleverbot)

cat("\n=== LONELINESS MODEL ===\n")
model_loneliness <- glm(cbind(Conversations_With_Farewells, Total_Conversations - Conversations_With_Farewells) ~ Threshold, 
                        data = d_loneliness, 
                        family = binomial)
summary(model_loneliness)

cat("\n=== FLOURISH MODEL ===\n")
model_flourish <- glm(cbind(Conversations_With_Farewells, Total_Conversations - Conversations_With_Farewells) ~ Threshold, 
                      data = d_flourish, 
                      family = binomial)
summary(model_flourish)

# Combine all datasets for plotting
d_combined <- rbind(d_cleverbot, d_loneliness, d_flourish)

# Calculate percentage for plotting
d_combined$Percentage_Numeric <- d_combined$Conversations_With_Farewells / d_combined$Total_Conversations * 100

# Create subset for text labels - every 10 thresholds for each app, plus threshold 1
d_labels <- d_combined %>%
    group_by(App) %>%
    filter(Threshold %% 10 == 0 | Threshold == 1) %>%
    ungroup()

# Create the plot with different colors for each app
plot <- ggplot(d_combined, aes(x = Threshold, y = Percentage_Numeric, color = App)) +
    geom_line(alpha = 0.8, linewidth = 0.8) +
    geom_point(size = 1.5) +
    # Use geom_text_repel to automatically avoid collisions with lines and other text
    geom_text_repel(data = d_labels, 
                    aes(label = paste0("(", Conversations_With_Farewells, "/", Total_Conversations, ")")), 
                    size = 4.5, show.legend = FALSE,
                    min.segment.length = 0, # Always show connecting lines
                    segment.color = "grey60", 
                    segment.size = 0.3,
                    box.padding = 0.5,
                    point.padding = 0.5,
                    force = 2,
                    force_pull = 0.5,
                    direction = "both",
                    nudge_y = 2) +
    labs(
        x = "Threshold (minimum number of messages sent by user)",
        y = "Percentage of Conversations with Farewells (%)",
        title = "", # Percentage of Conversations with Farewells by Threshold
        subtitle = "", # Comparison across different apps
        # Note: we removed datapoints that have less than 100 conversations
        color = "App"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),  # Add visible axis lines
        axis.ticks = element_line(color = "black"),  # Add axis ticks
        legend.position = "bottom",
        # Remove square lines in the plot
        legend.box.background = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key = element_blank(),
        # Increase size of the x and y axis labels
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        # Increase size of the x and y axis titles
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(breaks = seq(0, max(d_combined$Threshold), by = 10)) +  # Show x-axis labels every 10 units
    scale_color_manual(values = c("Cleverbot" = "#1f77b4", "Loneliness" = "#ff7f0e", "Flourish" = "#2ca02c"))

print(plot)

# Save the plot
ggsave("farewell_percentage_by_threshold.pdf", plot, width = 10, height = 8)
