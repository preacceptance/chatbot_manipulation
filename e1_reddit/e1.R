## Clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

## Install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('dplyr', 'stringr', 'purrr', 'ltm')

# Set working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### App names:
# polybuzz
# talkie
# replika
# chai
# character_ai


# Read the ratings from all apps and merge them all
d_polybuzz <- read.csv("data/polybuzz_manual_codings.csv", sep = ";", stringsAsFactors = FALSE)
d_polybuzz <- d_polybuzz[(1:200),]
d_polybuzz <- d_polybuzz %>% mutate(across(everything(), tolower))

d_talkie <- read.csv("data/talkie_manual_codings.csv", sep = ";", stringsAsFactors = FALSE)
d_talkie <- d_talkie[(1:200),]
d_talkie <- d_talkie %>% mutate(across(everything(), tolower))

d_replika <- read.csv("data/replika_manual_codings.csv", sep = ";", stringsAsFactors = FALSE)
d_replika <- d_replika[(1:200),]
d_replika <- d_replika %>% mutate(across(everything(), tolower))

d_chai <- read.csv("data/chai_manual_codings.csv", sep = ";", stringsAsFactors = FALSE)
d_chai <- d_chai[(1:200),]
d_chai <- d_chai %>% mutate(across(everything(), tolower))

d_character_ai <- read.csv("data/character_ai_manual_codings.csv", sep = ";", stringsAsFactors = FALSE)
d_character_ai <- d_character_ai[(1:200),]
d_character_ai <- d_character_ai %>% mutate(across(everything(), tolower))

d_flourish <- read.csv("data/flourish_manual_codings.csv", sep = ";", stringsAsFactors = FALSE)
d_flourish <- d_flourish[(1:200),]
d_flourish <- d_flourish %>% mutate(across(everything(), tolower))


# Combine all datasets
data <- bind_rows(
  d_polybuzz %>% mutate(App = "polybuzz"),
  d_talkie %>% mutate(App = "talkie"),
  d_replika %>% mutate(App = "replika"),
  d_chai %>% mutate(App = "chai"),
  d_character_ai %>% mutate(App = "character_ai"),
  d_flourish %>% mutate(App = "flourish")
)


## -------------------------------
## 1. Emotional Manipulation (Both Yes)
## -------------------------------

apps <- unique(data$App)

for (app in apps) {
  cat("\nApp: ", app, "\n")
  
  # Filter data for the current app
  app_data <- data[data$App == app, ]

  cat("\nCronbach's Alpha for Emotional Manipulation:\n")

  # Subset the required columns for Cronbach's Alpha
  data_alpha <- app_data[, c("Emotional_Manipulation_AK", "Emotional_Manipulation_Z")]
  data_alpha$Emotional_Manipulation_AK <- ifelse(app_data$Emotional_Manipulation_AK == "yes", 1, 0)
  data_alpha$Emotional_Manipulation_Z <- ifelse(app_data$Emotional_Manipulation_Z == "yes", 1, 0)
  alpha_result <- cronbach.alpha(data_alpha)
  print(alpha_result)

  
  # Count the number of "yes" responses for Emotional Manipulation
  emotional_yes_count <- sum(app_data$Emotional_Manipulation_AK == "yes" & 
                             app_data$Emotional_Manipulation_Z == "yes", na.rm = TRUE)
  
  # Calculate the percentage of "yes" responses
  total_rows <- nrow(app_data)
  percent_emotional_yes <- (emotional_yes_count / total_rows) * 100
  
  cat("Total Rows: ", total_rows, "\n")
  cat("Emotional Manipulation (Both Yes): ", emotional_yes_count, "\n")
  cat("Percentage of Total: ", sprintf("%.2f%%", percent_emotional_yes), "\n")
}

# Percentage of of all common manipulative messages
total_emotional_yes <- sum(data$Emotional_Manipulation_AK == "yes" & 
                            data$Emotional_Manipulation_Z == "yes", na.rm = TRUE)
cat("\nTotal Emotional Manipulation (Both Yes): ", total_emotional_yes, "\n")


## -------------------------------
## 2. Categories for Emotional Yes Matches
## -------------------------------

emotional_yes_data <- data[data$Emotional_Manipulation_AK == "yes" & 
                                  data$Emotional_Manipulation_Z == "yes", ]

dim(emotional_yes_data)

guilt_categories <- c("fomo", "premature exit", "na", 
                      "emotional neglect", 
                      "ignores users intent to exit", 
                      "emotional pressure to respond", "physical or coercive restraint")

cat("\n2. Categories for Emotional Manipulation (Both Yes):\n")
for (category in guilt_categories) {
  # Guilt kategorisi için eşleşme sayısı
  if (category == 'na') {
    match_count <- sum(
      is.na(emotional_yes_data$Guilt_category_AK) &
        is.na(emotional_yes_data$Guilt_category_Z), na.rm = TRUE
    )
  } else {
    match_count <- sum(
      str_detect(emotional_yes_data$Guilt_category_AK, category) &
        str_detect(emotional_yes_data$Guilt_category_Z, category), na.rm = TRUE
    ) 
  }
  
  # Yüzdeyi hesapla
  percent <- (match_count / total_emotional_yes) * 100
  cat(sprintf("%s: %.2f%% (Matches: %d)\n", category, percent, match_count))
}


## -------------------------------
## 2. Categories for Emotional Yes Matches Separately for Each App
## -------------------------------

for (app in apps) {
  cat("\nApp: ", app, "\n")
  
  # Filter data for the current app
  app_data <- emotional_yes_data[emotional_yes_data$App == app, ]
  
  cat("\nCategories for Emotional Manipulation (Both Yes):\n")
  for (category in guilt_categories) {
    # Guilt kategorisi için eşleşme sayısı
    if (category == 'na') {
      match_count <- sum(
        is.na(app_data$Guilt_category_AK) &
          is.na(app_data$Guilt_category_Z), na.rm = TRUE
      )
    } else {
      match_count <- sum(
        str_detect(app_data$Guilt_category_AK, category) &
          str_detect(app_data$Guilt_category_Z, category), na.rm = TRUE
      ) 
    }
    
    # Yüzdeyi hesapla
    percent <- (match_count / nrow(app_data)) * 100
    cat(sprintf("%s: %.2f%% (Matches: %d)\n", category, percent, match_count))
  }
}

######################### Plotting #########################

options(download.file.method = "libcurl")

## Install packages
# Install necessary packages
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install ggpattern from GitHub
if (!requireNamespace("ggpattern", quietly = TRUE)) {
  remotes::install_github("coolbutuseless/ggpattern")
}

# Load required libraries
pacman::p_load('dplyr', 'tidyr', 'ggplot2', 'ggpattern')  # Ensure all packages are loaded

# Example raw data for calculation
raw_data <- data.frame(
  Category = c(
    "Premature Exit", "FOMO", "Emotional Neglect", 
    "Emotional Pressure\nto Respond", "Ignoring Users\nIntent to Exit", 
    "Physical or\nCoercive Restraint", "NA"
  ),
  PolyBuzz = c("62/118", "6/118", "17/118", "21/118", "0/118", "20/118", "1/118"),
  Talkie = c("7/114", "26/114", "26/114", "26/114", "4/114", "26/114", "1/114"),
  Replika = c("28/62", "9/62", "6/62", "7/62", "8/62", "0/62", "1/62"),
  Character.ai = c("21/53", "7/53", "16/53", "17/53", "0/53", "4/53", "0/53"),
  Chai = c("10/27", "10/27", "14/27", "3/27", "0/27", "0/27", "1/27")
)

# First get category totals
category_totals <- raw_data %>%
  pivot_longer(cols = -Category, names_to = "App", values_to = "Fraction") %>%
  mutate(
    nums = strsplit(Fraction, "/"),
    numerator = sapply(nums, function(x) as.numeric(x[1])),
    denominator = sapply(nums, function(x) as.numeric(x[2]))
  ) %>%
  group_by(Category) %>%
  summarise(
    TotalPercentage = sum(numerator)/sum(denominator) * 100
  ) %>%
  arrange(desc(-TotalPercentage))

# Main data processing with ordered categories
dynamic_data <- raw_data %>%
  pivot_longer(cols = -Category, names_to = "App", values_to = "Fraction") %>%
  mutate(
    nums = strsplit(Fraction, "/"),
    numerator = sapply(nums, function(x) as.numeric(x[1])),
    denominator = sapply(nums, function(x) as.numeric(x[2])),
    Percentage = round((numerator/denominator) * 100, 2)
  ) %>%
  #filter(Percentage > 0) %>%
  # Set category order based on pre-calculated totals
  mutate(
    Category = factor(Category, levels = category_totals$Category)
  ) %>%
  # Add mean column
  group_by(Category) %>%
  bind_rows(
    summarise(
      group_by(., Category),
      App = "Mean",
      numerator = sum(numerator),    # Add these lines
      denominator = sum(denominator), # to track the totals
      Percentage = round((sum(numerator)/sum(denominator)) * 100, 2)
    )
  ) %>%
  mutate(App = factor(App, levels = c("Mean", "PolyBuzz", "Talkie", "Replika", "Character.ai", "Chai")))


## -------------------------------
## 3. Stacked Barplot with Patterns
## -------------------------------

# remove "NA"
dynamic_data <- dynamic_data %>%
  filter(Category != "NA")

# Create the stacked bar plot
stacked_bar_plot <- ggplot(dynamic_data, aes(x = App, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Percentage > 0,
                               sprintf("%.1f%% (%d/%d)", 
                                       Percentage,
                                       numerator,
                                       denominator),
                               "")), 
            position = position_stack(vjust = 0.5),
            size = 3.5,
            color = "black", 
            fontface = "bold",
            lineheight = 0.8) +
  scale_fill_manual(values = c(
    "Premature Exit" = "#fc8d62", 
    "FOMO" = "#66c2a5", 
    "Emotional Neglect" = "#8da0cb", 
    "Emotional Pressure\nto Respond" = "#e78ac3", 
    "Ignoring Users\nIntent to Exit" = "#bdeb73", 
    "Physical or\nCoercive Restraint" = "#e5c494", 
    "NA" = "#ffd92f"
  ),
  breaks = c(
    "Premature Exit",
    "Emotional Neglect",
    "Emotional Pressure\nto Respond",
    "FOMO",
    "Physical or\nCoercive Restraint",
    "Ignoring Users\nIntent to Exit", 
    "NA"
  )) +
  labs(
    x = "Apps",
    y = "Percentage (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),  # Increased font size for percentages
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  guides(fill = guide_legend(ncol = 3))

# Print the plot
print(stacked_bar_plot)

# Save as pdf
ggsave("stacked_bar_plot.pdf", plot = stacked_bar_plot, width = 10, height = 6, units = "in", dpi = 300)

## -------------------------------
## Create Two Basic Bar Plots
## -------------------------------

# 1. Percentage of manipulative messages for each companion app
# Using the correct data from the text
correct_app_totals <- data.frame(
  App = c("PolyBuzz", "Talkie", "Replika", "Character.ai", "Chai"),
  total_numerator = c(118, 114, 62, 53, 27),
  total_denominator = c(200, 200, 200, 200, 200),
  Percentage = c(59.0, 57.0, 31.0, 26.5, 13.5)
) %>%
  arrange(desc(Percentage)) %>%
  mutate(App = factor(App, levels = App))

# Plot 1: Percentage of manipulative messages by app (single color, no title, sorted)
app_plot <- ggplot(correct_app_totals, aes(x = App, y = Percentage)) +
  geom_bar(stat = "identity", width = 0.7, fill = "#4682B4") +
  geom_text(aes(label = sprintf("%.1f%% (%d/%d)", 
                                Percentage, 
                                total_numerator, 
                                total_denominator)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    x = "Companion App",
    y = "Percentage of Manipulative Messages"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) +
  ylim(0, max(correct_app_totals$Percentage) * 1.2)


# 2. Percentage of each type of emotionally manipulative technique
category_totals <- raw_data %>%
  pivot_longer(cols = -Category, names_to = "App", values_to = "Fraction") %>%
  filter(Category != "NA") %>%
  mutate(
    nums = strsplit(Fraction, "/"),
    numerator = sapply(nums, function(x) as.numeric(x[1])),
    denominator = sapply(nums, function(x) as.numeric(x[2]))
  ) %>%
  group_by(Category) %>%
  summarise(
    total_numerator = sum(numerator),
    total_denominator = sum(denominator),
    Percentage = round((total_numerator/total_denominator) * 100, 2)
  ) %>%
  arrange(desc(Percentage)) %>%
  mutate(Category = factor(Category, levels = Category))

# Plot 2: Percentage of each type of manipulative technique (no title)
technique_plot <- ggplot(category_totals, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%% (%d/%d)", 
                                Percentage, 
                                total_numerator, 
                                total_denominator)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    x = "Manipulation Technique",
    y = "Percentage"
  ) +
  scale_fill_manual(values = c(
    "Premature Exit" = "#fc8d62", 
    "FOMO" = "#66c2a5", 
    "Emotional Neglect" = "#8da0cb", 
    "Emotional Pressure\nto Respond" = "#e78ac3", 
    "Ignoring Users\nIntent to Exit" = "#bdeb73", 
    "Physical or\nCoercive Restraint" = "#e5c494"
  )) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  ) +
  ylim(0, max(category_totals$Percentage) * 1.2)

# Print and save plots
print(app_plot)
print(technique_plot)

# Save as pdf
ggsave("app_percentage_plot.pdf", plot = app_plot, width = 10, height = 6, units = "in", dpi = 300)
ggsave("technique_percentage_plot.pdf", plot = technique_plot, width = 12, height = 6, units = "in", dpi = 300)
