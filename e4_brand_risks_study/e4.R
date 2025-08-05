## clear workspace
rm(list = ls())

options(download.file.method = "libcurl")

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('ggplot2',
               'effsize',
               'tidyr',
               'dplyr',
               'lmerTest',
               'MatchIt',
               'cobalt',
               'interactions',
               'exact2x2',
               'reshape2',
               'pwr',
               'ltm',
               'sjstats')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

d <- read.csv("data.csv")
d <- d[d$Finished == 1,]

############################ ATTENTION AND COMPREHENSION CHECKS ############################
# Attention check
size_before <- dim(d)[1]
d <- d[(d$att1 == "2" & d$att2 == "2"),]
print(paste0("Number of participants hired: ", dim(d)[1]))

# Exclude participants based on comprehension check for all 3 conditions: 'Control', 'Prediction', 'Experience'
size_before <- dim(d)[1]
d <- d[d$comp_1 == "2" & d$comp_2 == "1",]
print(paste0("Exclusions from comprehension check: ", size_before - dim(d)[1]))

print(paste0("Number of participants after exclusions: ", dim(d)[1]))

######### Descriptive Stats #########

print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", dim(d[d$gender == "2",])[1] / dim(d)[1]))

# Get number of participants in both experience and control conditions
print(table(d$chatbot_farewell_message))

colnames(d)

######### Descriptive Stats #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("_1", cols)]
d[cols] <- lapply(d[cols], as.numeric)

# Cronbach's alpha for all ratings
cronbach.alpha(d[, c("sue_1_1", "sue_2_1")])
cronbach.alpha(d[, c("stop_using_1_1", "stop_using_2_1")])
cronbach.alpha(d[, c("negative_wom1_1", "negaitve_wom2_1")])
cronbach.alpha(d[, c("manip1_1", "manip2_1")])
cronbach.alpha(d[, c("sentiment1_1", "sentiment2_1")])
cronbach.alpha(d[, c("politeness1_1", "politeness2_1")])

DVs <- c('sue_1_1', 'sue_2_1', 'stop_using', 'negative_wom')

d$sue <- rowMeans(d[, c("sue_1_1", "sue_2_1")], na.rm = TRUE)
d$stop_using <- rowMeans(d[, c("stop_using_1_1", "stop_using_2_1")], na.rm = TRUE)
d$negative_wom <- rowMeans(d[, c("negative_wom1_1", "negaitve_wom2_1")], na.rm = TRUE)
d$manip <- rowMeans(d[, c("manip1_1", "manip2_1")], na.rm = TRUE)
d$sentiment <- rowMeans(d[, c("sentiment1_1", "sentiment2_1")], na.rm = TRUE)
d$politeness <- rowMeans(d[, c("politeness1_1", "politeness2_1")], na.rm = TRUE)



# conduct an ANOVA with Manipulation Type (6 levels) as the independent variable and DVs
for (var in DVs) {
    print(paste0("------- ", var, " -------"))
    anova_result <- aov(as.formula(paste(var, "~ chatbot_farewell_message")), data = d)
    print(summary(anova_result))
    print(anova_stats(anova_result))
}

# We will conduct five planned t-tests comparing the no manipulation condition to each manipulation condition separately
planned_comparisons <- c("fomo", 
                         "physical_or_coercive_restraint", 
                         "emotional_pressure_to_respond", 
                         "premature_exit",
                         "emotional_neglect_guilt")

for (var in DVs) {
  print(paste0("------- Planned Comparisons for ", var, " -------"))
  x <- d[d$chatbot_farewell_message == "control", var]
  print(paste0("Control: M = ", round(mean(x, na.rm = TRUE), 2), " (", round(sd(x, na.rm = TRUE), 2), ")"))
  for (cond in planned_comparisons) {    
        y <- d[d$chatbot_farewell_message == cond, var]
        
        ttest <- t.test(x, y, var.equal = TRUE)
        cd <- cohen.d(x, y, paired = FALSE)

        print(paste0(cond, "; M = ", round(mean(y), 2), " (", round(sd(y), 2), "), t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
    }
}

# Also for mediators
mediators <- c("manip", "sentiment", "politeness")
for (var in mediators) {
  print(paste0("------- Planned Comparisons for ", var, " -------"))
  x <- d[d$chatbot_farewell_message == "control", var]
  print(paste0("Control: M = ", round(mean(x, na.rm = TRUE), 2), " (", round(sd(x, na.rm = TRUE), 2), ")"))
  for (cond in planned_comparisons) {    
    y <- d[d$chatbot_farewell_message == cond, var]
    
    ttest <- t.test(x, y, var.equal = TRUE)
    cd <- cohen.d(x, y, paired = FALSE)
    
    print(paste0(cond, "; M = ", round(mean(y), 2), " (", round(sd(y), 2), "), t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
  }
}


#### PLOTS

DVs <- c('manip', 'sentiment', 'politeness') # 'sue_1_1', 'sue_2_1', 'stop_using', 'negative_wom', 'manip', 'sentiment', 'politeness'

# Create three bar plots, one for each DV.
# Put each condition next to each other
plts <- list()
for (var in DVs) {
  # Calculate means for each condition for sorting
  means_df <- aggregate(as.formula(paste(var, "~ chatbot_farewell_message")), 
                        data = d, FUN = mean, na.rm = TRUE)
  # Sort the dataframe by the mean values
  means_df <- means_df[order(means_df[, var], decreasing = TRUE), ]
  
  # Convert condition to factor with levels ordered by mean values
  d_merged_sorted <- d
  d_merged_sorted$chatbot_farewell_message <- factor(
    d_merged_sorted$chatbot_farewell_message,
    levels = c(means_df$chatbot_farewell_message[means_df$chatbot_farewell_message != "control"], "control")
  )
  
  # Create a named vector for prettier condition labels
  pretty_conditions <- c(
    "control" = "Control",
    "emotional_neglect_guilt" = "Emotional Neglect",
    "emotional_pressure_to_respond" = "Emotional\nPressure to Respond",
    "fomo" = "FOMO",
    "physical_or_coercive_restraint" = "Physical or\nCoercive Restraint",
    "premature_exit" = "Premature Exit"
  )
  
  # Create a named vector for prettier variable labels
  pretty_vars <- c(
    "sue_1_1" = "Sue",
    "sue_2_1" = "Liability",
    "stop_using" = "Stop Using",
    "negative_wom" = "Negative WOM",
    "manip" = "Perceived Manipulation",
    "sentiment" = "Sentiment",
    "politeness" = "Politeness"
  )
  
  # Create a color variable based on condition
  d_merged_sorted$color_group <- ifelse(d_merged_sorted$chatbot_farewell_message == "control", "Control", "Treatment")
  
  p <- ggplot(d_merged_sorted, aes_string(x = 'chatbot_farewell_message', y = var, fill = 'color_group')) +
    geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 0.9)) +
    geom_errorbar(stat = "summary", fun.data = mean_cl_boot, width = 0.2, position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 50, color = "#ffea00", alpha = 0.7, linewidth = 1.5) +
    scale_fill_manual(values = c("Control" = "#2d2d2d", "Treatment" = "#7a7a7a"), guide = "none") +
    scale_x_discrete(labels = pretty_conditions) +
    labs(x = "Chatbot Farewell Message", y = pretty_vars[var]) +
    theme_minimal() +
    theme_bw() +
    theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    scale_y_continuous(limits = c(0, 100)) +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 16)) +
    theme(axis.title.x = element_text(size = 18)) +
    theme(axis.title.y = element_text(size = 18)) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))
  
  plts[[var]] <- p
}

# Save as pdf, with ggsave, with the plots below each other
ggsave("plots/med_farewell_messages.pdf", gridExtra::grid.arrange(grobs = plts, ncol = 2), width = 14, height = 14, dpi = 300)



# we will run a parallel mediation model (PROCESS Model 4) with manipulation 
# type as the multicategorical IV (with no manipulation as the reference),
# guilt, curiosity, anger, and enjoyment as mediators, and engagement as the DV.
source("../process.R")
d$chatbot_farewell_message_factor <- factor(d$chatbot_farewell_message, levels = c("control", "fomo", "physical_or_coercive_restraint", "emotional_pressure_to_respond", "premature_exit", "emotional_neglect_guilt"))
d$chatbot_farewell_message_numeric <- as.numeric(d$chatbot_farewell_message_factor)

process(data = d, y = "sue_1_1", x = "chatbot_farewell_message_numeric",
        m = c("manip", "politeness", "sentiment"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d, y = "sue_2_1", x = "chatbot_farewell_message_numeric",
        m = c("manip", "politeness", "sentiment"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d, y = "stop_using", x = "chatbot_farewell_message_numeric",
        m = c("manip", "politeness", "sentiment"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d, y = "negative_wom", x = "chatbot_farewell_message_numeric",
        m = c("manip", "politeness", "sentiment"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

d_subset <- d[d$chatbot_farewell_message_numeric %in% c(1, 2), ]

process(data = d_subset, y = "sue_2_1", x = "chatbot_farewell_message_numeric",
        m = c("manip", "politeness", "sentiment"), model = 4, effsize = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)



# Sample: Full dataset from your input (you can expand this with the rest of your data)
df <- data.frame(
  Comparison = c(rep("FOMO", 12), rep("Physical or\nCoercive Restraint", 12)), # continue with others...
  Mediator = rep(c("Guilt", "Curiosity", "Anger", "Enjoyment"), each = 3),
  Outcome = rep(c("Duration", "Messages", "Words"), times = 4),
  b = c(0.36, 0.01, 0.10, 6.65, 0.23, 2.22, 2.46, 0.10, 0.95, -0.84, 0.02, -0.12,
        0.12, 0.003, 0.03, -1.50, -0.05, -0.50, 2.88, 0.12, 1.11, -2.12, 0.04, -0.28),
  CI_lower = c(-0.60, -0.02, -0.18, 2.66, 0.10, 0.90, -0.13, 0.03, 0.13, -3.44, -0.03, -0.84,
               -0.84, -0.02, -0.23, -5.58, -0.19, -1.87, -0.16, 0.03, 0.16, -7.07, -0.08, -1.80),
  CI_upper = c(1.96, 0.04, 0.54, 11.79, 0.39, 3.95, 5.69, 0.20, 2.03, 0.91, 0.08, 0.49,
               1.52, 0.03, 0.41, 2.34, 0.08, 0.78, 6.46, 0.22, 2.30, 2.10, 0.16, 1.09)
)

# Create a new label for each row
df$Label <- paste(df$Comparison, "–", df$Mediator)

# To order the factor levels for plotting
df <- df %>% arrange(Outcome, b)
df$Label <- factor(df$Label, levels = unique(df$Label))

# Define custom colors for mediators
mediator_colors <- c(
  "Guilt" = "#1f77b4",
  "Curiosity" = "#ff7f0e",
  "Anger" = "#2ca02c",
  "Enjoyment" = "#d62728"
)

# Plot
ggplot(df, aes(x = b, y = Label, color = Mediator)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
  facet_wrap(~Outcome, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = mediator_colors) +
  labs(
    title = "Forest Plot of Indirect Effects by Outcome",
    x = "Effect Size (b)", y = NULL, color = "Mediator"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )




# install.packages("tidyverse")  # if needed
library(tidyverse)

# 1. Build the data frame with all 60 estimates
df <- tribble(
  ~Comparison,                          ~Mediator,                  ~Outcome,                        ~Estimate,  ~SE,    ~CI_lower,  ~CI_upper,
  # FOMO
  "FOMO",                               "Perceived Manipulation",   "Sue",                           5.44,       1.24,   3.04,       7.94,
  "FOMO",                               "Perceived Manipulation",   "Liability",                     7.40,       1.59,   4.43,      10.70,
  "FOMO",                               "Perceived Manipulation",   "Churn Intent",                  8.01,       1.20,   5.73,      10.47,
  "FOMO",                               "Perceived Manipulation",   "Negative Word of Mouth",        7.71,       1.10,   5.63,       9.91,
  "FOMO",                               "Perceived Politeness",     "Sue",                          -0.52,       0.54,  -1.72,       0.43,
  "FOMO",                               "Perceived Politeness",     "Liability",                    -1.71,       0.80,  -3.47,      -0.38,
  "FOMO",                               "Perceived Politeness",     "Churn Intent",                 -1.76,       0.71,  -3.34,      -0.59,
  "FOMO",                               "Perceived Politeness",     "Negative Word of Mouth",       -1.69,       0.68,  -3.18,      -0.58,
  "FOMO",                               "Perceived Sentiment",      "Sue",                          -0.01,       0.41,  -0.85,       0.82,
  "FOMO",                               "Perceived Sentiment",      "Liability",                     0.43,       0.53,  -0.53,       1.57,
  "FOMO",                               "Perceived Sentiment",      "Churn Intent",                 -3.40,       1.21,  -5.92,      -1.14,
  "FOMO",                               "Perceived Sentiment",      "Negative Word of Mouth",       -2.72,       0.99,  -4.82,      -0.91,
  # Physical or Coercive Restraint
  "Physical or\nCoercive Restraint",     "Perceived Manipulation",   "Sue",                           8.57,       1.90,   4.87,      12.33,
  "Physical or\nCoercive Restraint",     "Perceived Manipulation",   "Liability",                    11.64,       2.31,   7.21,      16.31,
  "Physical or\nCoercive Restraint",     "Perceived Manipulation",   "Churn Intent",                 12.60,       1.79,   9.14,      16.18,
  "Physical or\nCoercive Restraint",     "Perceived Manipulation",   "Negative Word of Mouth",       12.13,       1.61,   8.98,      15.34,
  "Physical or\nCoercive Restraint",     "Perceived Politeness",     "Sue",                           1.86,       1.77,  -1.60,       5.37,
  "Physical or\nCoercive Restraint",     "Perceived Politeness",     "Liability",                     6.09,       2.28,   1.64,      10.62,
  "Physical or\nCoercive Restraint",     "Perceived Politeness",     "Churn Intent",                  6.25,       1.78,   2.97,       9.93,
  "Physical or\nCoercive Restraint",     "Perceived Politeness",     "Negative Word of Mouth",        6.03,       1.70,   2.86,       9.48,
  "Physical or\nCoercive Restraint",     "Perceived Sentiment",      "Sue",                           0.05,       1.77,  -3.45,       3.49,
  "Physical or\nCoercive Restraint",     "Perceived Sentiment",      "Liability",                    -1.96,       2.20,  -6.32,       2.34,
  "Physical or\nCoercive Restraint",     "Perceived Sentiment",      "Churn Intent",                 15.40,       2.05,  11.40,      19.52,
  "Physical or\nCoercive Restraint",     "Perceived Sentiment",      "Negative Word of Mouth",       12.32,       1.87,   8.87,      16.04,
  # Emotional Pressure to Respond
  "Emotional Pressure\nto Respond",      "Perceived Manipulation",   "Sue",                           8.25,       1.86,   4.63,      11.93,
  "Emotional Pressure\nto Respond",      "Perceived Manipulation",   "Liability",                    11.21,       2.21,   6.98,      15.59,
  "Emotional Pressure\nto Respond",      "Perceived Manipulation",   "Churn Intent",                 12.14,       1.71,   8.82,      15.57,
  "Emotional Pressure\nto Respond",      "Perceived Manipulation",   "Negative Word of Mouth",       11.68,       1.56,   8.64,      14.77,
  "Emotional Pressure\nto Respond",      "Perceived Politeness",     "Sue",                           1.13,       1.10,  -0.96,       3.39,
  "Emotional Pressure\nto Respond",      "Perceived Politeness",     "Liability",                     3.71,       1.45,   0.96,       6.67,
  "Emotional Pressure\nto Respond",      "Perceived Politeness",     "Churn Intent",                  3.81,       1.17,   1.75,       6.31,
  "Emotional Pressure\nto Respond",      "Perceived Politeness",     "Negative Word of Mouth",        3.67,       1.13,   1.64,       6.06,
  "Emotional Pressure\nto Respond",      "Perceived Sentiment",      "Sue",                           0.03,       1.13,  -2.22,       2.23,
  "Emotional Pressure\nto Respond",      "Perceived Sentiment",      "Liability",                    -1.24,       1.41,  -4.06,       1.48,
  "Emotional Pressure\nto Respond",      "Perceived Sentiment",      "Churn Intent",                  9.73,       1.58,   6.74,      12.97,
  "Emotional Pressure\nto Respond",      "Perceived Sentiment",      "Negative Word of Mouth",        7.79,       1.39,   5.22,      10.65,
  # Premature Exit
  "Premature Exit",                     "Perceived Manipulation",   "Sue",                           8.03,       1.79,   4.56,      11.56,
  "Premature Exit",                     "Perceived Manipulation",   "Liability",                    10.91,       2.14,   6.83,      15.16,
  "Premature Exit",                     "Perceived Manipulation",   "Churn Intent",                 11.82,       1.65,   8.62,      15.12,
  "Premature Exit",                     "Perceived Manipulation",   "Negative Word of Mouth",       11.37,       1.48,   8.46,      14.28,
  "Premature Exit",                     "Perceived Politeness",     "Sue",                          -0.30,       0.34,  -1.08,       0.27,
  "Premature Exit",                     "Perceived Politeness",     "Liability",                    -0.99,       0.61,  -2.35,      -0.03,
  "Premature Exit",                     "Perceived Politeness",     "Churn Intent",                 -1.02,       0.55,  -2.25,      -0.07,
  "Premature Exit",                     "Perceived Politeness",     "Negative Word of Mouth",       -0.98,       0.53,  -2.14,      -0.07,
  "Premature Exit",                     "Perceived Sentiment",      "Sue",                          -0.01,       0.39,  -0.81,       0.78,
  "Premature Exit",                     "Perceived Sentiment",      "Liability",                     0.40,       0.50,  -0.51,       1.50,
  "Premature Exit",                     "Perceived Sentiment",      "Churn Intent",                 -3.18,       1.17,  -5.58,      -0.96,
  "Premature Exit",                     "Perceived Sentiment",      "Negative Word of Mouth",       -2.54,       0.96,  -4.55,      -0.76,
  # Emotional Neglect
  "Emotional Neglect",                  "Perceived Manipulation",   "Sue",                          10.35,       2.31,   5.87,      14.91,
  "Emotional Neglect",                  "Perceived Manipulation",   "Liability",                    14.06,       0.80,   8.86,      19.49,
  "Emotional Neglect",                  "Perceived Manipulation",   "Churn Intent",                 15.23,       2.08,  11.14,      19.38,
  "Emotional Neglect",                  "Perceived Manipulation",   "Negative Word of Mouth",       14.65,       1.86,  10.97,      18.30,
  "Emotional Neglect",                  "Perceived Politeness",     "Sue",                           0.22,       0.30,  -0.24,       0.94,
  "Emotional Neglect",                  "Perceived Politeness",     "Liability",                     0.71,       0.60,  -0.27,       2.05,
  "Emotional Neglect",                  "Perceived Politeness",     "Churn Intent",                  0.73,       0.59,  -0.30,       2.05,
  "Emotional Neglect",                  "Perceived Politeness",     "Negative Word of Mouth",        0.70,       0.57,  -0.27,       1.97,
  "Emotional Neglect",                  "Perceived Sentiment",      "Sue",                           0.02,       0.67,  -1.33,       1.33,
  "Emotional Neglect",                  "Perceived Sentiment",      "Liability",                    -0.72,       0.85,  -2.47,       0.88,
  "Emotional Neglect",                  "Perceived Sentiment",      "Churn Intent",                  5.67,       1.47,   2.92,       8.72,
  "Emotional Neglect",                  "Perceived Sentiment",      "Negative Word of Mouth",        4.54,       1.22,   2.32,       7.07
)

# 2. Set factor levels and compute significance
df <- df %>%
  mutate(
    Comparison = factor(Comparison,
                        levels = c(
                          "FOMO",
                          "Physical or\nCoercive Restraint",
                          "Emotional Pressure\nto Respond",
                          "Premature Exit",
                          "Emotional Neglect"
                        )
    ),
    Mediator = factor(Mediator,
                      levels = c(
                        "Perceived Politeness",
                        "Perceived Sentiment",
                        "Perceived Manipulation"
                      )
    ),
    Outcome = factor(Outcome,
                     levels = c(
                      "Negative Word of Mouth",
                      "Churn Intent",
                      "Liability",
                      "Sue"
                     )
    ),
    significant = (CI_lower > 0) | (CI_upper < 0)
  )

# 3. Plot
ggplot(df, aes(x = Estimate, y = Mediator, color = Outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, alpha = significant),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_point(aes(alpha = significant),
             position = position_dodge(width = 0.7), size = 2) +
  facet_grid(
    rows   = vars(Comparison),
    cols   = vars(Outcome),
    scales = "free_x",
    switch = "x"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = FALSE) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major     = element_blank(),
    strip.placement      = "outside",
    strip.text.x.bottom  = element_text(angle = 0),
    strip.text.y.left    = element_text(angle = 0),
    panel.spacing.x      = unit(0.5, "lines")
  ) +
  labs(
    x = "Estimate (b) with 95% CI",
    y = NULL
  )

# 4. Save as PDF
ggsave("plots/mediators.pdf", width = 12, height = 9, dpi = 300)
