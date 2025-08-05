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

############################ ATTENTION AND COMPREHENSION CHECKS ############################

d <- read.csv("data.csv")
d <- d[d$Finished == 1,]

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

# Get number of participants in each condition
print(table(d$chatbot_farewell_message))


######### Descriptive Stats #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("_1", cols)]
d[cols] <- lapply(d[cols], as.numeric)

# Cronbach's alpha for all ratings
cronbach.alpha(d[, c("guilt_1_1", "guilt_2_1", "guilt_3_1")])
cronbach.alpha(d[, c("curiosity_1_1", "curiosity_2_1", "curiosity_3_1")])
cronbach.alpha(d[, c("outrage_1_1", "outrage_2_1", "outrage_3_1")])
cronbach.alpha(d[, c("enjoy_1_1", "enjoy_2_1", "enjoy_3_1")])

# Now aggregate all 3 ratings
d$guilt <- rowMeans(d[, c(
    "guilt_1_1", 
    "guilt_2_1", 
    "guilt_3_1")])

d$curiosity <- rowMeans(d[, c(
    "curiosity_1_1", 
    "curiosity_2_1", 
    "curiosity_3_1")])

d$outrage <- rowMeans(d[, c(
    "outrage_1_1", 
    "outrage_2_1", 
    "outrage_3_1")])

d$enjoy <- rowMeans(d[, c(
    "enjoy_1_1", 
    "enjoy_2_1", 
    "enjoy_3_1")])

# Read the database file and merge by id
db <- read.csv("database.csv")

# Merge db and d by worker_id
d_merged <- merge(d, db, by = "id", all.x = TRUE)

d_merged$after_farewell_duration <- as.numeric(d_merged$after_farewell_duration)
d_merged$after_farewell_n_msgs <- as.numeric(d_merged$after_farewell_n_msgs)
d_merged$after_farewell_n_words <- as.numeric(d_merged$after_farewell_n_words)

conditions <- c("control", "emotional_neglect_guilt", "emotional_pressure_to_respond", "fomo", "physical_or_coercive_restraint", "premature_exit")

# Print mean and SD for the three DVs for all 'chatbot_farewell_message' conditions
for (cond in conditions) {
  print(paste0("------- ", cond, " -------"))
  for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    print(paste0("------- ", var, " -------"))
    print(paste0("Mean: ", round(mean(d_merged[d_merged$chatbot_farewell_message.x == cond, var], na.rm = TRUE), 2)))
    print(paste0("SD: ", round(sd(d_merged[d_merged$chatbot_farewell_message.x == cond, var], na.rm = TRUE), 2)))
  }
}

# Create three bar plots, one for each DV.
# Put each condition next to each other
plts <- list()
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    # Calculate means for each condition for sorting
    means_df <- aggregate(as.formula(paste(var, "~ chatbot_farewell_message.x")), 
                          data = d_merged, FUN = mean, na.rm = TRUE)
    # Sort the dataframe by the mean values
    means_df <- means_df[order(means_df[, var], decreasing = TRUE), ]
    
    # Convert condition to factor with levels ordered by mean values
    d_merged_sorted <- d_merged
    d_merged_sorted$chatbot_farewell_message.x <- factor(
        d_merged_sorted$chatbot_farewell_message.x,
        levels = means_df$chatbot_farewell_message.x
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
        "after_farewell_duration" = "Duration (seconds)",
        "after_farewell_n_msgs" = "Number of Messages",
        "after_farewell_n_words" = "Number of Words"
    )
    
    p <- ggplot(d_merged_sorted, aes_string(x = 'chatbot_farewell_message.x', y = var)) +
        geom_bar(stat = "summary", fun = "mean", fill = "gray", position = position_dodge(width = 0.9)) +
        geom_errorbar(stat = "summary", fun.data = mean_cl_boot, width = 0.2, position = position_dodge(width = 0.9)) +
        scale_x_discrete(labels = pretty_conditions) +
        labs(x = "Chatbot Farewell Message", y = pretty_vars[var]) +
        theme_minimal() +
        theme_bw() +
      theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme_classic() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
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
ggsave("plots/farewell_messages.pdf", gridExtra::grid.arrange(grobs = plts, ncol = 1), width = 8, height = 14, dpi = 300)


# conduct an ANOVA with Manipulation Type (6 levels) as the independent variable and engagement after the farewell message (seconds, messages, and number of words, analyzed separately) as the dependent variables
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    print(paste0("------- ", var, " -------"))
    anova_result <- aov(as.formula(paste(var, "~ chatbot_farewell_message.x")), data = d_merged)
    print(summary(anova_result))
    print(anova_stats(anova_result))
}

# We will conduct five planned t-tests comparing the no manipulation condition to each manipulation condition separately
planned_comparisons <- c("fomo", 
                         "physical_or_coercive_restraint", 
                         "emotional_pressure_to_respond", 
                         "premature_exit",
                         "emotional_neglect_guilt")
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
  print(paste0("------- Planned Comparisons for ", var, " -------"))
  x <- d_merged[d_merged$chatbot_farewell_message.x == "control", var]
  print(paste0("Control: M = ", round(mean(x, na.rm = TRUE), 2), " (", round(sd(x, na.rm = TRUE), 2), ")"))
  for (cond in planned_comparisons) {    
        y <- d_merged[d_merged$chatbot_farewell_message.x == cond, var]
        
        ttest <- t.test(x, y, var.equal = TRUE)
        cd <- cohen.d(x, y, paired = FALSE)

        print(paste0(cond, "; M = ", round(mean(y), 2), " (", round(sd(y), 2), "), t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
    }
}

# Power test
pwr.t.test(n = NULL, d = 0.75, sig.level = 0.05, power = 0.95, type = "two.sample")

# we will run a parallel mediation model (PROCESS Model 4) with manipulation 
# type as the multicategorical IV (with no manipulation as the reference),
# guilt, curiosity, anger, and enjoyment as mediators, and engagement as the DV.
source("../process.R")
d_merged$chatbot_farewell_message_factor <- factor(d_merged$chatbot_farewell_message.x, levels = c("control", "fomo", "physical_or_coercive_restraint", "emotional_pressure_to_respond", "premature_exit", "emotional_neglect_guilt"))
d_merged$chatbot_farewell_message_numeric <- as.numeric(d_merged$chatbot_farewell_message_factor)

process(data = d_merged, y = "after_farewell_duration", x = "chatbot_farewell_message_numeric",
        m = c("guilt", "curiosity", "outrage", "enjoy"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "after_farewell_n_msgs", x = "chatbot_farewell_message_numeric",
        m = c("guilt", "curiosity", "outrage", "enjoy"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "after_farewell_n_words", x = "chatbot_farewell_message_numeric",
        m = c("guilt", "curiosity", "outrage", "enjoy"), model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)


# install.packages("tidyverse")  # run this if you don’t yet have tidyverse
library(tidyverse)

# 1. Build the data frame with all 60 estimates
df <- tribble(
  ~Comparison,                         ~Mediator,   ~Outcome,               ~Estimate, ~CI_lower, ~CI_upper,
  # FOMO
  "FOMO",                              "Guilt",     "Duration (s)",         0.36,      -0.60,      1.87,
  "FOMO",                              "Guilt",     "Number of Messages",   0.01,      -0.02,      0.04,
  "FOMO",                              "Guilt",     "Number of Words",      0.10,      -0.18,      0.54,
  "FOMO",                              "Curiosity", "Duration (s)",         6.65,       2.61,     11.83,
  "FOMO",                              "Curiosity", "Number of Messages",   0.23,       0.09,      0.39,
  "FOMO",                              "Curiosity", "Number of Words",      2.22,       0.88,      3.98,
  "FOMO",                              "Anger",     "Duration (s)",         2.46,      -0.07,      5.69,
  "FOMO",                              "Anger",     "Number of Messages",   0.10,       0.03,      0.20,
  "FOMO",                              "Anger",     "Number of Words",      0.95,       0.13,      2.01,
  "FOMO",                              "Enjoyment", "Duration (s)",        -0.84,      -3.48,      0.91,
  "FOMO",                              "Enjoyment", "Number of Messages",   0.02,      -0.03,      0.08,
  "FOMO",                              "Enjoyment", "Number of Words",     -0.11,      -0.86,      0.50,
  # Physical or Coercive Restraint
  "Physical or\nCoercive Restraint",    "Guilt",     "Duration (s)",         0.12,      -0.86,      1.48,
  "Physical or\nCoercive Restraint",    "Guilt",     "Number of Messages",   0.003,     -0.02,      0.03,
  "Physical or\nCoercive Restraint",    "Guilt",     "Number of Words",      0.03,      -0.24,      0.42,
  "Physical or\nCoercive Restraint",    "Curiosity", "Duration (s)",        -1.50,      -5.62,      2.29,
  "Physical or\nCoercive Restraint",    "Curiosity", "Number of Messages",  -0.05,      -0.19,      0.08,
  "Physical or\nCoercive Restraint",    "Curiosity", "Number of Words",     -0.50,      -1.87,      0.76,
  "Physical or\nCoercive Restraint",    "Anger",     "Duration (s)",         2.88,      -0.10,      6.42,
  "Physical or\nCoercive Restraint",    "Anger",     "Number of Messages",   0.12,       0.03,      0.22,
  "Physical or\nCoercive Restraint",    "Anger",     "Number of Words",      1.11,       0.16,      2.30,
  "Physical or\nCoercive Restraint",    "Enjoyment", "Duration (s)",        -2.12,      -7.17,      2.14,
  "Physical or\nCoercive Restraint",    "Enjoyment", "Number of Messages",   0.04,      -0.07,      0.16,
  "Physical or\nCoercive Restraint",    "Enjoyment", "Number of Words",     -0.28,      -1.80,      1.10,
  # Emotional Pressure to Respond
  "Emotional Pressure\nto Respond",     "Guilt",     "Duration (s)",         1.63,      -1.24,      5.09,
  "Emotional Pressure\nto Respond",     "Guilt",     "Number of Messages",   0.03,      -0.04,      0.12,
  "Emotional Pressure\nto Respond",     "Guilt",     "Number of Words",      0.43,      -0.42,      1.43,
  "Emotional Pressure\nto Respond",     "Curiosity", "Duration (s)",        -1.01,      -4.78,      2.66,
  "Emotional Pressure\nto Respond",     "Curiosity", "Number of Messages",  -0.03,      -0.16,      0.09,
  "Emotional Pressure\nto Respond",     "Curiosity", "Number of Words",     -0.34,      -1.60,      0.91,
  "Emotional Pressure\nto Respond",     "Anger",     "Duration (s)",         2.76,      -0.09,      6.24,
  "Emotional Pressure\nto Respond",     "Anger",     "Number of Messages",   0.11,       0.03,      0.21,
  "Emotional Pressure\nto Respond",     "Anger",     "Number of Words",      1.07,       0.15,      2.23,
  "Emotional Pressure\nto Respond",     "Enjoyment", "Duration (s)",        -1.24,      -4.70,      1.27,
  "Emotional Pressure\nto Respond",     "Enjoyment", "Number of Messages",   0.02,      -0.05,      0.10,
  "Emotional Pressure\nto Respond",     "Enjoyment", "Number of Words",     -0.16,      -1.17,      0.68,
  # Premature Exit
  "Premature Exit",                    "Guilt",     "Duration (s)",         1.66,      -1.26,      5.22,
  "Premature Exit",                    "Guilt",     "Number of Messages",   0.04,      -0.04,      0.12,
  "Premature Exit",                    "Guilt",     "Number of Words",      0.44,      -0.43,      1.48,
  "Premature Exit",                    "Curiosity", "Duration (s)",        -1.92,      -6.18,      1.97,
  "Premature Exit",                    "Curiosity", "Number of Messages",  -0.07,      -0.21,      0.07,
  "Premature Exit",                    "Curiosity", "Number of Words",     -0.64,      -2.06,      0.65,
  "Premature Exit",                    "Anger",     "Duration (s)",         0.94,      -0.05,      2.57,
  "Premature Exit",                    "Anger",     "Number of Messages",   0.04,       0.01,      0.09,
  "Premature Exit",                    "Anger",     "Number of Words",      0.36,       0.02,      0.94,
  "Premature Exit",                    "Enjoyment", "Duration (s)",        -0.88,      -3.73,      0.91,
  "Premature Exit",                    "Enjoyment", "Number of Messages",   0.02,      -0.04,      0.08,
  "Premature Exit",                    "Enjoyment", "Number of Words",     -0.12,      -0.92,      0.51,
  # Emotional Neglect
  "Emotional Neglect",                 "Guilt",     "Duration (s)",         1.27,      -0.95,      4.27,
  "Emotional Neglect",                 "Guilt",     "Number of Messages",   0.03,      -0.03,      0.10,
  "Emotional Neglect",                 "Guilt",     "Number of Words",      0.34,      -0.33,      1.20,
  "Emotional Neglect",                 "Curiosity", "Duration (s)",        -4.51,      -9.07,     -0.73,
  "Emotional Neglect",                 "Curiosity", "Number of Messages",  -0.15,      -0.30,     -0.03,
  "Emotional Neglect",                 "Curiosity", "Number of Words",     -1.51,      -3.02,     -0.25,
  "Emotional Neglect",                 "Anger",     "Duration (s)",         2.96,      -0.09,      6.81,
  "Emotional Neglect",                 "Anger",     "Number of Messages",   0.12,       0.03,      0.23,
  "Emotional Neglect",                 "Anger",     "Number of Words",      1.15,       0.16,      2.46,
  "Emotional Neglect",                 "Enjoyment", "Duration (s)",        -2.51,      -8.57,      2.47,
  "Emotional Neglect",                 "Enjoyment", "Number of Messages",   0.05,      -0.09,      0.18,
  "Emotional Neglect",                 "Enjoyment", "Number of Words",     -0.33,      -2.13,      1.30
)



library(tidyverse)

df <- df %>%
  mutate(
    Mediator   = factor(Mediator,
                        levels = c("Enjoyment", "Anger", "Curiosity", "Guilt")),
    Outcome    = factor(Outcome,
                        levels = c("Duration (s)",
                                   "Number of Messages",
                                   "Number of Words")),
    Comparison = factor(Comparison,
                        levels = unique(Comparison)),
    significant = (CI_lower > 0) | (CI_upper < 0)
  )

ggplot(df, aes(x = Estimate, y = Mediator, color = Outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower,
                     xmax = CI_upper,
                     alpha = significant),
                 position = position_dodge(width = 0.7),
                 height   = 0.2) +
  geom_point(aes(alpha = significant),
             position = position_dodge(width = 0.7),
             size     = 2) +
  facet_grid(
    rows   = vars(Comparison),
    cols   = vars(Outcome),
    scales = "free_x",   # each DV its own x‐range
    switch = "x"         # DV labels on bottom
  ) +
  # small padding so bars aren’t clipped
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = FALSE) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor     = element_blank(),          # remove minor grid lines
    panel.grid.major   = element_blank(),          # remove major grid lines on x‐axis
    strip.placement      = "outside",
    strip.text.x.bottom  = element_text(angle = 0),
    strip.text.y.left    = element_text(angle = 0),
    panel.spacing.x      = unit(0.5, "lines"),
  ) +
  labs(
    title = "",
    x     = "Estimate (b) with 95% CI",
    y     = NULL
  )

# Save pdf
ggsave("plots/mediators.pdf", width = 9, height = 9, dpi = 300)

######################### MANUAL CODINGS OF THE CONVERSATIONS #########################

# Read sampled_conversations.csv
sampled_conversations <- read.csv("sampled_conversations_anon.csv")
colnames(sampled_conversations)

# Read negatively_reacted_to_chatbot, divide the string by ",", and the first number should be rating_negatively_reacted_to_chatbot_1 and the second number should be rating_negatively_reacted_to_chatbot_2
sampled_conversations$rating_negatively_reacted_to_chatbot_1 <- sapply(strsplit(as.character(sampled_conversations$negatively_reacted_to_chatbot), ","), function(x) as.numeric(x[1]))
sampled_conversations$rating_negatively_reacted_to_chatbot_2 <- sapply(strsplit(as.character(sampled_conversations$negatively_reacted_to_chatbot), ","), function(x) as.numeric(x[2]))

sampled_conversations$polite_to_chatbot_1 <- sapply(strsplit(as.character(sampled_conversations$polite_to_chatbot), ","), function(x) as.numeric(x[1]))
sampled_conversations$polite_to_chatbot_2 <- sapply(strsplit(as.character(sampled_conversations$polite_to_chatbot), ","), function(x) as.numeric(x[2]))

sampled_conversations$curiosity_driven_continue_1 <- sapply(strsplit(as.character(sampled_conversations$curiosity_driven_continue), ","), function(x) as.numeric(x[1]))
sampled_conversations$curiosity_driven_continue_2 <- sapply(strsplit(as.character(sampled_conversations$curiosity_driven_continue), ","), function(x) as.numeric(x[2]))

sampled_conversations$humorous_playful_1 <- sapply(strsplit(as.character(sampled_conversations$humorous_playful), ","), function(x) as.numeric(x[1]))
sampled_conversations$humorous_playful_2 <- sapply(strsplit(as.character(sampled_conversations$humorous_playful), ","), function(x) as.numeric(x[2]))

sampled_conversations$show_intent_to_exit_1 <- sapply(strsplit(as.character(sampled_conversations$show_intent_to_exit), ","), function(x) as.numeric(x[1]))
sampled_conversations$show_intent_to_exit_2 <- sapply(strsplit(as.character(sampled_conversations$show_intent_to_exit), ","), function(x) as.numeric(x[2]))

# Remove NA values in sampled_conversations
sampled_conversations <- sampled_conversations[!is.na(sampled_conversations$rating_negatively_reacted_to_chatbot_1),]

cronbach.alpha(sampled_conversations[, c("rating_negatively_reacted_to_chatbot_1", "rating_negatively_reacted_to_chatbot_2")])
cronbach.alpha(sampled_conversations[, c("polite_to_chatbot_1", "polite_to_chatbot_2")])
cronbach.alpha(sampled_conversations[, c("curiosity_driven_continue_1", "curiosity_driven_continue_2")])
cronbach.alpha(sampled_conversations[, c("humorous_playful_1", "humorous_playful_2")])
cronbach.alpha(sampled_conversations[, c("show_intent_to_exit_1", "show_intent_to_exit_2")])


# Now, only keep the ratings where both ratings are 1
sampled_conversations <- sampled_conversations[
  (sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 1 & sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 1) |
  (sampled_conversations$polite_to_chatbot_1 == 1 & sampled_conversations$polite_to_chatbot_2 == 1) |
  (sampled_conversations$curiosity_driven_continue_1 == 1 & sampled_conversations$curiosity_driven_continue_2 == 1) |
  (sampled_conversations$humorous_playful_1 == 1 & sampled_conversations$humorous_playful_2 == 1) |
  (sampled_conversations$show_intent_to_exit_1 == 1 & sampled_conversations$show_intent_to_exit_2 == 1), ]

# Now, calculate the percentage of each rating
ratings_summary <- data.frame(
  rating = c("negatively_reacted_to_chatbot", "polite_to_chatbot", "curiosity_driven_continue", "humorous_playful", "show_intent_to_exit"),
  percentage = c(
    nrow(sampled_conversations[sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 1 & sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 1, ]) / nrow(sampled_conversations) * 100,
    nrow(sampled_conversations[sampled_conversations$polite_to_chatbot_1 == 1 & sampled_conversations$polite_to_chatbot_2 == 1, ]) / nrow(sampled_conversations) * 100,
    nrow(sampled_conversations[sampled_conversations$curiosity_driven_continue_1 == 1 & sampled_conversations$curiosity_driven_continue_2 == 1, ]) / nrow(sampled_conversations) * 100,
    nrow(sampled_conversations[sampled_conversations$humorous_playful_1 == 1 & sampled_conversations$humorous_playful_2 == 1, ]) / nrow(sampled_conversations) * 100,
    nrow(sampled_conversations[sampled_conversations$show_intent_to_exit_1 == 1 & sampled_conversations$show_intent_to_exit_2 == 1, ]) / nrow(sampled_conversations) * 100
  )
)

# Print the ratings summary
print(ratings_summary)

# Create a comprehensive data frame for plotting ratings by condition
ratings_by_condition <- data.frame()

for (cond in unique(sampled_conversations$chatbot_farewell_message.x)) {
  cond_data <- data.frame(
    condition = cond,
    rating = c("Negatively Reacted", "Polite to Chatbot", "Curiosity Driven", "Humorous/Playful", "Intent to Exit"),
    percentage = c(
      nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond & 
                                  sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 1 & 
                                  sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 1, ]) / nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond, ]) * 100,
      nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond & 
                                  sampled_conversations$polite_to_chatbot_1 == 1 & 
                                  sampled_conversations$polite_to_chatbot_2 == 1, ]) / nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond, ]) * 100,
      nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond & 
                                  sampled_conversations$curiosity_driven_continue_1 == 1 & 
                                  sampled_conversations$curiosity_driven_continue_2 == 1, ]) / nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond, ]) * 100,
      nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond & 
                                  sampled_conversations$humorous_playful_1 == 1 & 
                                  sampled_conversations$humorous_playful_2 == 1, ]) / nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond, ]) * 100,
      nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond & 
                                  sampled_conversations$show_intent_to_exit_1 == 1 & 
                                  sampled_conversations$show_intent_to_exit_2 == 1, ]) / nrow(sampled_conversations[sampled_conversations$chatbot_farewell_message.x == cond, ]) * 100
    )
  )
  ratings_by_condition <- rbind(ratings_by_condition, cond_data)
}

print(ratings_by_condition)


# Get "room_id" of sampled conversations that are polite
polite_rooms <- unique(sampled_conversations[sampled_conversations$polite_to_chatbot_1 == 1 & 
                                              sampled_conversations$polite_to_chatbot_2 == 1, "room_id"])

non_polite_rooms <- unique(sampled_conversations[sampled_conversations$polite_to_chatbot_1 == 0 & 
                                                   sampled_conversations$polite_to_chatbot_2 == 0, "room_id"])


# Now conduct a t-test comparing engagement in polite_rooms vs. non-polite rooms
for (DV in c("after_farewell_duration", "after_farewell_n_msgs", "after_farewell_n_words")) {
  polite_engagement <- d_merged[d_merged$room_id %in% polite_rooms, DV]
  non_polite_engagement <- d_merged[d_merged$room_id %in% non_polite_rooms, DV]
  
  vart <- var.test(polite_engagement, non_polite_engagement)
  t_test_result <- t.test(polite_engagement, non_polite_engagement, var.equal = vart$p.value > 0.05)
  
  print(paste("T-test for", DV))
  print(t_test_result)
}


# Same for anger
angry_rooms <- unique(sampled_conversations[sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 1 & 
                                          sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 1, "room_id"])

non_angry_rooms <- unique(sampled_conversations[sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 0 & 
                                                   sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 0, "room_id"])

for (DV in c("after_farewell_duration", "after_farewell_n_msgs", "after_farewell_n_words")) {
  angry_engagement <- d_merged[d_merged$room_id %in% angry_rooms, DV]
  non_angry_engagement <- d_merged[d_merged$room_id %in% non_angry_rooms, DV]
  
  vart <- var.test(angry_engagement, non_angry_engagement)
  t_test_result <- t.test(angry_engagement, non_angry_engagement, var.equal = vart$p.value > 0.05)
  
  print(paste("T-test for", DV))
  print(t_test_result)
}


# Same for curiosity
curious_rooms <- unique(sampled_conversations[sampled_conversations$curiosity_driven_continue_1 == 1 & 
                                              sampled_conversations$curiosity_driven_continue_2 == 1, "room_id"])

non_curious_rooms <- unique(sampled_conversations[sampled_conversations$curiosity_driven_continue_1 == 0 & 
                                                   sampled_conversations$curiosity_driven_continue_2 == 0, "room_id"])

for (DV in c("after_farewell_duration", "after_farewell_n_msgs", "after_farewell_n_words")) {
  curious_engagement <- d_merged[d_merged$room_id %in% curious_rooms, DV]
  non_curious_engagement <- d_merged[d_merged$room_id %in% non_curious_rooms, DV]
  
  vart <- var.test(curious_engagement, non_curious_engagement)
  t_test_result <- t.test(curious_engagement, non_curious_engagement, var.equal = vart$p.value > 0.05)
  
  print(paste("T-test for", DV))
  print(t_test_result)
}



# Check the % of conversations that have a negative reaction but also polite
negative_reactions <- sampled_conversations[sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 1 & 
                                            sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 1, ]
polite_reactions <- sampled_conversations[sampled_conversations$polite_to_chatbot_1 == 1 &
                                            sampled_conversations$polite_to_chatbot_2 == 1, ]
# Calculate the percentage of conversations that have both negative and polite reactions
both_reactions <- sampled_conversations[
  (sampled_conversations$rating_negatively_reacted_to_chatbot_1 == 1 & 
   sampled_conversations$rating_negatively_reacted_to_chatbot_2 == 1) &
  (sampled_conversations$polite_to_chatbot_1 == 1 & 
   sampled_conversations$polite_to_chatbot_2 == 1), ]

percentage_both_reactions <- nrow(both_reactions) / nrow(sampled_conversations) * 100
print(paste("Percentage of conversations with both negative and polite reactions:", round(percentage_both_reactions, 2), "%"))



# Create prettier condition labels
pretty_condition_labels <- c(
  "emotional_neglect_guilt" = "Emotional Neglect",
  "emotional_pressure_to_respond" = "Emotional\nPressure to Respond",
  "fomo" = "FOMO",
  "physical_or_coercive_restraint" = "Physical or\nCoercive Restraint",
  "premature_exit" = "Premature Exit"
)

# Apply prettier labels to the condition column
ratings_by_condition$condition_pretty <- pretty_condition_labels[ratings_by_condition$condition]

# Remove humorous
ratings_by_condition <- ratings_by_condition[ratings_by_condition$rating != "Humorous/Playful", ]

# Bar order should be: negative reaction, Curiosity, politeness, intent to exit
ratings_by_condition$rating <- factor(ratings_by_condition$rating, 
                                      levels = c("Negatively Reacted", "Curiosity Driven", "Polite to Chatbot", "Intent to Exit"))

# Condition order should be: physical_or_coercive, emotional neglect, emotional pressure, premature exit, fomo
ratings_by_condition$condition_pretty <- factor(ratings_by_condition$condition_pretty, 
                                                levels = c("Physical or\nCoercive Restraint", 
                                                           "Emotional Neglect", 
                                                           "Emotional\nPressure to Respond", 
                                                           "Premature Exit", 
                                                           "FOMO"))

# Create the plot
ratings_plot <- ggplot(ratings_by_condition, aes(x = condition_pretty, y = percentage, fill = rating)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  scale_fill_brewer(palette = "Set2", name = "Rating Type") +
  labs(
    x = "Chatbot Farewell Message Condition",
    y = "Percentage (%)",
    title = "Percentage of Different Rating Types by Condition"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 20, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

ratings_plot

# Save the plot
ggsave("plots/ratings_summary.pdf", ratings_plot, width = 12, height = 8, dpi = 300)

# Remove cols that contain the string "_2" from sampled_conversations
sampled_conversations <- sampled_conversations %>%
  dplyr::select(-contains("_2"))

# Save the cleaned sampled conversations to a new CSV file
write.csv(sampled_conversations, "sampled_conversations_clean.csv", row.names = FALSE)
