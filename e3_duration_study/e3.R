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
d <- d[d$Finished == "1",]

############################ ATTENTION AND COMPREHENSION CHECKS ############################
# Attention check
size_before <- dim(d)[1]
d <- d[(d$att1 == "2" & d$att2 == "2"),]
print(paste0("Number of participants hired: ", dim(d)[1]))

# For bonus, compensate $0.17 extra for each minute they spent after the farewell message

# Exclude participants based on comprehension check for all 3 conditions: 'Control', 'Prediction', 'Experience'
size_before <- dim(d)[1]
d <- d[d$comp_1 == "2" & d$comp_2 == "1",]
print(paste0("Exclusions from comprehension check: ", size_before - dim(d)[1]))

print(paste0("Number of participants after exclusions: ", dim(d)[1]))

######### Descriptive Stats #########

print(paste0("Age: ", mean(as.numeric(d$age), trim = 0, na.rm = TRUE))) ## mean age
print(paste0("Percentage of females: ", dim(d[d$gender == "2",])[1] / dim(d)[1]))

# Get number of participants in both experience and control conditions
print(table(d$chatbot_farewell_message, d$length))

######### Descriptive Stats #########

# Convert all columns whose column name contains following strings to numeric: 'lone', 'comp'
cols <- colnames(d)
cols <- cols[grepl("_1", cols)]
d[cols] <- lapply(d[cols], as.numeric)

# Cronbach's alpha for all ratings
cronbach.alpha(d[, c("curiosity_1_1", "curiosity_2_1", "curiosity_3_1")])
cronbach.alpha(d[, c("enjoy_1_1", "enjoy_2_1", "enjoy_3_1")])

d$curiosity <- rowMeans(d[, c(
    "curiosity_1_1", 
    "curiosity_2_1", 
    "curiosity_3_1")])

d$enjoy <- rowMeans(d[, c(
    "enjoy_1_1", 
    "enjoy_2_1", 
    "enjoy_3_1")])

# Read the database file and merge by worker_id
db <- read.csv("database.csv")

# Merge db and d by worker_id
d_merged <- merge(d, db, by = "id", all.x = TRUE)


d_merged$after_farewell_duration <- as.numeric(d_merged$after_farewell_duration)
d_merged$after_farewell_n_msgs <- as.numeric(d_merged$after_farewell_n_msgs)
d_merged$after_farewell_n_words <- as.numeric(d_merged$after_farewell_n_words)


duration <- c("long", "short")
conditions <- c("control", "fomo")


# We will conduct a 2 (Duration) x 2 (Manipulation) ANOVA on each of the three 
# post-farewell engagement DV’s (seconds, messages, and number of words, analyzed
# separately) to test whether there is an interaction effect between Duration 
# and Manipulation on engagement.
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
  print(paste0("------- ", var, " -------"))
  anova_result <- aov(as.formula(paste(var, "~ chatbot_farewell_message.x * length")), data = d_merged)
  print(summary(anova_result))
  print(anova_stats(anova_result))
}


# Print mean and SD for the three DVs for all 'chatbot_farewell_message' conditions
for (dur in duration) {
  print(paste0("------- ", dur, " -------"))
  for (cond in conditions) {
    print(paste0("------- ", cond, " -------"))
    for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
      print(paste0("------- ", var, " -------"))
      print(paste0("Mean: ", round(mean(d_merged[d_merged$chatbot_farewell_message.x == cond & d_merged$length == dur, var], na.rm = TRUE), 2)))
      print(paste0("SD: ", round(sd(d_merged[d_merged$chatbot_farewell_message.x == cond & d_merged$length == dur, var], na.rm = TRUE), 2)))
    }
  }
}

# Regardless of the ANOVA results, we will conduct the following five planned t-tests:
#1.	FOMO vs. Control at short duration
#2.	FOMO vs. Control at long duration
#3.	Short vs. long duration in the Control condition
#4.	Short vs. long duration in the FOMO condition
#5.	Difference-in-differences contrast: (FOMO − Control at long duration) vs. (FOMO − Control at short duration)

# 1. 
# FOMO vs. Control at short duration
fomo_short <- d_merged[d_merged$chatbot_farewell_message.x == "fomo" & d_merged$length == "short", ]
control_short <- d_merged[d_merged$chatbot_farewell_message.x == "control" & d_merged$length == "short", ]
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    print(paste0("------- FOMO vs. Control at short duration for ", var, " -------"))
    ttest <- t.test(fomo_short[[var]], control_short[[var]], var.equal = TRUE)
    cd <- cohen.d(fomo_short[[var]], control_short[[var]], paired = FALSE)
    print(paste0("t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
}

# 2.
# FOMO vs. Control at long duration
fomo_long <- d_merged[d_merged$chatbot_farewell_message.x == "fomo" & d_merged$length == "long", ]
control_long <- d_merged[d_merged$chatbot_farewell_message.x == "control" & d_merged$length == "long", ]
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    print(paste0("------- FOMO vs. Control at long duration for ", var, " -------"))
    ttest <- t.test(fomo_long[[var]], control_long[[var]], var.equal = TRUE)
    cd <- cohen.d(fomo_long[[var]], control_long[[var]], paired = FALSE)
    print(paste0("t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
}

# 3.
# Short vs. long duration in the Control condition
control_short <- d_merged[d_merged$chatbot_farewell_message.x == "control" & d_merged$length == "short", ]
control_long <- d_merged[d_merged$chatbot_farewell_message.x == "control" & d_merged$length == "long", ]
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    print(paste0("------- Short vs. long duration in the Control condition for ", var, " -------"))
    ttest <- t.test(control_short[[var]], control_long[[var]], var.equal = TRUE)
    cd <- cohen.d(control_short[[var]], control_long[[var]], paired = FALSE)
    print(paste0("t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
}

# 4.
# Short vs. long duration in the FOMO condition
fomo_short <- d_merged[d_merged$chatbot_farewell_message.x == "fomo" & d_merged$length == "short", ]
fomo_long <- d_merged[d_merged$chatbot_farewell_message.x == "fomo" & d_merged$length == "long", ]
for (var in c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')) {
    print(paste0("------- Short vs. long duration in the FOMO condition for ", var, " -------"))
    ttest <- t.test(fomo_short[[var]], fomo_long[[var]], var.equal = TRUE)
    cd <- cohen.d(fomo_short[[var]], fomo_long[[var]], paired = FALSE)
    print(paste0("t(", round(ttest$parameter, 2), ") = ", round(ttest$statistic, 2), ", p = ", round(ttest$p.value, 3), ", d = ", round(cd$estimate, 2)))
}


# Create three bar plots, one for each DV.
# Put each condition next to each other

# Function to create bar plots
create_bar_plots <- function(variables, variable_labels, data = d_merged) {
    plts <- list()
    for (var in variables) {
        # Calculate means for each condition for sorting
        means_df <- aggregate(as.formula(paste(var, "~ chatbot_farewell_message.x")), 
                              data = data, FUN = mean, na.rm = TRUE)
        # Sort the dataframe by the mean values
        means_df <- means_df[order(means_df[, var], decreasing = TRUE), ]
        
        # Convert condition to factor with levels ordered by mean values
        d_merged_sorted <- data
        d_merged_sorted$chatbot_farewell_message.x <- factor(
            d_merged_sorted$chatbot_farewell_message.x,
            levels = means_df$chatbot_farewell_message.x
        )
        
        # Set factor levels to show short duration first
        d_merged_sorted$length <- factor(d_merged_sorted$length, levels = c("short", "long"))
        
        # Create a named vector for prettier condition labels
        pretty_conditions <- c(
            "control" = "Control",
            "fomo" = "FOMO"
        )
        
        p <- ggplot(d_merged_sorted, aes_string(x = 'chatbot_farewell_message.x', y = var, fill = "length")) +
            geom_bar(stat = "summary", fun = "mean", aes(fill = length), position = position_dodge(width = 0.8), width = 0.8) +
            geom_errorbar(stat = "summary", fun.data = mean_cl_boot, width = 0.2, position = position_dodge(width = 0.8), aes(fill = length)) +
            scale_x_discrete(labels = pretty_conditions) +
            scale_fill_manual(values = c("short" = "gray60", "long" = "gray25")) +
            labs(x = "Chatbot Farewell Message", y = variable_labels[var]) +
            labs(fill = "Duration") +
            theme_minimal() +
            theme_bw() +
            theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            theme_classic() +
            
            theme(
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
            theme(axis.text.x = element_text(size = 16)) +
            theme(axis.text.y = element_text(size = 16)) +
            theme(axis.title.x = element_text(size = 18)) +
            theme(axis.title.y = element_text(size = 18)) +
            theme(plot.title = element_text(size = 18, hjust = 0.5)) +
            theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16)) +
            # remove legend
            theme(legend.position = "none")
        
        plts[[var]] <- p
    }
    return(plts)
}

# Create main outcome plots
outcome_vars <- c('after_farewell_duration', 'after_farewell_n_msgs', 'after_farewell_n_words')
outcome_labels <- c(
    "after_farewell_duration" = "Duration (seconds)",
    "after_farewell_n_msgs" = "Number of Messages",
    "after_farewell_n_words" = "Number of Words"
)
plts <- create_bar_plots(outcome_vars, outcome_labels)

# Save as pdf, with ggsave, with the plots below each other
ggsave("plots/farewell_messages.pdf", gridExtra::grid.arrange(grobs = plts, ncol = 3), width = 12, height = 5, dpi = 350)

# Create mediator plots
mediator_vars <- c('curiosity', 'enjoy')
mediator_labels <- c(
    "curiosity" = "Curiosity",
    "enjoy" = "Enjoyment"
)
mediator_plts <- create_bar_plots(mediator_vars, mediator_labels)

# Save mediator plots as pdf
ggsave("plots/mediator_plots.pdf", gridExtra::grid.arrange(grobs = mediator_plts, ncol = 2), width = 8, height = 5, dpi = 350)




# we will run a parallel mediation model (PROCESS Model 4) with manipulation 
# type as the multicategorical IV (with no manipulation as the reference),
# guilt, curiosity, anger, and enjoyment as mediators, and engagement as the DV.
source("../process.R")
d_merged$chatbot_farewell_message_factor <- factor(d_merged$chatbot_farewell_message.x, levels = c("control", "fomo"))
d_merged$chatbot_farewell_message_numeric <- as.numeric(d_merged$chatbot_farewell_message_factor)
d_merged$length <- factor(d_merged$length, levels = c("short", "long"))
d_merged$length_numeric <- as.numeric(d_merged$length)

# run moderated mediation models (PROCESS Model 7)
process(data = d_merged, y = "after_farewell_duration", x = "chatbot_farewell_message_numeric",
        m = c("curiosity", "enjoy"), w = "length_numeric", model = 7, effsize = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "after_farewell_n_msgs", x = "chatbot_farewell_message_numeric",
        m = c("curiosity", "enjoy"), w = "length_numeric", model = 7, effsize = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "after_farewell_n_words", x = "chatbot_farewell_message_numeric",
        m = c("curiosity", "enjoy"), w = "length_numeric", model = 7, effsize = 1, total = 1, stand = 1,
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
