# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)
library(ggh4x)
library(cowplot)

# Source functions used in the Bayesian analyses and figure generation
source("smile25b_functions.R")

# Create secondary analysis folder (if it doesn't exist)
if (!dir.exists("data/secondary_analysis")) {
  dir.create("data/secondary_analysis")
}

# read processed data set
df <- read_csv("data/smile25b_processed_data.csv")

# Prepare dataframe for data analysis
df_long <- df %>%
  # create a unique participant id
  mutate(id = row_number()) %>%
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, SP_DEQ_fear_total, SP_DEQ_anger_total,
    SP_SWL_total, SP_Burnout_total,
    NP_DEQ_happy_total, NP_DEQ_fear_total, NP_DEQ_anger_total,
    NP_SWL_total, NP_Burnout_total,
    face_compliance_scalar, face_compliance_binary, math_errors
  ) %>%
  # Pivot smile and natural pose columns into long format
  pivot_longer(
    cols = c(starts_with("SP_"), starts_with("NP_")),
    names_to = c("pose", ".value"),
    names_pattern = "^(SP|NP)_(.*)$"
  ) %>%
  # recode pose variable into readable labels
  mutate(pose = factor(
    case_when(
      pose == "SP" ~ "smile",
      pose == "NP" ~ "natural",
      TRUE ~ pose
    ),
    levels = c("smile", "natural")
  )) %>%
  relocate(pose, .after = id)

# Secondary analyses

# Satisfaction with life predicted by pose, context, threat, and repetition
SWL_model <- lmer(
  SWL_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

SWL_freq_anova <- as.data.frame(anova(SWL_model)) %>%
  tibble::rownames_to_column("term")

print(summary(SWL_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
SWL_emm <- emmeans(SWL_model, ~ pose | context + threat + repetition)

summary(SWL_emm)

# Extract pairwise comparisons of pose across the other conditions
SWL_emm_pairs <- as.data.frame(pairs(SWL_emm))

summary(SWL_emm_pairs)

# Calculate the effect size of the pose difference
SWL_emm_effect_size <- as.data.frame(
  eff_size(
    SWL_emm,
    sigma = sigma(SWL_model),
    edf = df.residual(SWL_model)
  )
)

# Add effect size estimates to the emm summary
SWL_emm_summary <- SWL_emm_pairs %>%
  left_join(
    SWL_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(SWL_emm_summary)

# Draw the SWL score with the custom function 'draw_plot'
SWL_plot <- draw_plot(
  df_wide = df,
  outcome = "SWL_total",
  outcome_label = "satisfaction with life",
  legend_position = "none",
  x_axis = FALSE,
  y_axis = FALSE,
  y_text = "Δ Satisfaction with Life Reports",
  y_breaks = c(-.2, 0, .2)
)

print(SWL_plot)

# SWL Bayesian analysis

print(SWL_start_time <- Sys.time())

# Compute Bayes Factor anova for the SWL scores using the custom function 'compute_bf_anova'
SWL_bf_anova <- compute_bf_anova(df_long, "SWL_total")

print(SWL_execution_time <- Sys.time() - SWL_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(SWL_bf_anova, "data/secondary_analysis/smile25b_SWL_bf_anova.Rds")
SWL_bf_anova <- readRDS("data/secondary_analysis/smile25b_SWL_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
SWL_bf_anova_table <- extract_bf_anova(SWL_bf_anova)

print(SWL_bf_anova_table)

# Combine frequentist and bayesian anova tables

SWL_combined_anova <- SWL_freq_anova %>%
  left_join(SWL_bf_anova_table, by = c("term" = "term_dropped"))

print(SWL_combined_anova)

# Highlight the pre-registered two-way interactions
SWL_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
SWL_simple_effects <- compute_bf_simple_effects(df, "SWL_total")

print(SWL_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
SWL_stat_summary <- SWL_emm_summary %>%
  left_join(
    SWL_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of inferential statistics of the pose differences conditioned by context, threat, and repetition
print(SWL_stat_summary)

# Save SWL analysis results
saveRDS(
  SWL_stat_summary,
  "data/secondary_analysis/smile25b_SWL_results_simple.Rds"
)

# Burnout predicted by pose, context, threat, and repetition
Burnout_model <- lmer(
  Burnout_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(Burnout_model))

# Store model's frequentist ANOVA
Burnout_freq_anova <- as.data.frame(anova(Burnout_model)) %>%
  tibble::rownames_to_column("term")

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
Burnout_emm <- emmeans(Burnout_model, ~ pose | context + threat + repetition)

summary(Burnout_emm)

# Extract pairwise comparisons of pose across the other conditions
Burnout_emm_pairs <- as.data.frame(pairs(Burnout_emm))

summary(Burnout_emm_pairs)

# Calculate the effect size of the pose difference
Burnout_emm_effect_size <- as.data.frame(
  eff_size(
    Burnout_emm,
    sigma = sigma(Burnout_model),
    edf = df.residual(Burnout_model)
  )
)

# Add effect size estimates to the emm summary
Burnout_emm_summary <- Burnout_emm_pairs %>%
  left_join(
    Burnout_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(Burnout_emm_summary)

# Draw the Burnout score with the custom function 'draw_plot'
Burnout_plot <- draw_plot(
  df_wide = df,
  outcome = "Burnout_total",
  outcome_label = "burnout",
  legend_position = "top_right",
  x_axis = FALSE,
  y_axis = FALSE,
  y_text = "Δ Burnout Reports",
  y_breaks = c(-.2, 0, .2)
)

print(Burnout_plot)

# Burnout Bayesian analysis

print(Burnout_start_time <- Sys.time())

# Compute Bayes Factor anova for the Burnout scores using the custom function 'compute_bf_anova'
Burnout_bf_anova <- compute_bf_anova(df_long, "Burnout_total")

print(Burnout_execution_time <- Sys.time() - Burnout_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(Burnout_bf_anova, "data/secondary_analysis/smile25b_Burnout_bf_anova.Rds")
Burnout_bf_anova <- readRDS("data/secondary_analysis/smile25b_Burnout_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
Burnout_bf_anova_table <- extract_bf_anova(Burnout_bf_anova)

print(Burnout_bf_anova_table)

# Combine frequentist and bayesian anova tables
Burnout_combined_anova <- Burnout_freq_anova %>%
  left_join(Burnout_bf_anova_table, by = c("term" = "term_dropped"))

print(Burnout_combined_anova)

# Highlight the pre-registered two-way interactions
Burnout_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
Burnout_simple_effects <- compute_bf_simple_effects(df, "Burnout_total")

print(Burnout_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
Burnout_stat_summary <- Burnout_emm_summary %>%
  left_join(
    Burnout_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of inferential statistics of the pose differences conditioned by context, threat, and repetition
print(Burnout_stat_summary)

# Save Burnout analysis results
saveRDS(
  Burnout_stat_summary,
  "data/secondary_analysis/smile25b_Burnout_results_simple.Rds"
)

# Exploratory analysis: Outcome = fear, dataset = full

# Full factorial general linear mixed model of fear predicted by pose, context, threat, and repetition
fear_model <- lmer(
  DEQ_fear_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(fear_model))

fear_freq_anova <- as.data.frame(anova(fear_model)) %>%
  tibble::rownames_to_column("term")

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
fear_emm <- emmeans(fear_model, ~ pose | context + threat + repetition)

summary(fear_emm)

# Extract pairwise comparisons of pose across the other conditions
fear_emm_pairs <- as.data.frame(pairs(fear_emm))

summary(fear_emm_pairs)

# Calculate the effect size of the pose difference
fear_emm_effect_size <- as.data.frame(
  eff_size(
    fear_emm,
    sigma = sigma(fear_model),
    edf = df.residual(fear_model)
  )
)

# Add effect size estimates to the emm summary
fear_emm_summary <- fear_emm_pairs %>%
  left_join(
    fear_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(fear_emm_summary)

# Draw the Fear score with the custom function 'draw_plot'
fear_plot <- draw_plot(
  df_wide = df,
  outcome = "DEQ_fear_total",
  outcome_label = "fear",
  legend_position = "none",
  x_axis = TRUE,
  y_axis = FALSE,
  y_text = "Δ Fear Reports",
  y_breaks = c(-.4, 0, .4)
)

print(fear_plot)

# Fear Bayesian analysis

print(fear_start_time <- Sys.time())

# Compute Bayes Factor anova for the Fear scores using the custom function 'compute_bf_anova'
fear_bf_anova <- compute_bf_anova(df_long, "DEQ_fear_total")

print(fear_execution_time <- Sys.time() - fear_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(fear_bf_anova, "data/secondary_analysis/smile25b_fear_bf_anova.Rds")
fear_bf_anova <- readRDS("data/secondary_analysis/smile25b_fear_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
fear_bf_anova_table <- extract_bf_anova(fear_bf_anova)

print(fear_bf_anova_table)

# Combine frequentist and bayesian anova tables

fear_combined_anova <- fear_freq_anova %>%
  left_join(fear_bf_anova_table, by = c("term" = "term_dropped"))

print(fear_combined_anova)

# Highlight the pre-registered two-way interactions
fear_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
fear_simple_effects <- compute_bf_simple_effects(
  df_wide = df,
  outcome = "DEQ_fear_total"
)

print(fear_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
fear_stat_summary <- fear_emm_summary %>%
  left_join(
    fear_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of inferential statistics of the pose differences conditioned by context, threat, and repetition
print(fear_stat_summary)

# Save fear analysis results
saveRDS(
  fear_stat_summary,
  "data/secondary_analysis/smile25b_fear_results_simple.Rds"
)

# Exploratory analysis: Outcome = anger, dataset = full

# Full factorial general linear mixed model of anger predicted by pose, context, threat, and repetition
anger_model <- lmer(
  DEQ_anger_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(anger_model))

anger_freq_anova <- as.data.frame(anova(anger_model)) %>%
  tibble::rownames_to_column("term")

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
anger_emm <- emmeans(anger_model, ~ pose | context + threat + repetition)

summary(anger_emm)

# Extract pairwise comparisons of pose across the other conditions
anger_emm_pairs <- as.data.frame(pairs(anger_emm))

summary(anger_emm_pairs)

# Calculate the effect size of the pose difference
anger_emm_effect_size <- as.data.frame(
  eff_size(
    anger_emm,
    sigma = sigma(anger_model),
    edf = df.residual(anger_model)
  )
)

# Add effect size estimates to the emm summary
anger_emm_summary <- anger_emm_pairs %>%
  left_join(
    anger_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(anger_emm_summary)

# Draw the Anger score with the custom function 'draw_plot'
anger_plot <- draw_plot(
  df_wide = df,
  outcome = "DEQ_anger_total",
  outcome_label = "anger",
  legend_position = "none",
  x_axis = TRUE,
  y_axis = FALSE,
  y_text = "Δ Anger Reports",
  y_breaks = c(-.2, .4, 1)
)

print(anger_plot)

# Anger Bayesian analysis

print(anger_start_time <- Sys.time())

# Compute Bayes Factor anova for the Anger scores using the custom function 'compute_bf_anova'
anger_bf_anova <- compute_bf_anova(df_long, "DEQ_anger_total")

print(anger_execution_time <- Sys.time() - anger_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(anger_bf_anova, "data/secondary_analysis/smile25b_anger_bf_anova.Rds")
anger_bf_anova <- readRDS("data/secondary_analysis/smile25b_anger_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
anger_bf_anova_table <- extract_bf_anova(anger_bf_anova)

print(anger_bf_anova_table)

# Combine frequentist and bayesian anova tables
anger_combined_anova <- anger_freq_anova %>%
  left_join(anger_bf_anova_table, by = c("term" = "term_dropped"))

print(anger_combined_anova)

# Highlight the pre-registered two-way interactions
anger_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
anger_simple_effects <- compute_bf_simple_effects(
  df_wide = df,
  outcome = "DEQ_anger_total"
)

print(anger_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
anger_stat_summary <- anger_emm_summary %>%
  left_join(
    anger_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of inferential statistics of the pose differences conditioned by context, threat, and repetition
print(anger_stat_summary)

# Save anger analysis results
saveRDS(
  anger_stat_summary,
  "data/secondary_analysis/smile25b_anger_results_simple.Rds"
)

combined_plot <- plot_grid(
  SWL_plot, Burnout_plot,
  fear_plot, anger_plot,
  labels = c("a)", "b)", "c)", "d)"),
  label_x = 0.05,
  ncol = 2
)

print(combined_plot)

ggsave(
  "figures/smile25b_combined_secondary_plot.jpg",
  plot = combined_plot,
  width = 14, height = 10, dpi = 300
)
