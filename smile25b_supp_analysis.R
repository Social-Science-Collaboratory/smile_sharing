# Supplemental analysis: sensitivity checks reported in the supplementary information

# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)
library(ggh4x)
library(see)
library(patchwork)

# Source functions used in the Bayesian analyses and figure generation
source("smile25b_functions.R")

# Read processed data set
df <- read_csv("data/smile25b_processed_data.csv")

# Build a panel title of the form "A. Label (n = 123)", with n italicised
panel_title <- function(label, n) {
  bquote(.(label) ~ "(" * italic(n) ~ "=" ~ .(format(n, big.mark = ",")) * ")")
}

# Sensitivity analysis: Outcome = happiness, dataset = compliant responses only (Hard sensitivity check)

# Compliance check based on AU12 activation difference (delta = 1.5 out of 5) in smile vs natual condition via OpenFace
table(df$face_compliance_scalar)

# Check the distribution of math errors
table(df$math_errors)

# Check participants who were aware of the true purpose of the study
table(df$joint_awareness_check)

# Prepare data for sensitivity analysis
df_sens_wide <- df %>%
  filter(
    # Filter participants who complied with facial expression instructions
    face_compliance_scalar == TRUE,
    # With no math errors
    math_errors == 0,
    # Who were not aware of the study's true purpose
    joint_awareness_check == FALSE
  )

# Check number of cases that meet the sensitivity filter
nrow(df_sens_wide)

# Look at the number of participants in each condition
table(df_sens_wide$context, df_sens_wide$repetition, df_sens_wide$threat)

# Convert to long format
df_sens_long <- df_sens_wide %>%
  ## create a unique participant id
  mutate(id = row_number()) %>%
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, NP_DEQ_happy_total
  ) %>%
  ## Pivot smile and natural pose columns into long format
  pivot_longer(
    cols = c(starts_with("SP_"), starts_with("NP_")),
    names_to = c("pose", ".value"),
    names_pattern = "^(SP|NP)_(.*)$"
  ) %>%
  ## recode pose variable into readable labels
  mutate(pose = factor(
    case_when(
      pose == "SP" ~ "smile",
      pose == "NP" ~ "natural",
      TRUE ~ pose
    ),
    levels = c("smile", "natural")
  )) %>%
  relocate(pose, .after = id)

# Full factorial general linear mixed model of happiness predicted by pose, context, threat, and repetition
happy_sens_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_sens_long
)

happy_sens_freq_anova <- as.data.frame(anova(happy_sens_model)) %>%
  tibble::rownames_to_column("term")

# print model coefficient estimates
print(summary(happy_sens_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_sens_emm <- emmeans(happy_sens_model, ~ pose | context + threat + repetition)

summary(happy_sens_emm)

# Extract pairwise comparisons of pose across the other conditions
happy_sens_emm_pairs <- as.data.frame(pairs(happy_sens_emm))

summary(happy_sens_emm_pairs)

# Calculate the effect size of the pose difference
happy_sens_emm_effect_size <- as.data.frame(
  eff_size(
    happy_sens_emm,
    sigma = sigma(happy_sens_model),
    edf = df.residual(happy_sens_model)
  )
)

# Add effect size estimates to the emm summary
happy_sens_emm_summary <- happy_sens_emm_pairs %>%
  left_join(
    happy_sens_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(happy_sens_emm_summary)

# Draw the happiness score with the custom function 'draw_plot' (Panel C: strong smile required)
panel_c <- draw_plot(
  df_wide = df_sens_wide,
  outcome = "DEQ_happy_total",
  outcome_label = "happiness",
  legend_position = "top_right",
  x_axis = TRUE,
  y_axis = FALSE,
  y_text = "Δ Happiness Reports",
  y_breaks = c(-1.5, -1, -.5, 0, .5, 1)
) +
  labs(
    title = panel_title("C. Sensitivity: Strong smile required", nrow(df_sens_wide)),
    y = NULL
  ) +
  theme(
    axis.line.y  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

# Hard sensitivity check Bayesian analysis

happy_sens_start_time <- Sys.time()

# Compute Bayes Factor anova for the DEQ Happiness scores with the custom function 'compute_bf_anova'
happy_sens_bf_anova <- compute_bf_anova(df_sens_long, "DEQ_happy_total")

print(happy_sens_execution_time <- Sys.time() - happy_sens_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(happy_sens_bf_anova, "data/main_analysis/smile25b_happy_sens_bf_anova.Rds")
happy_sens_bf_anova <- readRDS("data/main_analysis/smile25b_happy_sens_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
happy_sens_bf_anova_table <- extract_bf_anova(happy_sens_bf_anova)

print(happy_sens_bf_anova_table)

# Combine frequentist and bayesian anova tables

happy_sens_combined_anova <- happy_sens_freq_anova %>%
  left_join(happy_sens_bf_anova_table, by = c("term" = "term_dropped"))

print(happy_sens_combined_anova)

# Save combined Anova table for results section
saveRDS(
  happy_sens_combined_anova,
  "data/main_analysis/smile25b_happy_sens_results_anova.Rds"
)

# Highlight the pre-registered two-way interactions
happy_sens_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
happy_sens_simple_effects <- compute_bf_simple_effects(df_sens_wide, "DEQ_happy_total")

print(happy_sens_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
happy_sens_stat_summary <- happy_sens_emm_summary %>%
  left_join(
    happy_sens_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of simple effect statistics of the pose differences conditioned by context, threat, and repetition
print(happy_sens_stat_summary)

saveRDS(
  happy_sens_stat_summary,
  "data/main_analysis/smile25b_happy_sens_results_simple.Rds"
)

## Sensitivity analysis with less stringent scalar coding (Soft sensitivity check)

# Prepare data for sensitivity analysis
df_sens_soft_wide <- df %>%
  filter(
    # Filter participants who complied with facial expression instructions
    AU12_scalar_smile - AU12_scalar_natura >= 0.5,
    face_detection_natura >= 0.8 & face_detection_smile >= 0.8,
    # With no math errors
    math_errors == 0,
    # Who were not aware of the study's true purpose
    joint_awareness_check == FALSE
  )

# Check number of cases that meet the sensitivity filter
nrow(df_sens_soft_wide)

# Look at the number of participants in each condition
table(df_sens_soft_wide$context, df_sens_soft_wide$repetition, df_sens_soft_wide$threat)

# Convert to long format
df_sens_soft_long <- df_sens_soft_wide %>%
  ## create a unique participant id
  mutate(id = row_number()) %>%
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, NP_DEQ_happy_total
  ) %>%
  ## Pivot smile and natural pose columns into long format
  pivot_longer(
    cols = c(starts_with("SP_"), starts_with("NP_")),
    names_to = c("pose", ".value"),
    names_pattern = "^(SP|NP)_(.*)$"
  ) %>%
  ## recode pose variable into readable labels
  mutate(pose = factor(
    case_when(
      pose == "SP" ~ "smile",
      pose == "NP" ~ "natural",
      TRUE ~ pose
    ),
    levels = c("smile", "natural")
  )) %>%
  relocate(pose, .after = id)

# Full factorial general linear mixed model of happiness predicted by pose, context, threat, and repetition
happy_sens_soft_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_sens_soft_long
)

happy_sens_soft_freq_anova <- as.data.frame(anova(happy_sens_soft_model)) %>%
  tibble::rownames_to_column("term")

# print model coefficient estimates
print(summary(happy_sens_soft_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_sens_soft_emm <- emmeans(happy_sens_soft_model, ~ pose | context + threat + repetition)

summary(happy_sens_soft_emm)

# Extract pairwise comparisons of pose across the other conditions
happy_sens_soft_emm_pairs <- as.data.frame(pairs(happy_sens_soft_emm))

summary(happy_sens_soft_emm_pairs)

# Calculate the effect size of the pose difference
happy_sens_soft_emm_effect_size <- as.data.frame(
  eff_size(
    happy_sens_soft_emm,
    sigma = sigma(happy_sens_soft_model),
    edf = df.residual(happy_sens_soft_model)
  )
)

# Add effect size estimates to the emm summary
happy_sens_soft_emm_summary <- happy_sens_soft_emm_pairs %>%
  left_join(
    happy_sens_soft_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(happy_sens_soft_emm_summary)

# Draw the happiness score with the custom function 'draw_plot' (Panel B: weak smile required)
panel_b <- draw_plot(
  df_wide = df_sens_soft_wide,
  outcome = "DEQ_happy_total",
  outcome_label = "happiness",
  legend_position = "none",
  x_axis = TRUE,
  y_axis = FALSE,
  y_text = "Δ Happiness Reports",
  y_breaks = c(-1.5, -1, -.5, 0, .5, 1)
) +
  labs(
    title = panel_title("B. Sensitivity: Weak smile required", nrow(df_sens_soft_wide)),
    y = NULL
  ) +
  theme(
    axis.line.y  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

# Draw the full sample with the custom function 'draw_plot' (Panel A), on the
# same y-axis breaks as the two sensitivity panels so the row is comparable
panel_a <- draw_plot(
  df_wide = df,
  outcome = "DEQ_happy_total",
  outcome_label = "happiness",
  legend_position = "none",
  x_axis = TRUE,
  y_axis = TRUE,
  y_text = "Δ Happiness Reports",
  y_breaks = c(-1.5, -1, -.5, 0, .5, 1)
) +
  labs(title = panel_title("A. Full sample", nrow(df)))

# Combine the full sample (Panel A) with the soft (Panel B) and hard (Panel C)
# sensitivity checks into one figure, sharing a single y-axis across one row
sens_plot <- panel_a + panel_b + panel_c + plot_layout(nrow = 1)

print(sens_plot)

# Save combined sensitivity plot to figures folder
ggsave(
  "figures/smile25b_sens_plot.jpg",
  plot = sens_plot,
  width = 24, height = 8, dpi = 300
)

# Soft sensitivity check Bayesian analysis

print(happy_sens_soft_start_time <- Sys.time())

# Compute Bayes Factor anova for the DEQ Happiness scores with the custom function 'compute_bf_anova'
happy_sens_soft_bf_anova <- compute_bf_anova(df_sens_soft_long, "DEQ_happy_total")

print(happy_sens_soft_execution_time <- Sys.time() - happy_sens_soft_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(happy_sens_soft_bf_anova, "data/main_analysis/smile25b_happy_sens_soft_bf_anova.Rds")
happy_sens_soft_bf_anova <- readRDS("data/main_analysis/smile25b_happy_sens_soft_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
happy_sens_soft_bf_anova_table <- extract_bf_anova(happy_sens_soft_bf_anova)

print(happy_sens_soft_bf_anova_table)

# Combine frequentist and bayesian anova tables

happy_sens_soft_combined_anova <- happy_sens_soft_freq_anova %>%
  left_join(happy_sens_soft_bf_anova_table, by = c("term" = "term_dropped"))

# Save combined Anova table for results section
saveRDS(
  happy_sens_soft_combined_anova,
  "data/main_analysis/smile25b_happy_sens_soft_results_anova.Rds"
)

# Highlight the pre-registered two-way interactions
happy_sens_soft_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
happy_sens_soft_simple_effects <- compute_bf_simple_effects(df_sens_soft_wide, "DEQ_happy_total")

print(happy_sens_soft_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
happy_sens_soft_stat_summary <- happy_sens_soft_emm_summary %>%
  left_join(
    happy_sens_soft_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of inferential statistics of the pose differences conditioned by context, threat, and repetition
print(happy_sens_soft_stat_summary)

saveRDS(
  happy_sens_soft_stat_summary,
  "data/main_analysis/smile25b_happy_sens_soft_results_simple.Rds"
)
