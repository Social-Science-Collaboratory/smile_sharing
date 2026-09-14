# Preliminary data analysis

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

# Descriptive statistics

# Number of participants
print(paste0("Number of participants: ", nrow(df)))

## Calculate age mean and standard deviation
print(paste0("Mean age: ", round(mean(df$Age, na.rm = TRUE), 2), "; sd: ", round(sd(df$Age, na.rm = TRUE), 2)))

## Calculate percentage of female participants
print(paste0(
  "Percentage female: ",
  round(
    nrow(df %>% filter(Gender == "Female")) / nrow(df) * 100,
    2
  ),
  "%"
))

## Ethinicity distribution
table(df$Ethnicity)

# Calculate mean and standard deviation of smile activation unit 12 across pose conditions
print(
  paste0(
    "AU 12 activation scores (Natural pose): Mean = ",
    round(mean(df$AU12_scalar_natura, na.rm = TRUE), 2),
    "/10 SD = ",
    round(sd(df$AU12_scalar_natura, na.rm = TRUE), 2)
  )
)

print(
  paste0(
    "Mean AU 12 activation scores (Smiling pose): Mean = ",
    round(mean(df$AU12_scalar_smile, na.rm = TRUE), 2),
    "/10 SD = ",
    round(sd(df$AU12_scalar_smile, na.rm = TRUE), 2)
  )
)

# Calculate mean and standard deviation of happiness scores across pose conditions
print(
  paste0(
    "Happiness scores (Natural pose): Mean = ",
    round(mean(df$NP_DEQ_happy_total, na.rm = TRUE), 2),
    "/10 SD = ",
    round(sd(df$NP_DEQ_happy_total, na.rm = TRUE), 2)
  )
)

print(
  paste0(
    "Happiness  scores (Smiling pose): Mean = ",
    round(mean(df$SP_DEQ_happy_total, na.rm = TRUE), 2),
    "/10 SD = ",
    round(sd(df$SP_DEQ_happy_total, na.rm = TRUE), 2)
  )
)

# Prepare dataframe for data analysis
df_long <- df %>%
  ## create a unique participant id
  mutate(id = row_number()) %>%
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, SP_DEQ_fear_total, SP_DEQ_anger_total,
    SP_SWL_total, SP_Burnout_total,
    NP_DEQ_happy_total, NP_DEQ_fear_total, NP_DEQ_anger_total,
    NP_SWL_total, NP_Burnout_total
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

# Main analysis: Outcome = happiness, dataset = full

# Full factorial general linear mixed model of happiness predicted by pose, context, threat, and repetition
happy_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

summary(happy_model)

happy_freq_anova <- as.data.frame(anova(happy_model)) %>%
  tibble::rownames_to_column("term")

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_emm <- emmeans(happy_model, ~ pose | context + threat + repetition)

summary(happy_emm)

# Extract pairwise comparisons of pose across the other conditions
happy_emm_pairs <- as.data.frame(pairs(happy_emm))

summary(happy_emm_pairs)

# Calculate the effect size of the pose difference
happy_emm_effect_size <- as.data.frame(
  eff_size(
    happy_emm,
    sigma = sigma(happy_model),
    edf = df.residual(happy_model)
  )
)

# Add effect size estimates to the emm summary
happy_emm_summary <- happy_emm_pairs %>%
  left_join(
    happy_emm_effect_size %>%
      select(context, threat, repetition, effect.size),
    by = c("context", "threat", "repetition")
  )

summary(happy_emm_summary)

# Draw the happiness score with the custom function 'draw_plot'
happy_plot <- draw_plot(
  df_wide = df,
  outcome = "DEQ_happy_total",
  outcome_label = "happiness",
  legend_position = "top_right",
  x_axis = TRUE,
  y_axis = TRUE,
  y_text = "Δ Happiness Reports",
  y_breaks = c(-1.5, -1, -.5, 0, .5)
)

print(happy_plot)

# Save figure
ggsave(
  "figures/smile25b_happy_plot.jpg",
  plot = happy_plot,
  width = 12, height = 8, dpi = 300
)

# Bayesian Analysis of happiness score outcomes

happy_start_time <- Sys.time()

# Compute Bayes Factor anova for the DEQ Happiness scores with the custom function 'compute_bf_anova'
happy_bf_anova <- compute_bf_anova(
  df_long = df_long,
  outcome = "DEQ_happy_total"
)

print(happy_execution_time <- Sys.time() - happy_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(happy_bf_anova, "data/main_analysis/smile25b_happy_bf_anova.Rds")
happy_bf_anova <- readRDS("data/main_analysis/smile25b_happy_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
happy_bf_anova_table <- extract_bf_anova(
  outcome_bf_anova = happy_bf_anova
)

print(happy_bf_anova_table)

# Combine frequentist and bayesian anova tables

happy_combined_anova <- happy_freq_anova %>%
  left_join(happy_bf_anova_table, by = c("term" = "term_dropped"))

# Save combined Anova table for results section
saveRDS(
  happy_combined_anova,
  "data/main_analysis/smile25b_happy_results_anova.Rds"
)

# Highlight the pre-registered two-way interactions
happy_combined_anova %>%
  filter(term %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects with the custom function 'compute_bf_simple_effects'
happy_simple_effects <- compute_bf_simple_effects(
  df_wide = df,
  outcome = "DEQ_happy_total"
)

print(happy_simple_effects)

# Add the Bayes Factors to the statistics summary dataframe
happy_stat_summary <- happy_emm_summary %>%
  left_join(
    happy_simple_effects %>%
      select(context, threat, repetition, BF10),
    by = c("context", "threat", "repetition")
  )

# Summary of inferential statistics of the pose differences conditioned by context, threat, and repetition
print(happy_stat_summary)

saveRDS(
  happy_stat_summary,
  "data/main_analysis/smile25b_happy_results_simple.Rds"
)
