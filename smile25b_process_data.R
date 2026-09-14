# Load libraries
library(tidyverse)

# Initial processing:
# - compile questionnaires
# - remove participants who did not consent to data sharing
# - Merge Prolific demographic data
# - remove identifying information (Prolific participant and submission IDs)

# List gorilla questionnaire .csv files across the data batch folders
questionnaire_list <- Sys.glob(
  file.path("data", "gorilla_survey", "gorilla-v*-p*", "data_exp_262549-v*_questionnaires.csv")
)

# Read the questionaire list, remove header rows, and bind tables into a single dataframe
df <- lapply(
  questionnaire_list, function(questionnaire_csv) {
    df <- read_csv(questionnaire_csv) %>%
      # Remove rows with no participant ID information
      filter(!is.na(`Participant Private ID`))
    return(df)
  }
) %>%
  bind_rows()

## Check participants who did not consent to participate in the study
table(df$`ConsentForm object-8 Response`)

## Check participants who did not consent to sharing their data after study disclosure
table(df$`Data_consent object-14 Response`)

## List the IDs of the participants who did not consent to sharing their data
no_consent_list <- df %>%
  filter(`Data_consent object-14 Response` == "No, I do not want my data used in the research.") %>%
  select(`Participant Private ID`) %>%
  rename(Gorilla_ID = `Participant Private ID`)

## Remove all rows with non-consenting participant IDs
df <- df %>%
  filter(!(`Participant Private ID` %in% no_consent_list$Gorilla_ID))

## Pull demographic data from Prolific
demo_data <- read_csv(
  file.path("data", "gorilla_survey", "smile25b_prolific_demographic.csv")
) %>%
  filter(Status == "APPROVED") %>%
  select(`Participant id`, `Ethnicity simplified`) %>%
  rename(
    Prolific_ID = `Participant id`,
    Ethnicity = `Ethnicity simplified`
  ) %>%
  mutate(Ethnicity = ifelse(
    Ethnicity == "DATA_EXPIRED",
    NA, Ethnicity
  ))

# Rename Prolific ID column to match demographic data
df <- df %>%
  rename(Prolific_ID = `Participant Public ID`)

# Join prolific demographic data into dataframe
df <- left_join(df, demo_data, by = "Prolific_ID")

## Remove Prolific ID information from dataframe
df <- df %>%
  select(-`Prolific_ID`)

## Remove the Prolific submission ID, which also links back to participants
df <- df %>%
  select(-`Participant External Session ID`)

# Clean workspace
rm(no_consent_list, demo_data)

# Save raw data
write_csv(df, "data/smile25b_raw_data.csv")

# Optionally reload raw data
df <- read_csv("data/smile25b_raw_data.csv")

# Select variables of interest
df <- df %>%
  select(
    `Local Date and Time`,
    `Experiment Version`,
    `Participant Private ID`,
    `Participant Status`,
    `randomiser-7z4z`,
    `randomiser-e9dd`,
    `ConsentForm object-8 Response`,
    `Fill1pos_math1 object-2 Value`:`Multiple Choice object-21 Response`,
    Ethnicity
  )

# Rename metadata and filler condition variables
df <- df %>%
  rename(
    DateTime = `Local Date and Time`,
    ExperimentVersion = `Experiment Version`,
    Gorilla_ID = `Participant Private ID`,
    Status = `Participant Status`,
    threat = `randomiser-7z4z`,
    group = `randomiser-e9dd`,
    consent = `ConsentForm object-8 Response`,
    Fill1pos_math1 = `Fill1pos_math1 object-2 Value`,
    Fill1pos_DEQ_happiness = `Fill1pos_DEQ object-6 happiness\n`,
    Fill1pos_DEQ_satisfaction = `Fill1pos_DEQ object-6 satisfaction\n`,
    Fill1pos_DEQ_enjoyment = `Fill1pos_DEQ object-6 enjoyment\n`,
    Fill1pos_DEQ_alarmed = `Fill1pos_DEQ object-6 alarmed\n`,
    Fill1pos_DEQ_scared = `Fill1pos_DEQ object-6 scared\n`,
    Fill1pos_DEQ_fear = `Fill1pos_DEQ object-6 fear\n`,
    Fill1pos_DEQ_irritation = `Fill1pos_DEQ object-6 irritation\n`,
    Fill1pos_DEQ_aggravation = `Fill1pos_DEQ object-6 aggravation\n`,
    Fill1pos_DEQ_annoyance = `Fill1pos_DEQ object-6 annoyance\n`,
    Fill1pos_SWL_ideal = `Fill1pos_SWL object-9 In most ways my life is close to my ideal\n`,
    Fill1pos_SWL_condition = `Fill1pos_SWL object-9 The conditions of my life are excellent\n`,
    Fill1pos_SWL_satisfied = `Fill1pos_SWL object-9 I am satisfied with my life\n`,
    Fill1pos_SWL_change = `Fill1pos_SWL object-9 If I could live my life over, I would change almost nothing\n`,
    Fill1pos_Burnout_tired = `Fill1pos_Burnout object-14 tired\n`,
    Fill1pos_Burnout_hopeless = `Fill1pos_Burnout object-14 hopeless\n`,
    Fill1pos_Burnout_worthless = `Fill1pos_Burnout object-14 worthless\n`,
    Fill1pos_Burnout_disappointed = `Fill1pos_Burnout object-14 disappointed\n`,
    Fill1pos_Burnout_depressed = `Fill1pos_Burnout object-14 depressed\n`,
    Fill1pos_math2 = `Fill1pos_math2 object-211 Value`,
    Fill1pos_math3 = `Fill1pos_math3 object-22 Value`,
    Fill1pos_math4 = `Fill1pos_math4 object-23 Value`,
    Fill4pos_math1 = `Fill4pos_math1 object-2 Value`,
    Fill4pos_DEQ_happiness = `Fill4pos_DEQ object-6 happiness\n`,
    Fill4pos_DEQ_satisfaction = `Fill4pos_DEQ object-6 satisfaction\n`,
    Fill4pos_DEQ_enjoyment = `Fill4pos_DEQ object-6 enjoyment\n`,
    Fill4pos_DEQ_alarmed = `Fill4pos_DEQ object-6 alarmed\n`,
    Fill4pos_DEQ_scared = `Fill4pos_DEQ object-6 scared\n`,
    Fill4pos_DEQ_fear = `Fill4pos_DEQ object-6 fear\n`,
    Fill4pos_DEQ_irritation = `Fill4pos_DEQ object-6 irritation\n`,
    Fill4pos_DEQ_aggravation = `Fill4pos_DEQ object-6 aggravation\n`,
    Fill4pos_DEQ_annoyance = `Fill4pos_DEQ object-6 annoyance\n`,
    Fill4pos_SWL_ideal = `Fill4pos_SWL object-9 In most ways my life is close to my ideal\n`,
    Fill4pos_SWL_condition = `Fill4pos_SWL object-9 The conditions of my life are excellent\n`,
    Fill4pos_SWL_satisfied = `Fill4pos_SWL object-9 I am satisfied with my life\n`,
    Fill4pos_SWL_change = `Fill4pos_SWL object-9 If I could live my life over, I would change almost nothing\n`,
    Fill4pos_Burnout_tired = `Fill4pos_Burnout object-14 tired\n`,
    Fill4pos_Burnout_hopeless = `Fill4pos_Burnout object-14 hopeless\n`,
    Fill4pos_Burnout_worthless = `Fill4pos_Burnout object-14 worthless\n`,
    Fill4pos_Burnout_disappointed = `Fill4pos_Burnout object-14 disappointed\n`,
    Fill4pos_Burnout_depressed = `Fill4pos_Burnout object-14 depressed\n`,
    Fill4pos_math2 = `Fill4pos_math2 object-211 Value`,
    Fill4pos_math3 = `Fill4pos_math3 object-22 Value`,
    Fill4pos_math4 = `Fill4pos_math4 object-23 Value`,
    Purpose = `Purpose object-2 Value`,
    Concealment = `Concealment object-5 Value`,
    Age = `Age object-7 Value`,
    Gender = `Gender object-8 Response`,
    Quality_issues = `Quality_issues object-10 Value`,
    Data_consent = `Data_consent object-14 Response`,
    Display_check = `Multiple Choice object-21 Response`
  )

# Outcome variable data is spread across different Gorilla task blocks due to the randomization/branching pipeline
# Use coalesce (extract the first non-NA value) across matching blocks to obtain the outcome value
df <- df %>%
  mutate(
    # Smile pose condition
    SP_math1 = reduce(across(matches("SP_math1 object-2 .*")), coalesce),
    SP_DEQ_happiness = reduce(across(matches("SP_DEQ object-6 happiness.*Quantised.*")), coalesce),
    SP_DEQ_satisfaction = reduce(across(matches("SP_DEQ object-6 satisfaction.*Quantised.*")), coalesce),
    SP_DEQ_enjoyment = reduce(across(matches("SP_DEQ object-6 enjoyment.*Quantised.*")), coalesce),
    SP_DEQ_alarmed = reduce(across(matches("SP_DEQ object-6 alarmed.*Quantised.*")), coalesce),
    SP_DEQ_scared = reduce(across(matches("SP_DEQ object-6 scared.*Quantised.*")), coalesce),
    SP_DEQ_fear = reduce(across(matches("SP_DEQ object-6 fear.*Quantised.*")), coalesce),
    SP_DEQ_irritation = reduce(across(matches("SP_DEQ object-6 irritation.*Quantised.*")), coalesce),
    SP_DEQ_aggravation = reduce(across(matches("SP_DEQ object-6 aggravation.*Quantised.*")), coalesce),
    SP_DEQ_annoyance = reduce(across(matches("SP_DEQ object-6 annoyance.*Quantised.*")), coalesce),
    SP_SWL_ideal = reduce(across(matches("SP_SWL object-9 In most ways my life is close to my ideal.*Quantised.*")), coalesce),
    SP_SWL_condition = reduce(across(matches("SP_SWL object-9 The conditions of my life are excellent.*Quantised.*")), coalesce),
    SP_SWL_satisfied = reduce(across(matches("SP_SWL object-9 I am satisfied with my life.*Quantised.*")), coalesce),
    SP_SWL_change = reduce(across(matches("SP_SWL object-9 If I could live my life over.*Quantised.*")), coalesce),
    SP_Burnout_tired = reduce(across(matches("SP_Burnout object-14 tired.*Quantised.*")), coalesce),
    SP_Burnout_hopeless = reduce(across(matches("SP_Burnout object-14 hopeless.*Quantised.*")), coalesce),
    SP_Burnout_worthless = reduce(across(matches("SP_Burnout object-14 worthless.*Quantised.*")), coalesce),
    SP_Burnout_disappointed = reduce(across(matches("SP_Burnout object-14 disappointed.*Quantised.*")), coalesce),
    SP_Burnout_depressed = reduce(across(matches("SP_Burnout object-14 depressed.*Quantised.*")), coalesce),
    SP_math2 = reduce(across(matches("SP_math2 object-211.*")), coalesce),
    SP_math3 = reduce(across(matches("SP_math3 object-22.*")), coalesce),
    SP_math4 = reduce(across(matches("SP_math4 object-23.*")), coalesce),

    # Natural pose condition
    NP_math1 = reduce(across(matches("NP_math1 object-2 .*")), coalesce),
    NP_DEQ_happiness = reduce(across(matches("NP_DEQ object-6 happiness.*Quantised.*")), coalesce),
    NP_DEQ_satisfaction = reduce(across(matches("NP_DEQ object-6 satisfaction.*Quantised.*")), coalesce),
    NP_DEQ_enjoyment = reduce(across(matches("NP_DEQ object-6 enjoyment.*Quantised.*")), coalesce),
    NP_DEQ_alarmed = reduce(across(matches("NP_DEQ object-6 alarmed.*Quantised.*")), coalesce),
    NP_DEQ_scared = reduce(across(matches("NP_DEQ object-6 scared.*Quantised.*")), coalesce),
    NP_DEQ_fear = reduce(across(matches("NP_DEQ object-6 fear.*Quantised.*")), coalesce),
    NP_DEQ_irritation = reduce(across(matches("NP_DEQ object-6 irritation.*Quantised.*")), coalesce),
    NP_DEQ_aggravation = reduce(across(matches("NP_DEQ object-6 aggravation.*Quantised.*")), coalesce),
    NP_DEQ_annoyance = reduce(across(matches("NP_DEQ object-6 annoyance.*Quantised.*")), coalesce),
    NP_SWL_ideal = reduce(across(matches("NP_SWL object-9 In most ways my life is close to my ideal.*Quantised.*")), coalesce),
    NP_SWL_condition = reduce(across(matches("NP_SWL object-9 The conditions of my life are excellent.*Quantised.*")), coalesce),
    NP_SWL_satisfied = reduce(across(matches("NP_SWL object-9 I am satisfied with my life.*Quantised.*")), coalesce),
    NP_SWL_change = reduce(across(matches("NP_SWL object-9 If I could live my life over.*Quantised.*")), coalesce),
    NP_Burnout_tired = reduce(across(matches("NP_Burnout object-14 tired.*Quantised.*")), coalesce),
    NP_Burnout_hopeless = reduce(across(matches("NP_Burnout object-14 hopeless.*Quantised.*")), coalesce),
    NP_Burnout_worthless = reduce(across(matches("NP_Burnout object-14 worthless.*Quantised.*")), coalesce),
    NP_Burnout_disappointed = reduce(across(matches("NP_Burnout object-14 disappointed.*Quantised.*")), coalesce),
    NP_Burnout_depressed = reduce(across(matches("NP_Burnout object-14 depressed.*Quantised.*")), coalesce),
    NP_math2 = reduce(across(matches("NP_math2 object-211.*")), coalesce),
    NP_math3 = reduce(across(matches("NP_math3 object-22.*")), coalesce),
    NP_math4 = reduce(across(matches("NP_math4 object-23.*")), coalesce)
  ) %>%
  # Remove the randomization/branching block unprocessed columns
  select(-matches(".*object.*"))

# Gorilla experiment output data produces a different row for each task/questionnaire block the participant completes.
# Collapse each participant's data into a single row by extracting the only non-NA value per Gorilla_ID grouping for each variable
df <- df %>%
  group_by(Gorilla_ID) %>%
  summarise(
    across(where(is.character), ~ first(na.omit(na_if(.x, "")))),
    across(where(is.numeric), ~ first(na.omit(.x))),
    .groups = "drop"
  )

# Process the manipulation conditions, variable order, and variable type
df <- df %>%
  # Extract the context and repetition condition from the group randomization label
  mutate(
    # Check if the group label contains the "Pos" string
    context = if_else(
      str_detect(
        group, "Pos"
      ),
      "positive", "negative"
    ),
    # Check if the group label contains the "One" string
    repetition = if_else(
      str_detect(
        group, "One"
      ),
      "one", "ten"
    )
  ) %>%
  # Reorder variables in the dataframe
  relocate(context, repetition,
    .after = group
  ) %>%
  relocate(Purpose, Concealment, Age, Gender, Ethnicity, Quality_issues, Data_consent, Display_check,
    .after = last_col()
  ) %>%
  # Convert outcome variables to numeric
  mutate(across(Fill1pos_math1:NP_math4, as.numeric))


# Create general function for calculating composite scale scores
scale_mean <- function(df, cols) rowMeans(select(df, all_of(cols)), na.rm = TRUE)

# Set task prefixes
task_prefixes <- c("Fill1pos", "NP", "SP", "Fill4pos")

# Set DEQ subscale item names
deq_happy_items <- c("DEQ_happiness", "DEQ_satisfaction", "DEQ_enjoyment")
deq_fear_items <- c("DEQ_alarmed", "DEQ_scared", "DEQ_fear")
deq_anger_items <- c("DEQ_irritation", "DEQ_aggravation", "DEQ_annoyance")

# Compute scale scores for each task/DEQ subscale and position composite score after the last item
for (pfx in task_prefixes) {
  happy_cols <- paste0(pfx, "_", deq_happy_items)
  fear_cols <- paste0(pfx, "_", deq_fear_items)
  anger_cols <- paste0(pfx, "_", deq_anger_items)

  if (all(happy_cols %in% names(df))) {
    df[[paste0(pfx, "_DEQ_happy_total")]] <- scale_mean(df, happy_cols)
    df <- relocate(df, all_of(paste0(pfx, "_DEQ_happy_total")), .after = all_of(last(happy_cols)))
  }
  if (all(fear_cols %in% names(df))) {
    df[[paste0(pfx, "_DEQ_fear_total")]] <- scale_mean(df, fear_cols)
    df <- relocate(df, all_of(paste0(pfx, "_DEQ_fear_total")), .after = all_of(last(fear_cols)))
  }
  if (all(anger_cols %in% names(df))) {
    df[[paste0(pfx, "_DEQ_anger_total")]] <- scale_mean(df, anger_cols)
    df <- relocate(df, all_of(paste0(pfx, "_DEQ_anger_total")), .after = all_of(last(anger_cols)))
  }
}

# Set SWL scale item names
swl_items <- c(
  "SWL_ideal",
  "SWL_condition",
  "SWL_satisfied",
  "SWL_change"
)

# Compute SWL score for each task and position composite score after the last item
for (pfx in task_prefixes) {
  cols <- paste0(pfx, "_", swl_items)
  composite_col <- paste0(pfx, "_SWL_total")
  if (all(cols %in% names(df))) {
    df[[composite_col]] <- scale_mean(df, cols)
    df <- relocate(df, all_of(composite_col), .after = all_of(last(cols)))
  }
}

# Set Burnout scale item names
burnout_items <- c(
  "Burnout_tired", "Burnout_hopeless", "Burnout_worthless",
  "Burnout_disappointed", "Burnout_depressed"
)

# Compute Burnout score for each task and position composite score after the last item
for (pfx in task_prefixes) {
  cols <- paste0(pfx, "_", burnout_items)
  composite_col <- paste0(pfx, "_Burnout_total")
  if (all(cols %in% names(df))) {
    df[[composite_col]] <- scale_mean(df, cols)
    df <- relocate(df, all_of(composite_col), .after = all_of(last(cols)))
  }
}

# Clean worksapce
rm(
  task_prefixes, deq_happy_items, deq_anger_items, deq_fear_items,
  anger_cols, fear_cols, happy_cols, swl_items, burnout_items,
  pfx, cols, composite_col
)

# Pull facial expression compliance data from OpenFace processing code
openface_results <- read_csv("data/smile25b_openface_processed.csv") %>%
  mutate(Gorilla_ID = as.numeric(Gorilla_ID))


# Join openface compliance data with main data frame
df <- left_join(df, openface_results, by = "Gorilla_ID")

# Clean workspace
rm(openface_results)

# Check math problem solving accuracy

# Create a list with the correct answers
math_sol <- c(
  Fill1pos_math1 = 4,
  Fill1pos_math2 = 2,
  Fill1pos_math3 = 6,
  Fill1pos_math4 = 4,
  Fill4pos_math1 = 8,
  Fill4pos_math2 = 1,
  Fill4pos_math3 = 9,
  Fill4pos_math4 = 2,
  SP_math1 = 5,
  SP_math2 = 4,
  SP_math3 = 7,
  SP_math4 = 3,
  NP_math1 = 7,
  NP_math2 = 4,
  NP_math3 = 8,
  NP_math4 = 7
)

# Count math errors per participant by comparing each math_sol column to its expected solution
df <- df %>%
  mutate(
    math_errors = rowSums(
      mapply(
        function(col, sol) as.numeric(is.na(df[[col]]) | df[[col]] != sol),
        names(math_sol), math_sol
      )
    )
  )

# Clean workspace
rm(math_sol)

# Prepare dataset for the manual check of study purpose awareness (deception manipulation check)

awareness_df <- df %>%
  select(Gorilla_ID, Purpose, Concealment) %>%
  mutate(awareness_check_R1 = "", awareness_check_R2 = "")

write_csv(awareness_df, "data/smile25b_awareness_check_pending.csv")

# Read completed awarenes check and join to the main data frame

awareness_completed_df <- read_csv("data/smile25b_awareness_check_completed.csv") %>%
  select(Gorilla_ID, joint_awareness_check)

df <- df %>%
  left_join(awareness_completed_df, by = "Gorilla_ID")

# Save processed data file
write_csv(df, "data/smile25b_processed_data.csv")
