# Load libraries
library(tidyverse)

# List OpenFace output folders
openface_output_list <- Sys.glob(
    file.path("data", "gorilla_survey", "gorilla-v*-p*", "OpenFace_output", "*.csv")
)

# Calculate mean values for face detection, scalar activation of AU12, and binary activation of AU12
openface_results <- lapply(
    openface_output_list, function(openface_output_file) {
        df <- read.csv(
            openface_output_file
        ) |>
            summarise(
                success = mean(success),
                AU12_r = mean(AU12_r),
                AU12_c = mean(AU12_c)
            ) |>
            mutate(
                videoID = sub(
                    ".*/(.*)_processed\\.csv",
                    "\\1",
                    openface_output_file
                )
            )

        return(df)
    }
) |>
    bind_rows()

# Break down video ID into distinct information blocks
openface_results <- openface_results |>
    separate(videoID,
        into = c(
            "Experiment_ID",
            "Experiment_Version",
            "Gorilla_ID",
            "task_pre",
            "task",
            "schedule_ID",
            "recording",
            "rep_number",
            "screen_counter"
        )
    ) |>
    select(-c(Experiment_ID, Experiment_Version, task_pre, schedule_ID, recording, screen_counter))

# Define task category based on the gorilla block code
smile_task <- c("s1vq", "21bf", "k1jv", "2dqb", "zk4a", "dbv3", "v86q", "6966")

natura_task <- c("w23m", "9mm1", "1a4k", "irdf", "udup", "kluz", "45jz", "x7ha")

fill1_task <- c("e2jx")

fill4_task <- c("44y9")

# Set task based on gorilla block categorization
openface_results <- openface_results |>
    mutate(face = case_when(
        task %in% smile_task ~ "smile",
        task %in% natura_task ~ "natura",
        task %in% fill1_task ~ "fill1",
        task %in% fill4_task ~ "fill4",
        TRUE ~ NA_character_
    ))

# Calculate AU12 activation mean score across video frames
openface_results_summ <- openface_results |>
    # Group by ID and facial expression
    group_by(Gorilla_ID, face) |>
    # Remove filler conditions
    filter(face %in% c("natura", "smile")) |>
    # Calculate mean scores
    summarise(
        face_detection = mean(success),
        AU12_scalar = mean(AU12_r),
        AU12_binary = mean(AU12_c)
    ) |>
    # Convert dataframe to wide format
    pivot_wider(
        id_cols = Gorilla_ID,
        names_from = face,
        values_from = c(face_detection, AU12_scalar, AU12_binary),
        names_glue = "{.value}_{face}"
    ) |>
    # calculate compliance indices based on face detetion sucess and AU12 activation scores
    mutate(
        face_compliance_scalar = ifelse(
            (face_detection_natura > 0.8 &
                face_detection_smile > 0.8 &
                AU12_scalar_smile - AU12_scalar_natura >= 1.5),
            TRUE,
            FALSE
        ),
        face_compliance_binary = ifelse(
            (face_detection_natura > 0.8 &
                face_detection_smile > 0.8 &
                AU12_binary_natura < 0.5 &
                AU12_binary_smile >= 0.5),
            TRUE,
            FALSE
        )
    )

# List the IDs of the participants who did not consent to sharing their data.
# This script runs independently of smile25b_process_data.R, so the same filter
# is applied here to keep non-consenting participants out of the shared file.
questionnaire_list <- Sys.glob(
    file.path("data", "gorilla_survey", "gorilla-v*-p*", "data_exp_262549-v*_questionnaires.csv")
)

no_consent_list <- lapply(
    questionnaire_list, function(questionnaire_csv) {
        read_csv(questionnaire_csv, show_col_types = FALSE) |>
            filter(
                `Data_consent object-14 Response` ==
                    "No, I do not want my data used in the research."
            ) |>
            select(`Participant Private ID`)
    }
) |>
    bind_rows() |>
    pull(`Participant Private ID`) |>
    unique() |>
    as.character()

# Remove participants who did not consent to data sharing
openface_results_summ <- openface_results_summ |>
    filter(!(Gorilla_ID %in% no_consent_list))

# Check openface compliance distribution
# Scalar (1.5 out 5 difference in smile vs natural conditions)
table(openface_results_summ$face_compliance_scalar)

# Binary categorization (True vs False)
table(openface_results_summ$face_compliance_binary)

# Save openface expression compiled data
write_csv(openface_results_summ, "data/smile25b_openface_processed.csv")
