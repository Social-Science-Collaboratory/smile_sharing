library(here)

# Paths
proj_dir <- here::here()

# List folders with video recording data from the gorilla survey
gorilla_folder_list <- Sys.glob(
  file.path("data", "gorilla_survey", "gorilla-v*-p*")
)

# Choose data folder for OpenFace facial expression data (set number from 1 to 8)
data_folder <- gorilla_folder_list[[1]]

# Set input, output, and temporary directories for facial expression processing
input_dir <- file.path(data_folder, "uploads")
temp_dir <- file.path(data_folder, "temp_videos")
output_dir <- file.path(data_folder, "OpenFace_output")

# Set ffmpeg path for video processing before OpenFace analysis
# Requires FFmpeg to be installed separately and available on the system PATH.
ffmpeg <- "ffmpeg"

# Set OpenFace FeatureExtraction binary path
# To download OpenFace, follow the instructions on the software page: https://github.com/TadasBaltrusaitis/OpenFace
os <- Sys.info()[["sysname"]]
openface <- if (os == "Windows") {
  file.path(proj_dir, "OpenFace_2.2.0_win_x64", "FeatureExtraction.exe")
} else {
  file.path(proj_dir, "OpenFace", "build", "bin", "FeatureExtraction")
}

# Set a frame per second sampling rate for video analysis
fps <- 10

# Create directories if needed
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Start timer
start_time <- Sys.time()

# Find all .webm files in the uploads folder
webm_files <- list.files(input_dir, pattern = "\\.webm$", full.names = TRUE)

# return error message if empty
if (length(webm_files) == 0) {
  stop("No .webm files found in: ", input_dir)
}

# Convert video recordings to mp4 format and resample at 10fps
temp_videos <- character(length(webm_files))

for (i in seq_along(webm_files)) {
  video_path <- webm_files[i]
  video_name <- tools::file_path_sans_ext(basename(video_path))
  temp_video <- file.path(temp_dir, paste0(video_name, "_processed.mp4"))

  system2(ffmpeg, args = c(
    "-i", shQuote(video_path),
    "-vf", paste0("fps=", fps),
    "-c:v", "libx264",
    "-pix_fmt", "yuv420p",
    "-preset", "fast",
    "-crf", "23",
    "-y", shQuote(temp_video)
  ))

  temp_videos[i] <- temp_video
}

# Run OpenFace once across all converted videos
message("Running OpenFace on ", length(temp_videos), " videos...")
system2(openface, args = c(
  rbind("-f", shQuote(temp_videos)),
  "-aus",
  "-out_dir", shQuote(output_dir)
))

message("Done processing all videos")

# Clean up temp files
unlink(file.path(temp_dir, "*"), recursive = TRUE)

# Report total time
execution_time <- difftime(Sys.time(), start_time, units = "secs")
message(sprintf("Execution time: %.1f seconds", as.numeric(execution_time)))
message("All videos processed!")
