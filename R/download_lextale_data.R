#' A function to interact with gitlab
#'
#' This function will download lexTALE data from the `lextale_ru` repo.
#'
#' @param download_lextale_data A function downloading lexTALE data
#' @param destination Where to download the data (defaults to working directory)
#' @param apply_filter A conditional expression to subset the dataset
#' @keywords download
#' @import dplyr
#' @import tidyr
#' @import forcats
#' @import fs
#' @import readr
#' @import here
#' @import stringr
#' @import glue
#' @import lubridate
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom stats time
#' @importFrom utils download.file unzip
#' @export


download_lextale_data <- function(
    destination = NULL,
    apply_filter = NULL
) {

  # URL to zip file
  url_lextale <- "https://gitlab.pavlovia.org/jvcasillas/lextale_ru/-/archive/master/lextale_ru-master.zip"

  # Set path
  if (is.null(destination)) {
    dest_path <- here()
  } else {
    dest_path <- glue("{here({destination})}")
  }

  message(glue("Checking for {destination} directory"))
  # Throw error if destination directory doesn't exist
  if (!dir.exists(file.path(dest_path))) {
    stop(glue("{destination} directory not found... please provide a valid path."))
  } else {
    message(glue("{destination} directory exists."))
  }

  message("Checking for 'lextale' directory")
  # Create lextale directory if it doesn't exist
  if (!dir.exists(file.path(glue("{dest_path}/lextale")))) {
    message("'lextale' directory not found... creating it.")
    dir.create(file.path(glue("{dest_path}/lextale")))
  } else {
    message("'lextale' directory already exists.")
  }

  message("Creating sudirectories")
  # Create temp directory if it doesn't exist
  if (!dir.exists(file.path(glue("{dest_path}/lextale/temp")))) {
    message("'temp' directory not found... creating it.")
    dir.create(file.path(glue("{dest_path}/lextale/temp")))
  } else {
    message("'temp' directory already exists.")
  }

  # Create temp directory if it doesn't exist
  if (!dir.exists(file.path(glue("{dest_path}/lextale/raw")))) {
    message(glue("{destination}/lextale/raw subdirectory not found... creating it."))
    dir.create(file.path(glue("{dest_path}/lextale/raw")))
  } else {
    message(glue("{destination}/lextale/raw directory already exists."))
  }

  # Create temp directory if it doesn't exist
  if (!dir.exists(file.path(glue("{dest_path}/lextale/tidy")))) {
    message(glue("{destination}/lextale/tidy subdirectory not found... creating it."))
    dir.create(file.path(glue("{dest_path}/lextale/tidy")))
  } else {
    message(glue("{destination}/lextale/tidy directory already exists."))
  }

  # Download experiment zip file
  message("Downloading repo as zip file...")
  download.file(
    url = url_lextale,
    destfile = glue("{dest_path}/lextale/temp/lextale.zip")
  )

  # Unzip files
  message("Unzipping files...")
  unzip(
    zipfile = glue("{dest_path}/lextale/temp/lextale.zip"),
    junkpaths = TRUE,
    exdir = glue("{dest_path}/lextale/temp/")
  )

  # Get df of all unzipped files
  all_files <- dir_ls(path = glue("{dest_path}/lextale/temp")) |>
    as_tibble()

  # Get tibble of legit csv data files
  data_files_to_keep_tib <-
    dir_ls(path = glue("{dest_path}/lextale/temp"), regexp = ".csv") |>
    as_tibble() |>
    filter(!(.data$value %in% c(
      here("data", "raw", "temp", "lextale_instructions_text.csv"),
      here("data", "raw", "temp", "lextale_practice_trials.csv"),
      here("data", "raw", "temp", "lextale_trials.csv")
    )
    )
    ) |>
    transmute(
      full_path = .data$value,
      path = str_remove_all(.data$value, glue("{dest_path}/lextale/temp/")),
    ) |>
    separate(path, into = c("ruid", "trash1"), sep = "_lextale_ru_", remove = F) |>
    separate(path, into = c("trash2", "date"), sep = "_ru_", remove = F) |>
    separate(date, into = c("date", "time"), sep = "_") |>
    mutate(
      date = ymd(date),
      time = str_remove_all(time, ".csv")
    ) |>
    select(.data$ruid, date, time, path, .data$full_path) |>
    filter(!(.data$ruid %in% c("PARTICIPANT", "")))

  # Load data_files_to_keep, subset if necessary, then move delete unwanted files
  hold <- read_csv(data_files_to_keep_tib$full_path) |>
    clean_names() |>
    separate(date, into = c("date", "time"), sep = "_") |>
    separate(date, into = c("year", "month", "day"), sep = "-", remove = F) |>
    mutate(
      month = as.numeric(month),
      term = case_when(
        month >= 1  & month <= 5  ~ "sp",
        month >= 6  & month <= 8  ~ "su",
        month >= 9  & month <= 12 ~ "fa"
      ),
      term = glue("{term}_{year}")
    )

  # Apply filter if needed
  if (!is.null(apply_filter)) {
    ids_to_keep <- hold |>
      filter(!!rlang::parse_expr(apply_filter)) |>
      pull(.data$ruid) |>
      unique()
  } else {
    ids_to_keep <- hold |>
      pull(.data$ruid) |>
      unique()
  }

  # Get vector of files paths for files to keep
  data_files_to_keep <- data_files_to_keep_tib |>
    filter(.data$ruid %in% ids_to_keep) |>
    pull(.data$full_path)

  # Tidy lextale data
  tidy_lt <- hold |>
    filter(
      .data$ruid %in% ids_to_keep,
      !is.na(.data$lextale_trial_loop_this_rep_n)
    ) |>
    separate(date, into = c("year", "month", "day"), sep = "-", remove = F) |>
    mutate(
      month = as.numeric(month),
      term = case_when(
        month >= 1  & month <= 5  ~ "sp",
        month >= 6  & month <= 8  ~ "su",
        month >= 9  & month <= 12 ~ "fa"
      ),
      term = glue("{term}_{year}")
    ) |>
    select(-c("month", "day", "year", "time")) |>
    select(
      .data$ruid:date,
      .data$term,
      .data$exp_name:.data$frame_rate,
      trial_n = .data$lextale_trial_loop_this_trial_n,
      word:.data$correct_response,
      participant_response = .data$key_resp_lextale_trial_keys,
      is_correct = .data$key_resp_lextale_trial_corr,
      rt = .data$key_resp_lextale_trial_rt,
    ) |>
    write_csv(file = glue("{dest_path}/lextale/tidy/lextale_tidy_unscored.csv"))

  # Move data files to new directory
  message("Moving raw data files to appropriate directory...")
  file.rename(
    from = data_files_to_keep,
    to = str_replace_all(data_files_to_keep, "temp", "/raw")
  )

  # Clear out 'temp' directory and leave receipts
  message("Cleaning up...")
  file.remove(
    c(
      dir_ls(path = glue("{dest_path}/lextale/temp")),
      glue("{dest_path}/lextale/temp/.gitignore")
    )
  )

  new_readme <- glue("{dest_path}/lextale/README.md")
  file.create(new_readme)
  writeLines(
    text = c(
      glue("LexTALE data last downloaded on {Sys.time()}"),
      glue("{length(data_files_to_keep)} files moved to {dest_path}/lextale/raw"),
      glue("Tidy, unscored datafile moved to {dest_path}/lextale/tidy")
    ),
    con = new_readme
  )

  # Delete temp folder
  message("Deleting temp directory")
  dir_delete(glue("{dest_path}/lextale/temp"))

  message(
    c(
      "Finished.  ",
      glue("{length(data_files_to_keep)} files moved to {dest_path}/lextale/raw"),
      glue("Tidy, unscored datafile moved to {dest_path}/lextale/tidy")
    )
  )

}
