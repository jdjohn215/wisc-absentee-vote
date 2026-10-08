rm(list = ls())
library(tidyverse)

# Combine all per-election files in processed-data/ into one tidy dataset:
#   Election, HINDI, Jurisdiction, AbsenteeApplications, BallotsSent,
#   BallotsReturned, InPersonAbsentee, Date, election_date, days_before_election
#
# election_date is assigned here in code from the filename (year-month maps to
# Wisconsin's election date that month), so this step survives regeneration of
# the processed-data files.

election_dates <- tribble(
  ~file,         ~election_date,
  "2021-february", "2021-02-16", # Spring primary
  "2021-april",    "2021-04-06", # Spring general
  "2022-august",   "2022-08-09", # Partisan primary
  "2022-november", "2022-11-08", # General
  "2023-february", "2023-02-21", # Spring primary
  "2023-april",    "2023-04-04", # Spring general
  "2024-april",    "2024-04-02", # Presidential preference / spring general
  "2024-august",   "2024-08-13", # Partisan primary
  "2024-november", "2024-11-05", # General
  "2025-april",    "2025-04-01", # Spring general
  "2026-april",    "2026-04-07", # Spring general
  "2026-august",   "2026-08-11", # Partisan primary
  "2026-november", "2026-11-03"  # General
) |> mutate(election_date = as.Date(election_date))

files <- list.files("processed-data", full.names = TRUE)
keys <- tools::file_path_sans_ext(basename(files))
stopifnot(all(keys %in% election_dates$file))

absentee <- files |>
  set_names(keys) |>
  imap(\(p, key) {
    read_csv(p, show_col_types = FALSE) |>
      # drop any stale election_date so the lookup below is the single source
      select(-any_of("election_date")) |>
      mutate(file = key)
  }) |>
  list_rbind() |>
  left_join(election_dates, by = "file") |>
  mutate(
    report_date = mdy(Date),
    days_before_election = as.integer(election_date - report_date)
  ) |>
  select(-file, -Date) |>
  relocate(report_date, election_date, days_before_election, .after = InPersonAbsentee)

stopifnot(!any(is.na(absentee$report_date)), !any(is.na(absentee$election_date)))
write_csv(absentee, "processed-data/absentee-combined.csv")
cat(sprintf("Combined dataset: %d rows, %d elections, %d municipalities\n",
            nrow(absentee), n_distinct(absentee$election_date), n_distinct(absentee$HINDI)))
