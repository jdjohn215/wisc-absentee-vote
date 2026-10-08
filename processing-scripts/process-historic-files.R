rm(list = ls())
library(tidyverse)
library(readxl)

# Process pre-2026 WEC absentee reports from raw-data/ into the standardized
# long format matching processed-data/2026-*.csv:
#   Election, HINDI, Jurisdiction, AbsenteeApplications, BallotsSent,
#   BallotsReturned, InPersonAbsentee, Date  (municipality x report date)
#
# Decisions baked in:
# - Municipal ("Muni") files only; county files excluded.
# - HINDI coerced to integer (leading zeros dropped), matching the 2026 files.
# - Exact duplicates dropped (2021 Muni 1-27 "(1)" copy; undated 2024 GE Muni
#   file duplicating the 10/9/2024 snapshot).
# - "Municipal Absentee Counts as of September 19, 2024" is junk data (Brewers
#   game results) and is excluded.
# - 2025 Spring Primary files (numbered _0.._18, no dates) are excluded per
#   user decision, so there is no 2025-february output.

std_cols <- c("Election", "HINDI", "Jurisdiction", "AbsenteeApplications",
              "BallotsSent", "BallotsReturned", "InPersonAbsentee")

fmt <- function(d) paste(month(d), day(d), year(d), sep = "/")

read_snapshot <- function(path, date) {
  df <- if (grepl("\\.xlsx$", path)) read_xlsx(path) else read_csv(path, show_col_types = FALSE)
  df[, std_cols] |>
    mutate(
      HINDI = as.integer(as.character(HINDI)),
      across(where(is.character) & !Election & !Jurisdiction,
            ~suppressWarnings(as.integer(.x))),
      Date = date
    ) |>
    relocate(Date, .after = last_col())
}

# folder labels like "Oct. 24" (2022 GE), "3.20.2023" (2023), "March 13" (2024)
parse_folder <- function(lab, yr) {
  if (grepl("[A-Za-z]", lab)) {
    m <- str_match(lab, "([A-Za-z]+)\\.?\\s*([0-9]+)")
    mo <- match(substr(m[2], 1, 3), month.abb)
    dd <- as.integer(m[3])
  } else {
    p <- strsplit(lab, "\\.")[[1]]
    mo <- as.integer(p[1]); dd <- as.integer(p[2])
  }
  paste0(mo, "/", dd, "/", yr)
}

# ---- 2021 Spring Primary / Spring Election: date as m-d-yyyy in filename
p21 <- list.files("raw-data/Spring Primary 2021", full.names = TRUE, pattern = "Muni.*csv$")
p21 <- p21[!grepl("\\(1\\)", basename(p21))] # byte-identical duplicate download
d21p <- p21 |>
  map(\(p) read_snapshot(p, fmt(mdy(str_extract(basename(p), "[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}"))))) |>
  list_rbind()
d21e <- list.files("raw-data/Spring Election 2021", full.names = TRUE) |>
  map(\(p) read_snapshot(p, fmt(mdy(str_extract(basename(p), "[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}"))))) |>
  list_rbind()

# ---- 2022 August primary: date is folder name m.d.yyyy; xlsx only where csv missing
p22a <- list.files("raw-data/August Partisan Primary 2022", full.names = TRUE) |>
  map_chr(\(f) {
    csv <- file.path(f, "AbsenteeCounts_Muni_.csv")
    if (file.exists(csv)) csv else file.path(f, "AbsenteeCounts_Muni_.xlsx")
  })
d22a <- p22a |> map(\(p) read_snapshot(p, fmt(mdy(basename(dirname(p)))))) |> list_rbind()

# ---- 2022 General: folders "Oct. 24" / "Nov. 1"
p22g <- list.files("raw-data/General Election Fall 2022", full.names = TRUE) |>
  map_chr(\(f) list.files(f, pattern = "Muni", full.names = TRUE)) |>
  unname()
d22g <- p22g |> map(\(p) read_snapshot(p, parse_folder(basename(dirname(p)), 2022))) |>
  list_rbind()

# ---- 2023 Spring Primary / Election: folders "Feb. 8" or "3.20.2023"
read_by_folder <- function(dir, yr) {
  list.files(dir, full.names = TRUE) |>
    map_chr(\(f) list.files(f, pattern = "Muni", full.names = TRUE)) |>
    map(\(p) read_snapshot(p, parse_folder(basename(dirname(p)), yr))) |>
    list_rbind()
}
d23p <- read_by_folder("raw-data/Spring Primary 2023", 2023)
d23e <- read_by_folder("raw-data/Spring Election 2023", 2023)

# ---- 2024 Spring: dated files at top level (YYYYMMDD prefix) AND "March 5"-"March 21" folders
se24 <- "raw-data/Spring Election 2024"
p24s <- c(
  list.files(se24, pattern = "Muni.*\\.csv$", full.names = TRUE),
  list.files(se24, full.names = TRUE) |> keep(dir.exists) |>
    map_chr(\(f) list.files(f, pattern = "Muni", full.names = TRUE))
)
d24s <- p24s |>
  map(\(p) {
    bn <- basename(p)
    date <- if (grepl("^\\d{8} ", bn)) {
      fmt(ymd(gsub("^([0-9]{4})([0-9]{2})([0-9]{2}).*", "\\1-\\2-\\3", bn)))
    } else parse_folder(basename(dirname(p)), 2024)
    read_snapshot(p, date)
  }) |>
  list_rbind()

# ---- 2024 Partisan Primary: YYYYMMDD_AbsenteeCounts_Muni...
# Some files carry an extra column. Most are single-date snapshots with a
# constant date column, but 20240708 is a cumulative long file holding 11
# daily snapshots ("8-Jul", "29-Jun", ...), so per-row dates are used when
# parseable and the filename date otherwise.
read_24aug <- function(p) {
  raw <- read_csv(p, show_col_types = FALSE)
  if (ncol(raw) == 8) {
    parsed <- suppressWarnings(mdy(paste0(raw[[8]], "-2024")))
    if (all(is.na(parsed))) {
      # unnamed constant date column (e.g. "...8" = "9-Jul"): use filename date
      out <- raw[, std_cols] |>
        mutate(Date = fmt(ymd(gsub("^([0-9]{4})([0-9]{2})([0-9]{2}).*", "\\1-\\2-\\3", basename(p)))))
    } else {
      out <- raw |> mutate(Date = fmt(parsed)) |> select(all_of(std_cols), Date)
    }
  } else {
    out <- raw[, std_cols] |>
      mutate(Date = fmt(ymd(gsub("^([0-9]{4})([0-9]{2})([0-9]{2}).*", "\\1-\\2-\\3", basename(p)))))
  }
  out |>
    mutate(
      HINDI = as.integer(as.character(HINDI)),
      across(where(is.character) & !Date & !Election & !Jurisdiction,
            ~suppressWarnings(as.integer(.x)))
    ) |>
    relocate(Date, .after = last_col())
}
p24a <- list.files("raw-data/2024 Partisan Primary", pattern = "_Muni_.*\\.csv$", full.names = TRUE)
d24a <- p24a |> map(read_24aug) |>
  list_rbind() |>
  filter(!duplicated(select(cur_data(), Date, HINDI))) # 20240708 overlaps the standalone daily files

# ---- 2024 General: "as of <Month> <d>, <yyyy>" files
# "as of September 19, 2024" is junk data (Brewers results) and is excluded.
ge24 <- list.files("raw-data/General Election 2024", pattern = "^Municipal.*\\.csv$", full.names = TRUE)
ge24 <- ge24[!grepl("September 19", ge24)]
d24g <- ge24 |>
  map(\(p) read_snapshot(p, fmt(mdy(gsub(".*as of ([A-Za-z]+) ([0-9]{1,2}), ([0-9]{4})\\.csv$",
                                        "\\1-\\2-\\3", basename(p)))))) |>
  list_rbind()

# ---- 2025 Spring Election: suffix m_d_yy
p25e <- list.files("raw-data/2025 Spring Election Absentee Data", pattern = "Muni_.*\\.csv$", full.names = TRUE)
d25e <- p25e |>
  map(\(p) read_snapshot(p, fmt(mdy(gsub(".*Election_([0-9]{1,2})_([0-9]{1,2})_([0-9]{2})\\.csv$",
                                        "\\1-\\2-20\\3", basename(p)))))) |>
  list_rbind()

# ---- write one file per election, matching the 2026 naming scheme
outs <- list("2021-february" = d21p, "2021-april" = d21e, "2022-august" = d22a,
             "2022-november" = d22g, "2023-february" = d23p, "2023-april" = d23e,
             "2024-april" = d24s, "2024-august" = d24a, "2024-november" = d24g,
             "2025-april" = d25e)

iwalk(outs, \(df, nm) {
  stopifnot(!any(is.na(df$Date)), !any(duplicated(select(df, Date, HINDI))))
  write_csv(df, file.path("processed-data", paste0(nm, ".csv")))
  cat(sprintf("%s: %d rows, %d report dates\n", nm, nrow(df), n_distinct(df$Date)))
})
