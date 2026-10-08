rm(list = ls())
library(tidyverse)

nov.2026 <- read_csv("https://elections.wi.gov/sites/default/files/documents/AbsenteeCounts_Muni_2026%20General%20Election_cumulative_long_through_2026-10-07.csv")
aug.2026 <- read_csv("https://elections.wi.gov/sites/default/files/documents/AbsenteeCounts_Muni_2026%20Partisan%20Primary_cumulative_long_through_2026-08-13.csv")
april.2026 <- read_csv("https://elections.wi.gov/sites/default/files/documents/AbsenteeCounts_Muni_2026%20Spring%20Election_cumulative_long_through_2026-04-10.csv")

write_csv(nov.2026, "processed-data/2026-november.csv")
write_csv(aug.2026, "processed-data/2026-august.csv")
write_csv(april.2026, "processed-data/2026-april.csv")
