rm(list = ls())

library(tidyverse)

df <- read_csv("processed-data/absentee-combined.csv") |>
  mutate(Election = fct_reorder(Election, election_date),
         days_to_election = days_before_election * -1)

wi <- df |> filter(Jurisdiction == "TOTAL")

wi |>
  ggplot(aes(days_to_election, BallotsReturned)) +
  geom_step() +
  facet_wrap(facets = ~Election)

wi |>
  filter(str_detect(Election, "General")) |>
  ggplot(aes(days_to_election, BallotsReturned)) +
  geom_line(aes(color = Election)) +
  scale_x_continuous(breaks = seq(-49,0,7),
                     labels = c("7 weeks", "6 weeks", "5 weeks", "4 weeks",
                                "3 weeks", "2 weeks", "1 week", "election\nday"))

nov.days.before <- wi |>
  filter(report_date == max(report_date)) |>
  pull(days_before_election)

wi |>
  arrange(Election) |>
  filter(days_before_election == nov.days.before) |>
  select(Election, AbsenteeApplications, BallotsSent, BallotsReturned)
