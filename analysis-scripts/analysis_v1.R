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

########################################################
election.results <- read_csv("AllResults_ReportingUnit.csv")
election.size <- election.results |>
  group_by(year, month, office) |>
  summarise(votes = sum(votes)) |>
  slice_max(order_by = votes, n = 1) |>
  ungroup() |>
  select(year, month, total_votes = votes)
absentee.share <- wi |>
  mutate(year = year(election_date),
         month = month(election_date, label = T, abbr = F),
         month = str_to_upper(month)) |>
  filter(election_date == report_date) |>
  select(year, month, election_date, total_absentee = BallotsReturned) |>
  inner_join(election.size) |>
  mutate(pct_absentee = total_absentee/total_votes*100,
         label = paste(year, month, sep = "\n"))
ggplot(absentee.share, aes(election_date, pct_absentee)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = str_to_title(label)),
                           min.segment.length = 0) +
  labs(title = "Share of Electorate who Returned an Absentee Ballot")
