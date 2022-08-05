# Load libraries ---------------------------------------------------------------

library(StatsBombR)
library(dplyr)
library(ggplot2)

# Read data into R -------------------------------------------------------------

weuro_comp <- FreeCompetitions() %>%
  filter(competition_name == "UEFA Women's Euro")

weuro_matches <- FreeMatches(weuro_comp)

weuro_events <- free_allevents(weuro_matches) %>%
  janitor::clean_names()

# weuro_events_360 <- free_allevents_360(weuro_matches)

# Tidy data --------------------------------------------------------------------

england_events <- weuro_events %>%
  filter(team_name == "England Women's")

all_shots <- weuro_events %>%
  filter(!is.na(shot_outcome_name)) %>%
  mutate(
    # Shorten team names to country only
    team_name_short = case_when(
      stringr::str_detect(team_name, "England")     ~ "England",
      stringr::str_detect(team_name, "Germany")     ~ "Germany",
      stringr::str_detect(team_name, "France")      ~ "France",
      stringr::str_detect(team_name, "Sweden")      ~ "Sweden",
      stringr::str_detect(team_name, "Netherlands") ~ "Netherlands",
      stringr::str_detect(team_name, "Spain")       ~ "Spain",
      stringr::str_detect(team_name, "Austria")     ~ "Austria",
      stringr::str_detect(team_name, "Belgium")     ~ "Belgium",
      stringr::str_detect(team_name, "Iceland")     ~ "Iceland",
      stringr::str_detect(team_name, "Denmark")     ~ "Denmark",
      stringr::str_detect(team_name, "Norway")      ~ "Norway",
      stringr::str_detect(team_name, "Switzerland") ~ "Switzerland",
      stringr::str_detect(team_name, "Italy")       ~ "Italy",
      stringr::str_detect(team_name, "Portugal")    ~ "Portugal",
      stringr::str_detect(team_name, "Finland")     ~ "Finland",
      stringr::str_detect(team_name, "Ireland")     ~ "Northern Ireland"),
    # Order by final ranking
    team_name_short = factor(
      team_name_short,
      levels = c("Northern Ireland", "Finland", "Portugal", "Italy", "Switzerland",
                 "Norway", "Denmark", "Iceland", "Belgium", "Austria", "Spain",
                 "Netherlands", "Sweden", "France", "Germany", "England")))

# Build plot -------------------------------------------------------------------

# Activity types for England
ggplot() +
  geom_bar(
    data = england_events %>% filter(!is.na(player_name)),
    aes(y = type_name),
    stat = "count") +
  facet_wrap(~player_name, ncol = 6, scales = "free") +
  theme(
    legend.position = "bottom")

# Distribution of shot outcomes by team
shot_outcome_colours <- c(
  "Blocked" = "#FF0000",
  "Goal" = "#FF8700",
  "Off T" = "#FFD300",
  "Post" = "#DEFF0A",
  "Saved" = "#0AEFFF",
  "Saved Off Target" = "#147DF5",
  "Saved to Post" = "#580AFF",
  "Wayward" = "#BE0AFF")

ggplot() +
  geom_bar(
    data = all_shots,
    aes(y = team_name_short, fill = shot_outcome_name),
    stat = "count", width = .7) +
  facet_wrap(~shot_outcome_name, ncol = 4, scales = "free_x") +
  scale_fill_manual(values = shot_outcome_colours) +
  labs(
    title = "UEFA Women's EURO 2022: Shot outcomes by team",
    x = NULL, y = NULL,
    caption = "Data source: StatsBomb") +
  ggdark::dark_mode() +
  theme(
    plot.title = element_text(size = rel(2), face = "bold", margin = margin(b = 10, unit = "pt")),
    axis.text = element_text(size = rel(1.2)),
    strip.text = element_text(size = rel(1.5)),
    plot.caption = element_text(size = rel(1.2), margin = margin(t = 10, unit = "pt")),
    panel.spacing = unit(2, "lines"),
    legend.position = "none")

ggsave(
  here::here("plots/shot_outcomes.png"),
  last_plot(), width = 12, height = 9, units = "in", dpi = 300)
