library(tidyverse)
library(lubridate)

source("https://raw.githubusercontent.com/mattkwyatt/ggplot_themes/master/theme_mkw.R")

df <- read_csv("../../Python/plusword/output/plusword1_times.csv", col_types = c("Tcc"))

df <- df |>
  mutate(time = str_c(rep("00:", length(df$time)), time),
         time = hms::parse_hms(time))

winning_times <- df |> 
  group_by(date(date)) |> 
  filter(time == min(time)) |> 
  ungroup()

win_streaks <- winning_times |> 
  arrange(date) |> 
  pull(sender) |> 
  rle()

win_streaks <- tibble(sender = win_streaks$values, streak = win_streaks$lengths)

start_date <- winning_times |> pull(date) |> min() |> date()
end_date <- winning_times |> pull(date) |> max() |> date()
  

df |>
  filter(time < hms::hms(minutes = 10)) |> 
  ggplot(aes(x = fct_reorder(sender, .x = time, .fun = "median"), y = time)) +
  geom_boxplot() +
  theme_mkw() +
  labs(x = "Player", y = "Time (minutes)",
       title = "PlusWord finishing times (times over 10 minutes ignored)",
       subtitle = str_c(start_date, " to ", end_date))


winning_times |> 
  group_by(sender) |> 
  summarise(wins = n()) |> 
  ungroup() |> 
  arrange(wins) |> 
  ggplot(aes(x = fct_reorder(sender, .x = wins, .fun = "max"), y = wins)) +
  geom_col() +
  geom_text(aes(label = wins), nudge_y = 0.5) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0, 0.05))) +
  theme_mkw() +
  labs(x = "Player", y = "Victories",
       title = "PlusWord victories",
       subtitle = str_c(start_date, " to ", end_date))


df |> 
  group_by(sender) |> 
  summarise(best_time = min(time)) |> 
  arrange(best_time) |> 
  ggplot(aes(x = fct_reorder(sender, .x = best_time), y = best_time)) +
  geom_col() +
  geom_text(aes(label = best_time), nudge_y = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0, 0.05))) +
  theme_mkw() +
  labs(x = "Player", y = "Time (seconds)",
       title = "Fastest PlusWord finishing time",
       subtitle = str_c(start_date, " to ", end_date))
