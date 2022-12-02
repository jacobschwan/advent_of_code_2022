library(tidyverse)

opp_key_vec <- c("A" = "rock",
                 "B" = "paper",
                 "C" = "scissors")

me_key_vec <- c("X" = "rock",
                "Y" = "paper",
                "Z" = "scissors")

shape_points_vec <- c("rock"     = 1,
                      "paper"    = 2,
                      "scissors" = 3)

input_path <- here::here("02/input.txt")

input_tbl <- read_delim(input_path, delim = " ", col_names = c("opp_key", "me_key"))

# Part 1
outcome_tbl <- input_tbl %>%
    mutate(opp = opp_key_vec[opp_key],
           me = me_key_vec[me_key],
           play_points = shape_points_vec[me],
           outcome_points = case_when(me == opp ~ 3,
                                      me == "rock" & opp == "scissors" ~ 6,
                                      me == "paper" & opp == "rock" ~ 6,
                                      me == "scissors" & opp == "paper" ~ 6,
                                      T ~ 0),
           total_points = play_points + outcome_points)

sum(outcome_tbl$total_points)

# Part 2
outcome_key_vec <- c("X" = 0,
                     "Y" = 3,
                     "Z" = 6)

# Vector of wining plays
#            To Beat      Play 
win_vec <- c("rock"     = "paper",
             "paper"    = "scissors",
             "scissors" = "rock")

# Losing plays are the opposite, so switch values & names
lose_vec <- set_names(names(win_vec), win_vec)

outcome2_tbl <- input_tbl %>%
    mutate(opp = opp_key_vec[opp_key],
           outcome_points = outcome_key_vec[me_key],
           me = case_when(outcome_points == 3 ~ opp,
                          outcome_points == 6 ~ win_vec[opp],
                          outcome_points == 0 ~ lose_vec[opp]),
           play_points = shape_points_vec[me],
           total_points = play_points + outcome_points)

sum(outcome2_tbl$total_points)
