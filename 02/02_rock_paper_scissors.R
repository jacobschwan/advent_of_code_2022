# A == "rock"      X == "rock"
# B == "paper"     Y == "paper"
# C == "scissors"  Z == "scissors"

library(tidyverse)

opp_key_tbl <- tribble(
    ~opp_key,  ~opp,
    "A",       "rock",
    "B",       "paper",
    "C",       "scissors"
)

me_key_tbl <- tribble(
    ~me_key,   ~me,
    "X",       "rock",
    "Y",       "paper",
    "Z",       "scissors"
)

input_path <- here::here("02/input.txt")

input_tbl <- read_delim(input_path, delim = " ", col_names = c("opp_key", "me_key"))

outcome_tbl <- input_tbl %>%
    left_join(opp_key_tbl, by = c("opp_key")) %>%
    left_join(me_key_tbl, by = c("me_key")) %>%
    mutate(play_points = case_when(me == "rock" ~ 1,
                                   me == "paper" ~ 2,
                                   me == "scissors" ~ 3),
           outcome_points = case_when(me == opp ~ 3,
                                      me == "rock" & opp == "scissors" ~ 6,
                                      me == "paper" & opp == "rock" ~ 6,
                                      me == "scissors" & opp == "paper" ~ 6,
                                      T ~ 0),
           total_points = play_points + outcome_points)

sum(outcome_tbl$total_points)
