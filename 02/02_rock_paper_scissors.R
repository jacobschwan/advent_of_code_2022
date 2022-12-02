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

play_points_fn <- function(me) {
    case_when(me == "rock" ~ 1,
              me == "paper" ~ 2,
              me == "scissors" ~ 3)
}

input_path <- here::here("02/input.txt")

input_tbl <- read_delim(input_path, delim = " ", col_names = c("opp_key", "me_key"))

# Part 1
outcome_tbl <- input_tbl %>%
    left_join(opp_key_tbl, by = c("opp_key")) %>%
    left_join(me_key_tbl, by = c("me_key")) %>%
    mutate(play_points = play_points_fn(me),
           outcome_points = case_when(me == opp ~ 3,
                                      me == "rock" & opp == "scissors" ~ 6,
                                      me == "paper" & opp == "rock" ~ 6,
                                      me == "scissors" & opp == "paper" ~ 6,
                                      T ~ 0),
           total_points = play_points + outcome_points)

sum(outcome_tbl$total_points)

# Part 2
outcome_key_tbl <- tribble(
    ~outcome_key, ~outcome_points,
    "X",          0,
    "Y",          3,
    "Z",          6
)

outcome2_tbl <- outcome_key_tbl %>%
    right_join(input_tbl, by = c("outcome_key" = "me_key")) %>%
    left_join(opp_key_tbl, by = c("opp_key")) %>%
    mutate(me = case_when(outcome_points == 3 ~ opp,
                          outcome_points == 6 ~ case_when(opp == "rock" ~ "paper",
                                                          opp == "paper" ~ "scissors",
                                                          opp == "scissors" ~ "rock"),
                          outcome_points == 0 ~ case_when(opp == "rock" ~ "scissors",
                                                          opp == "paper" ~ "rock",
                                                          opp == "scissors" ~ "paper")),
           play_points = play_points_fn(me),
           total_points = play_points + outcome_points)

sum(outcome2_tbl$total_points)
