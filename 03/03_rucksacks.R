library(tidyverse)

input_path <- here::here("03/input.txt")

input_vec <- readLines(input_path) 

rucksack_tbl <- tibble(
    compartment1 = str_sub(input_vec, 1, nchar(input_vec) / 2),
    compartment2 = str_sub(input_vec, nchar(input_vec)/2 + 1, nchar(input_vec))
)

all_letters <- c(letters, LETTERS)

rucksack_tbl %>%
    mutate(c1_letters = map(compartment1, str_match, pattern = all_letters),
           c2_letters = map(compartment2, str_match, pattern = all_letters),
           match = map2_dbl(c1_letters, c2_letters, ~which(.x == .y)),
           match_ltr = all_letters[match]
           ) %>%
    pull(match) %>%
    sum()


