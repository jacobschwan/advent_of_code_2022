library(tidyverse)

input_path <- here::here("03/input.txt")

input_vec <- readLines(input_path) 

all_letters <- c(letters, LETTERS)

#Part1

rucksack_tbl <- tibble(
    compartment1 = str_sub(input_vec, 1, nchar(input_vec) / 2),
    compartment2 = str_sub(input_vec, nchar(input_vec)/2 + 1, nchar(input_vec))
)

rucksack_tbl %>%
    mutate(c1_letters = map(compartment1, str_match, pattern = all_letters),
           c2_letters = map(compartment2, str_match, pattern = all_letters),
           match = map2_dbl(c1_letters, c2_letters, ~which(.x == .y)),
           match_ltr = all_letters[match]
           ) %>%
    pull(match) %>%
    sum()


#Part 2

triple_match <- function(x, y, z) {
    which(x = (x == y & x == z))
}

matrix(input_vec, nrow = length(input_vec)/3, byrow = T) %>% as.data.frame()
    as_tibble(.name_repair = "universal" ) %>%
    set_names(c("x", "y", "z")) %>%
    mutate(across(everything(), ~map(.x, str_match, pattern = all_letters))) %>% 
    pmap_dbl(triple_match) %>%
    sum()