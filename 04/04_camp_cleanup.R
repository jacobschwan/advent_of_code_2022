library(tidyverse)

input_path <- here::here("04/input.txt")
    
input_tbl <- read_csv(input_path, col_names = c("elf1", "elf2")) 

pair_range_tbl <- input_tbl %>%
    separate(elf1, c("elf1_min", "elf1_max"), convert = T) %>%
    separate(elf2, c("elf2_min", "elf2_max"), convert = T)

#Part 1
pair_range_tbl %>%
    mutate(fully_contained = elf1_min <= elf2_min & elf1_max >= elf2_max | elf1_min >= elf2_min & elf1_max <= elf2_max) %>%
    pull(fully_contained) %>%
    sum()

#Part 2
pair_range_tbl %>%
    rowwise() %>%
    mutate(overlap = any(between(seq(elf1_min,elf1_max), elf2_min, elf2_max))) %>% 
    pull(overlap) %>%
    sum()
