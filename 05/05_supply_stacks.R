library(purrr)

input_path <- here::here("05/input.txt")

input_lines <- readLines(input_path) 

map_lines <- input_lines[1:grep("^ 1", input_lines) - 1]

map_columns <- seq.int(2, nchar(map_lines[1]) -1, by = 4)

map_list <- map_columns %>%
    map(~substr(map_lines, .x, .x)) %>%
    map(~.x[.x != " "])

move_boxes <- function(boxes, from, to, map_list) {
    for (i in 1:boxes) {
        map_list[[to]] <- c(map_list[[from]][1], map_list[[to]])
        map_list[[from]] <- map_list[[from]][-1]
    }
    map_list
}

move_lines <- input_lines[(grep("^ 1", input_lines) + 2):length(input_lines)]

move_list <- move_lines %>%
    strsplit(" ") %>%
    map(~as.numeric(.x[grep("[0-9]+", .x)]))

all_box_moves <- function(map_list, move_list) {
    for(i in move_list) {
        map_list <- move_boxes(boxes = i[1], 
                               from = i[2],
                               to = i[3],
                               map_list = map_list)
    }  
    map_list
}

all_box_moves(map_list, move_list) %>%
    map_chr(~.x[1]) %>%
    paste(collapse = "")

#GRTSWNJHH

move_boxes_9001 <- function(boxes, from, to, map_list) {
    map_list[[to]] <- c(map_list[[from]][1:boxes], map_list[[to]])
    map_list[[from]] <- map_list[[from]][-c(1:boxes)]
    map_list
}

all_box_moves_9001 <- function(map_list, move_list) {
    for(i in move_list) {
        map_list <- move_boxes_9001(boxes = i[1], 
                                    from = i[2],
                                    to = i[3],
                                    map_list = map_list)
    }
    map_list
}



all_box_moves_9001(map_list, move_list) %>%
    map_chr(~.x[1]) %>%
    paste(collapse = "")

#QLFQDBBHM