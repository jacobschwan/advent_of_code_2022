
input_path <- here::here("01/input.txt")

input_vec <- as.numeric(readLines(input_path))

starts <- c(1, which(is.na(input_vec)) + 1)
stops <- c(which(is.na(input_vec)) - 1, length(input_vec))

sum_vec_fn <- function(x,y) {
    sum(input_vec[c(x:y)])
}

calories_vec <- mapply(sum_vec_fn, starts, stops)

max(calories_vec)

sum(sort(calories_vec, decreasing = T)[1:3])
