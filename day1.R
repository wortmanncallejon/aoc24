pacman::p_load(dplyr, readr, purrr, adventr)

input <- read_advent(day = 1, year = 2024)

input <- input[-length(input)]


problem_one <- function(x) {
  list_1 <- sort(as.numeric(map_chr(x, ~ strsplit(.x, "   ")[[1]][1])))
  list_2 <- sort(as.numeric(map_chr(x, ~ strsplit(.x, "   ")[[1]][2])))

  return(sum(abs(list_1 - list_2)))
}


problem_two <- function(x) {
  list_1 <- as.numeric(map_chr(x, ~ strsplit(.x, "   ")[[1]][1]))
  list_2 <- as.numeric(map_chr(x, ~ strsplit(.x, "   ")[[1]][2]))

  factor <- map_dbl(1:length(list_1), ~ sum(list_1[.x] == list_2))

  return(sum(factor * list_1))
}


problem_one(input) |>
    submit_advent(level = 1, day = 1, year = 2024)

problem_two(input) |>
    submit_advent(level = 2, day = 1, year = 2024)


