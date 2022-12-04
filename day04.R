source("setwd.R")
library(purrr)
library(stringr)

# Part I ------------------------------------------------------------------

f <- scan(file = "data/input04.txt", what = character())

pairs <- str_split(f, ",")
pairs <- lapply(pairs, str_replace, "-", ":")

map_lgl(pairs, function(x) {
  all(eval(parse(text = x[1])) %in% eval(parse(text = x[2]))) | all(eval(parse(text = x[2])) %in% eval(parse(text = x[1])))
}) |> sum()


# Part II -----------------------------------------------------------------

map_lgl(pairs, function(x) {
  any(eval(parse(text = x[1])) %in% eval(parse(text = x[2]))) | any(eval(parse(text = x[2])) %in% eval(parse(text = x[1])))
}) |> sum()
