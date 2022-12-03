source("setwd.R")
library(stringr)
library(purrr)

# Part I ------------------------------------------------------------------

f <- scan(file = "data/input03.txt", what = character())

rucksack <- data.frame( left = str_sub(f, 1, nchar(f)/2), right = str_sub(f, nchar(f)/2 + 1))

all_letters <- c(letters, LETTERS)

l <- str_split(rucksack$left, "")

r <- str_split(rucksack$right, "")

map2(l, r, `%in%`) |> 
  map(which) -> l_inds
  
sum(map2_chr(l, l_inds, ~ .x[.y][1]) |> map_int(~ which(all_letters == .x)))


# Part II -----------------------------------------------------------------

first <- str_split(f[seq(1,300,3)], "")
second <- str_split(f[seq(2,300,3)], "")
third <- str_split(f[seq(3,300,3)], "")

pmap(list(first, second, third), ~ Reduce(intersect, list(..1,..2,..3))) |> 
  map_int(~ which(all_letters == .x)) |> sum()


