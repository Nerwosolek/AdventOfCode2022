source("setwd.R")
library(purrr)
library(stringr)
library(dplyr)

# Part I ------------------------------------------------------------------

f <- readLines("data/input07.txt")
f

make_dir <- function(dir_name) {
  setNames(list(0), dir_name)
}

path <- list()
dirs <- list(root = 0)
current_dir <- ""
for(i in 1:length(f)) {
  listing <- unlist(str_split(f[i], " "))
  if (listing[1] == "$" && listing[2] == "cd") {
    if (listing[3] == "/") {
      path <- ("root")
    }
    else if (listing[3] == "..") path <- path[-length(path)]
    else path <- append(path, listing[3])
    current_dir <- path[length(path)]
  }
  if (listing[1] == "dir") {
    dirs <- append(dirs, make_dir(paste0(str_c(path, collapse = "/"), "/", listing[2])))
  }
  if (!is.na(strtoi(listing[1]))) {
    dirs[[str_c(path, collapse = "/")]] <- dirs[[str_c(path, collapse = "/")]] + strtoi(listing[1])
  }
}

upper_dir_path <- function(dir_path) {
  p <- unlist(str_split(dir_path, "/"))
  p <- p[-length(p)]
  str_c(p, collapse = "/")
}


dirs_sum <- data.frame(path = names(dirs),
                       size = unlist(dirs), 
                       depth = 0
                       )

dirs_sum <- dirs_sum |> mutate(depth = str_count(path, "/") + 1)
dirs_sum <- dirs_sum |> arrange(desc(depth))

for (d in 1:(NROW(dirs_sum)-1)) {
  up <- upper_dir_path(dirs_sum$path[d])
  dirs_sum[up, "size"] <- dirs_sum[up, "size"] + dirs_sum[d, "size"]
}

dirs_sum |> filter(size <= 100000) |> summarise(sum(size))

# Part II -----------------------------------------------------------------


dirs_sum |> filter(size >= 2036703) |> summarise(min(size))
dirs_sum |> filter(size == 2050735)
