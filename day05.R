source("setwd.R")
library(purrr)
library(stringr)

# Part I ------------------------------------------------------------------

f <- scan(file = "data/input05.txt", what = character())
f2 <- readLines("data/input05.txt")
f2 <- f2[-c(1:10)]
f <- f[-(1:65)]
head(f)
init_stacks <- function() {
  list(
    c("C","S","G","B"),
    c("G","V","N","J","H","W","M","T"),
    c("S","Q","M"),
    c("M","N","W","T","L","S","B"),
    c("P","W","G","V","T","F","Z","J"),
    c("S","H","Q","G","B","T","C"),
    c("W","B","P","J","T"),
    c("M","Q","T","F","Z","C","D","G"),
    c("F","P","B","H","S","N"))
}
stacks <- init_stacks()

pop <- function(stack) {
  p <- stacks[[stack]][1]
  stacks[[stack]] <<- stacks[[stack]][-1]
  p
}

push <- function(stack, letter) {
  stacks[[stack]] <<- c(letter, stacks[[stack]])
}

commands <- str_split(f2, " ")
commands <- commands |> map(~ c(strtoi(.x[2]),strtoi(.x[4]),strtoi(.x[6])))

map(commands, function(command) replicate(command[1], push(command[3], pop(command[2]))))

tops <- map(stacks, ~ .x[1])
result <- str_flatten(unlist(tops))
result

# Part II -----------------------------------------------------------------

pop_n <- function(stack, n) {
  p <- stacks[[stack]][1:n]
  stacks[[stack]] <<- stacks[[stack]][-(1:n)]
  p
}

push_n <- function(stack, elems) {
  stacks[[stack]] <<- c(elems, stacks[[stack]])
}
stacks <- init_stacks()
map(commands, function(command) push_n(command[3], pop_n(command[2], command[1])))
tops2 <- str_flatten(unlist(map(stacks, ~ .x[1])))
tops2
