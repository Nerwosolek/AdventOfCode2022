source("setwd.R")
library(purrr)
library(stringr)


# Part I & II------------------------------------------------------------------

f <- readLines("data/input10.txt")

crt <- matrix(".", nrow = 6, ncol = 40)
char <- "."
cycles <- rep(NA, 240)
X <- 1

cycles_nbr <- 2 * length(which(startsWith(f, "addx"))) + length(which(startsWith(f, "noop")))
instr_pointer <- 1
micro_cycle <- 0

for (i in 1:cycles_nbr) {
  cycles[i] <- X
  if (((i - 1) %% 40) %in% c(cycles[i]-1, cycles[i], cycles[i]+1)) char <- "#"
  else char <- "."
  crt[ceiling(i / 40), ((i-1) %% 40) + 1] = char
  if (micro_cycle == 0) {
    instr <- fetch(f[instr_pointer])
    instr_pointer <- instr_pointer + 1
    if (instr[1] == "addx") micro_cycle <- 2
    else micro_cycle <- 1
  }
  if (micro_cycle > 0) {
    if (instr[1] == "addx" && micro_cycle == 1) X = X + as.numeric(instr[2])
    micro_cycle = micro_cycle - 1
  }
}

fetch <- function(instruction) {
  str_split(instruction, " ")[[1]]
}

check_cycles <- c(20, 60, 100, 140, 180, 220)
sum(cycles[check_cycles] * check_cycles)

for (i in 1:6) {
  print(str_c(crt[i,], collapse = ""))
}
