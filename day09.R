source("setwd.R")
library(purrr)
library(stringr)


# Part I ------------------------------------------------------------------

f <- readLines("data/input09.txt") 
f

head(f)

m <- matrix(0, nrow = 6000, ncol = 6000)


head = c(3000,3000)
tail = c(3000,3000)
x = 1
y = 2
m[tail[x], tail[y]] = 1

run <- function(h, t, m) {
  for (move in f) {
    for (step in 1:value(move)) {
      head[dimension(move)] = head[dimension(move)] + direction(move)
      if (distance(head, tail) > 1) {
        tail <- push(head, tail)
        m[tail[x], tail[y]] = 1
      }
    }
  }
  m
}

value <- function(move) {
  as.integer(substring(move, 3))
}

dimension <- function(move) {
  ifelse (substring(move, 1, 1) %in% c("L", "R"), x, y)
}

direction <- function(move) {
  ifelse (substring(move, 1, 1) %in% c("L", "D"), -1, +1)
}

distance <- function(h, t) {
  max(abs(h[x] - t[x]), abs(h[y] - t[y]))
}

push <- function(h, t) {
  c(t[x] + sign(h[x] - t[x]), t[y] + sign(h[y] - t[y]))
}

sum(run(head, tail, m))

# Part II -----------------------------------------------------------------

knots <- replicate(10, head)

run2 <- function(k, m) {
  for (move in f) {
    for (step in 1:value(move)) {
      k[dimension(move), 1] = k[dimension(move), 1] + direction(move) # move head
      for (i in 2:10) {
        if (distance(k[, i-1], k[, i]) > 1) {
          k[, i] <- push(k[, i-1], k[, i])
        } 
      }
      m[k[x, 10], k[y, 10]] = 1
    }
  }
  m
}

sum(run2(knots, m))

