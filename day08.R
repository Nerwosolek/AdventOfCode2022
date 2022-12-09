source("setwd.R")
library(purrr)
library(stringr)


# Part I ------------------------------------------------------------------

f <- scan("data/input08.txt", what = character())

m <- matrix(as.integer(unlist(strsplit(f, "", ))), nrow = 99, ncol = 99, byrow = TRUE)

vCnt <- 99 + 99 + 97 + 97

#artificial borders lower than every tree:
m_bordered <- rbind(rep(-1, 99), m, rep(-1, 99))
m_bordered <- cbind(rep(-1, 101), m_bordered, rep(-1, 101))

vCnt <- 0

max_neigh <- function(m, x, y, dir) {
  if (dir == "left") return(max(m[1:(x-1), y]))
  if (dir == "right") return(max(m[(x+1):101, y]))
  if (dir == "top") return(max(m[x, 1:(y-1)]))
  if (dir == "bottom") return(max(m[x, (y+1):101]))
}

run <- function(m_bordered) {
  vCnt <- 0
  browser()
  for (x in 2:100) {
    for (y in 2:100) {
      if (max_neigh(m_bordered, x, y, "left") < m_bordered[x, y] ||
          max_neigh(m_bordered, x, y, "right") < m_bordered[x, y] ||
          max_neigh(m_bordered, x, y, "top") < m_bordered[x, y] ||
          max_neigh(m_bordered, x, y, "bottom") < m_bordered[x, y]
          ) vCnt = vCnt + 1
    }
  }
  vCnt
}

result <- run(m_bordered)

# Part II -----------------------------------------------------------------
run2 <- function(m) {
  browser()
  scenic <- 0
  for (x in 1:99) {
    for (y in 1:99) {
      scenic <- max(scenic, first_blocking_tree_dist(m, x, y, "left") *
                      first_blocking_tree_dist(m, x, y, "right") *
                      first_blocking_tree_dist(m, x, y, "top") *
                      first_blocking_tree_dist(m, x, y, "bottom")
                      )
    }
  }
  scenic
}

first_blocking_tree_dist <- function(m, x, y, dir) {
  if (dir == "left") {
    if (x == 1) return(0)
    for (i in (x-1):1) {
      if (m[i, y] >= m[x, y] || i == 1) return(x - i)
    }
  }
  if (dir == "right") {
    if (x == 99) return(0)
    for (i in (x+1):99) {
      if (m[i, y] >= m[x, y] || i == 99) return(i - x)
    }
  }
  if (dir == "top") {
    if (y == 1) return(0)
    for (i in (y-1):1) {
      if (m[x, i] >= m[x, y] || i == 1) return(y - i)
    }
  }
  if (dir == "bottom") {
    if (y == 99) return(0)
    for (i in (y+1):99) {
      if (m[x, i] >= m[x, y] || i == 99) return(i - y)
    }
  }
}

run2(m)
