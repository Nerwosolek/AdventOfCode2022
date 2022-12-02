source("setwd.R")
library(dplyr)
library(purrr)
f <- read.table("data/input02.txt")
head(f)
colnames(f) <- c("P1","P2")

# Part I ------------------------------------------------------------------


comp <- function(p1, p2) {
  ifelse(p1 == p2,
         3, 
         ifelse(p1 == "A" && p2 == "B" | p1 == "B" && p2 == "C" | p1 == "C" && p2 == "A",
                0,
                6
    )
  )
}

f |> 
  mutate(P2a = ifelse(
    P2 == "X", "A", P2
  )) |> 
  mutate(
    P2a = ifelse(
      P2 == "Y", "B", P2a
    )
  ) |> 
  mutate(
    P2a = ifelse(
      P2 == "Z", "C", P2a
    )
  )|> 
  mutate(score = 
    (P2a == "A") * 1 + (P2a == "B") * 2 + (P2a == "C") * 3
  ) -> scores

scores <- cbind(scores, score2 = unlist(purrr::map2(scores$P2a, scores$P1, comp)))
sum(scores$score + scores$score2)
head(scores, 10)


# Part II -----------------------------------------------------------------

base = strtoi(charToRaw("A"), 16L)
asc <- function(x) { strtoi(charToRaw(x),16L) }
chr <- function(n) { rawToChar(as.raw(n)) }
#lose X
lose <- function(znak) { chr(base + (asc(znak) %% base - 1) %% 3) }
# win Z
win <- function(znak) { chr(base + (asc(znak) %% base + 1) %% 3) }
# draw Y
draw <- function(znak) znak

cbind(scores, P2b = unlist(map2(scores$P1, scores$P2, function(x,y) {
  if (y == "X") lose(x)
  else if (y == "Y") draw(x)
  else win(x)
}))) -> scores

scores |> mutate(score3 = (P2b == "A") * 1 + (P2b == "B") * 2 + (P2b == "C") * 3 +
                   (P2 == "Y") * 3 + (P2 == "Z") * 6) -> scores

little <- data.frame(P1 = c("A","B","C"), P2 = c("Y","X","Z"))
cbind(little, P2b = unlist(map2(little$P1, little$P2, function(x,y) {
  if (y == "X") lose(x)
  else if (y == "Y") draw(x)
  else win(x)
}))) |> mutate(score3 = (P2b == "A") * 1 + (P2b == "B") * 2 + (P2b == "C") * 3 +
                 (P2 == "Y") * 3 + (P2 == "Z") * 6)

sum(scores$score3)
head(scores)
