source("setwd.R")
library(purrr)

# Part I ------------------------------------------------------------------

f <- scan(file = "data/input01.txt", blank.lines.skip = FALSE)
f

out <- 0
purrr::walk(
  f,
  function(x) {
    if (is.na(x)) out <<- c(out, 0)
    else {
      out[length(out)] <<- out[length(out)] + x
    }
  }
)
max(out)


# Part II -----------------------------------------------------------------

sum(head(sort(out, decreasing = TRUE), 3))
