source("setwd.R")
library(purrr)
library(stringr)
library(dplyr)

# Part I ------------------------------------------------------------------
monkeys_inspection <- rep(0, 8)

monkeys <- list(
  c(63, 57),
  c(82, 66, 87, 78, 77, 92, 83),
  c(97, 53, 53, 85, 58, 54),
  c(50),
  c(64, 69, 52, 65, 73),
  c(57, 91, 65),
  c(67, 91, 84, 78, 60, 69, 99, 83),
  c(58, 78, 69, 65)
)

monkey_operation <- c(
  function(old) old * 11,
  function(old) old + 1,
  function(old) old * 7,
  function(old) old + 3,
  function(old) old + 6,
  function(old) old + 5,
  function(old) old * old,
  function(old) old + 7
  )

monkey_test <-
  c(
    function(worry) ifelse(worry %% 7 == 0, 7, 3),
    function(worry) ifelse(worry %% 11 == 0, 6, 1),
    function(worry) ifelse(worry %% 13 == 0, 5, 4),
    function(worry) ifelse(worry %% 3 == 0, 2, 8),
    function(worry) ifelse(worry %% 17 == 0, 4, 8),
    function(worry) ifelse(worry %% 2 == 0, 1, 7),
    function(worry) ifelse(worry %% 5 == 0, 3, 5),
    function(worry) ifelse(worry %% 19 == 0, 6, 2)
  )

run <- function() {
  browser()
  monkeys_inspection <<- rep(0, 8)
  for (round in 1:20) {
    for (m in 1:8) {
      monkeys <- inspect(m, monkey_operation[[m]], monkey_test[[m]], monkeys)
    }
  }
  monkeys
}

inspect <- function(m, operation, test, all_monkeys) {
  for (item_worry in all_monkeys[[m]]) {
    all_monkeys[[m]] <- all_monkeys[[m]][-1]
    item_worry <- floor(operation(item_worry) / 3)
    monkeys_inspection[m] <<- monkeys_inspection[m] + 1
    monkey_to <- test(item_worry)
    all_monkeys[[monkey_to]] <- c(all_monkeys[[monkey_to]], item_worry)
  }
  all_monkeys
}

result_monkeys <- run()
Reduce(f = `*`, sort(monkeys_inspection, decreasing = TRUE)[1:2])

# Part II -----------------------------------------------------------------

monkeys_inspection2 <- rep(0, 8)

run2 <- function(runs, monkey_operation, monkey_test, monkeys, ease_func) {
  #browser()
  monkeys_inspection2 <- rep(0, length(monkeys))
  for (round in 1:runs) {
    for (m in 1:length(monkeys)) {
      result <- inspect2(m, monkey_operation[[m]], monkey_test[[m]], monkeys, monkeys_inspection2, ease_func)
      monkeys <- result$monkeys
      monkeys_inspection2 <- result$monkeys_inspection
    }
  }
  list(monkeys = monkeys, monkeys_inspection = monkeys_inspection2)
}

ease <- function(worry) {
  floor(worry / 3)
}

inspect2 <- function(m, operation, test, all_monkeys, monkeys_inspection2, ease_func) {
  for (item_worry in all_monkeys[[m]]) {
    all_monkeys[[m]] <- all_monkeys[[m]][-1]
    item_worry <- operation(item_worry) |> ease_func()
    monkeys_inspection2[m] <- monkeys_inspection2[m] + 1
    monkey_to <- test(item_worry)
    all_monkeys[[monkey_to]] <- c(all_monkeys[[monkey_to]], item_worry)
  }
  list(monkeys = all_monkeys, monkeys_inspection = monkeys_inspection2)
}

result_monkeys2 <- run2(20, monkey_operation, monkey_test, monkeys, ease)
Reduce(f = `*`, sort(result_monkeys2$monkeys_inspection, decreasing = TRUE)[1:2])
result_monkeys_example <- run2(20, monkey_operation_example, monkey_test_example, monkeys_example, ease)
Reduce(f = `*`, sort(result_monkeys_example$monkeys_inspection, decreasing = TRUE)[1:2])

monkeys_example <- list(
  c(79, 98),
  c(54, 65, 75, 74),
  c(79, 60, 97),
  c(74)
)

monkey_operation_example <- c(
  function(old) old * 19,
  function(old) old + 6,
  function(old) old * old,
  function(old) old + 3
)

monkey_test_example <-
  c(
    function(worry) ifelse(worry %% 23 == 0, 3, 4),
    function(worry) ifelse(worry %% 19 == 0, 3, 1),
    function(worry) ifelse(worry %% 13 == 0, 2, 4),
    function(worry) ifelse(worry %% 17 == 0, 1, 2)
  )

easy_by <- function(divider) {
  stopifnot(divider > 0)
  function(worry) floor(worry / divider)
}

mod_by <- function(divider) {
  stopifnot(divider > 0)
  function(worry) floor(worry %% divider)
}

result_monkeys_example <- run2(10000, monkey_operation_example, 
                               monkey_test_example, monkeys_example, mod_by(23*19*13*17))
Reduce(f = `*`, sort(result_monkeys_example$monkeys_inspection, decreasing = TRUE)[1:2])

result_monkeys <- run2(10000, monkey_operation, 
                               monkey_test, monkeys, mod_by(7*11*13*3*17*2*5*19))
Reduce(f = `*`, sort(result_monkeys$monkeys_inspection, decreasing = TRUE)[1:2])
