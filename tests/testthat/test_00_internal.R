library("tibble")
library("dplyr")

list_a <- list(a = "1", b = NULL, c = list(a = "1", b = "2"))
list_b <- list(a = "2", b = "3", c = list(a = "3", b = "4"))

list_ap <- list(a = "1", b = NA_character_, c = list(list(a = "1", b = "2")))
list_bp <- list(a = "2", b = "3", c = list(list(a = "3", b = "4")))

tibble_a <- tibble(a = "1", b = NA_character_, c = list(list(a = "1", b = "2")))
tibble_b <- tibble(a = "2", b = "3", c = list(list(a = "3", b = "4")))

tibble_ab <- bind_rows(tibble_a, tibble_b)

test_that("prepare_list works", {
  
  expect_identical(prepare_list(list_a), list_ap)
  expect_identical(prepare_list(list_b), list_bp)
  
})

test_that("stack_row works", {
  
  expect_identical(stack_row(list_a), tibble_a)
  expect_identical(stack_row(list_b), tibble_b)
  
})

test_that("stack_rows works", {
  
  expect_identical(stack_rows(list(list_a, list_b)), tibble_ab)

})