library("tibble")
library("dplyr")

list_a <- list(a = "1", b = NULL, c = list(a = "1", b = "2"))
list_b <- list(a = "2", b = "3", c = list(a = "3", b = "4"))

list_ap <- list(a = "1", b = NA_character_, c = list(list(a = "1", b = "2")))
list_bp <- list(a = "2", b = "3", c = list(list(a = "3", b = "4")))

tibble_a <- tibble(a = "1", b = NA_character_, c = list(list(a = "1", b = "2")))
tibble_b <- tibble(a = "2", b = "3", c = list(list(a = "3", b = "4")))

df_a <- data.frame(a = "1", b = NA_character_, c = list(list(a = "1", b = "2")))
df_b <- data.frame(a = "2", b = "3", c = list(list(a = "3", b = "4")))

tibble_ab <- bind_rows(tibble_a, tibble_b)
df_ab <- rbind(df_a, df_b)

test_that("prepare_list works", {
  
  expect_identical(prepare_list(list_a), list_ap)
  expect_identical(prepare_list(list_b), list_bp)
  
})

test_that("stack_row works", {
  
  expect_identical(stack_row_tbl(list_a), tibble_a)
  expect_identical(stack_row_tbl(list_b), tibble_b)
  
  expect_identical(stack_row_df(list_a), df_a)
  expect_identical(stack_row_df(list_b), df_b)
  
})

test_that("stack_rows works", {
  
  expect_identical(stack_rows_tbl(list(list_a, list_b)), tibble_ab)
  expect_identical(stack_rows_df(list(list_a, list_b)), df_ab)
  
})


test_that("as_box_id works", {
  
  expect_identical(as_box_id(c(1, 2, 3)), c("1", "2", "3"))
  expect_identical(as_box_id(1407651371263), "1407651371263")
  
  # TODO: update to expect_snapshot_error() when we use testthat 3e
  expect_error(as_box_id("foo"), class = "boxr_id")
  expect_error(as_box_id(3.4), class = "boxr_id")
  
})
