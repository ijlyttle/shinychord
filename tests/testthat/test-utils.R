
library("dplyr")
library("readr")
library("lubridate")

# some setup
df_ref <- data_frame(
  int = c(1L, 2L, 3L),
  dbl = c(1, 2, 3),
  char = c("a", "b", "c"),
  dtm_a = ymd("2012-01-02") + hours(seq(1, 3)),
  dtm_b = ymd("2012-01-02") + hours(seq(1, 3))
)

fmt_reg <- stamp("2012-03-04 05:06:07", quiet = TRUE)
fmt_iso <- stamp("2012-03-04T05:06:07Z", quiet = TRUE)

txt_reg <-
  df_ref %>%
  mutate(dtm_a = fmt_reg(dtm_a), dtm_b = fmt_reg(dtm_b)) %>%
  format_csv()

txt_iso <-
  df_ref %>%
  mutate(dtm_a = fmt_iso(dtm_a), dtm_b = fmt_iso(dtm_b)) %>%
  format_csv()

txt_reg_iso <-
  df_ref %>%
  mutate(dtm_a = fmt_reg(dtm_a), dtm_b = fmt_iso(dtm_b)) %>%
  format_csv()

# ISO-8601
str_date <- c("2015-01-02", "20150102")
str_delim <- c("T", " ")
str_time <- c(
  "03:04:05.678", "030405.678",
  "03:04:05", "030405",
  "03:04", "0304",
  "03"
)
str_zone <- c("Z", "+0200", "-0200", "+02:00", "-02:00", "+02", "-02")

str_iso_8601 <-
  expand.grid(date = str_date, delim = str_delim, time = str_time, zone = str_zone) %>%
  tbl_df() %>%
  mutate(string = paste0(date, delim, time, zone)) %>%
  `[[`("string")

str_not_iso_8601 <- c(
  "hello",
  "2019-09-27 21:47:00"
)


context("df_has_numeric")

test_that("we can detect dataframes with numeric columns", {
  expect_true(df_has_numeric(mtcars))
  expect_true(df_has_numeric(df_ref))
  expect_false(df_has_numeric(df_ref[c("char", "dtm_a")]))
})

context("df_has_time")

test_that("we can detect dataframes with datetime columns", {
  expect_false(df_has_time(mtcars))
  expect_true(df_has_time(df_ref))
  expect_false(df_has_time(df_ref[c("char", "int", "dbl")]))
})

context("df_has_time_8601")

test_that("ISO-8601 regular expression works", {
  expect_true(all(is_time_8601(str_iso_8601)))
  expect_false(any(is_time_8601(str_not_iso_8601)))
})

test_that("we detect non-iso 8601 in dataframes", {
  expect_true(df_has_time_non_8601(df_ref, txt_reg, ","))
  expect_false(df_has_time_non_8601(df_ref, txt_iso, ","))
  expect_true(df_has_time_non_8601(df_ref, txt_reg_iso, ","))
})

