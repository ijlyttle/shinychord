library("shinychord")
library("stringr")

chord_read_time <-
  ch_input_time_compound(
    id = "time_1",
    label = "label goes here",
    step = c(1, 5),
    default = c(1, 30)
  )
