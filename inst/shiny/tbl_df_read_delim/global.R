library("shinychord")

chord_tbl_1 <- tbl_df_read_delim("file_1")
chord_tbl_2 <- tbl_df_read_delim("file_2")

chord_list_tbl_1_add <- rctval_add("add_1")
chord_list_tbl_1_remove <- rctval_remove("remove_1")
