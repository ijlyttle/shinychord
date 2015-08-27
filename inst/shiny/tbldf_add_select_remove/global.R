library("shinychord")

chord_tbl <- tbldf_read_delim("file")
chord_list_tbl_add <- rctval_add("add", item = "dataset")
chord_list_tbl_select <- rctval_select("select", item = "dataset")
chord_list_tbl_remove <- rctval_remove("remove")
