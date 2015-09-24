library("shinychord")

chord_data <- ch_read_delim("file")
chord_list_data_add <- ch_list_add("add", item = "dataset")
chord_list_data_select <- ch_list_select("select", item = "dataset")
chord_list_data_remove <- ch_list_remove("remove", item = "dataset")
