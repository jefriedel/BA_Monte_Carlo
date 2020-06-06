old_column = "Subject Number"
new_name = "New Subject Number"

col_descript[col_names==old_column] <<- new_name

tibble_in