old_column = "Subject Number"
new_name = "New Subject Number"

col_descript[col_descript==old_column] = new_name

new_name = make_clean_names(new_name)

BA_MC_data %>%
  rename(!!as.symbol(new_name) := !!as.symbol(make_clean_names(old_column)))
  