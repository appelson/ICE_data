# Cleaning
detention_data <- read_parquet("output_file.parquet") %>%
  clean_names() %>%
  mutate(
    stay_id = as.character(md5(paste0(stay_book_in_date_time, unique_identifier))),
    detention_length_days = as.numeric(difftime(detention_book_out_date_time, book_in_date_time, units = "days")),
    detention_quarter = paste0(year(book_in_date_time), "Q", quarter(book_in_date_time)),
    detention_facility = str_to_title(detention_facility),
    final_order_yes_no = str_to_title(final_order_yes_no),
    final_charge = str_to_title(final_charge),
    departure_country = str_to_title(departure_country),
    detention_length_cat = case_when(
      is.na(detention_length_days) ~ "Unknown",
      detention_length_days < 3 ~ "Short (<3 days)",
      detention_length_days < 14 ~ "Medium (3–13 days)",
      detention_length_days < 90 ~ "Long (2 weeks–3 months)",
      detention_length_days >= 90 ~ "Extended (3+ months)"
    ),

  ) %>%
  mutate(across(where(is.character), trimws)) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.) | . == "" | . == "NA", "None Reported", .))) %>%
  left_join(lookup, by = "detention_facility_code") %>%
  mutate(state = ifelse(is.na(state), "Unknown State", state))

write_csv(detention_data, "file.csv")
write_parquet(detention_data, "file.parquet")