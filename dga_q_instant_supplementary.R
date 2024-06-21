q_max_year <- q_instant %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -Inf, NA, .)))

write.csv(q_instant, "q_instant.csv", row.names = F)
write.csv(q_max_year, "q_max_year.csv", row.names = F)
