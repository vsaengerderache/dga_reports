percent_no_na <- q_daily %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise_at(vars(-date), ~ sum(!is.na(.)) / length(.) * 100)

q_daily_mean <- q_daily %>%
  mutate(date = yday(date)) %>%
  group_by(date) %>%
  summarise(across(.cols = where(is.numeric), .fns = ~mean(.x, na.rm = TRUE)))

q_month_mean <- q_daily %>%
  mutate(date = month(date)) %>%
  group_by(date) %>%
  summarise(across(.cols = where(is.numeric), .fns = ~mean(.x, na.rm = TRUE)))

q_year_mean <- q_daily %>%
  summarise(across(.cols = where(is.numeric), .fns = ~mean(.x, na.rm = TRUE)))
