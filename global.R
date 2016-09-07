library(GPIdata)
library(dplyr)

# Resample possible knoedler genres
kg <- knoedler %>%
  select(pi_record_no, genre, sale_date_year, buy_auth_name_1) %>%
  filter(!is.na(sale_date_year))



gnames <- sort(na.omit(unique(kg$genre)))
