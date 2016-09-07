library(GPIdata)
library(dplyr)

# Resample possible knoedler genres
kg <- knoedler %>%
  select(pi_record_no, genre, sale_date_year, buy_auth_name_1) %>%
  filter(!is.na(sale_date_year) & sale_date_year < 1970)

genres <- kg %>%
  na.omit() %>%
  count(genre) %>%
  mutate(prob = n/sum(n)) %>%
  arrange(genre)

gnames <- genres$genre
gstart <- genres$prob
names(gstart) <- gnames
