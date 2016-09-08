library(GPIdata)
library(dplyr)

# Resample possible knoedler genres
kg <- knoedler %>%
  select(pi_record_no, genre, sale_date_year, buy_auth_name_1) %>%
  filter(!is.na(sale_date_year) & sale_date_year < 1970)

start_year <- min(kg$sale_date_year)
end_year <- 1970

# Find the current distribution of genres in the original data
genres <- kg %>%
  na.omit() %>%
  count(genre) %>%
  mutate(prob = n/sum(n)) %>%
  arrange(genre)

# Make these genres and their proportions available as vectors
gnames <- genres$genre
gstart <- genres$prob
names(gstart) <- gnames
