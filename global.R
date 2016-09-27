load("knoedler.rda")
library(purrr)
library(dplyr)

row_rep <- function(df, n, .id = "replicate") {
  m <- nrow(df)
  ndf <- df[rep(seq_len(m), each = n),]
  ndf[[.id]] <- rep(seq_len(n), times = m)
  return(ndf)
}

# Resample possible knoedler genres
kg <- knoedler %>%
  filter(transaction == "Sold" & (is.na(sale_date_year) | sale_date_year < 1973)) %>%
  select(pi_record_no, stock_book_no, genre, sale_date_year)

start_year <- min(kg$sale_date_year, na.rm = TRUE)
end_year <- 1973

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

# Find distribution of years across stockbooks
year_counts <- kg %>%
  filter(!is.na(sale_date_year)) %>%
  count(stock_book_no, sale_date_year)

yearly_probs <- map(set_names(1:11), function(x) {
  yr <- filter(year_counts, stock_book_no == x)
  list(years = yr[["sale_date_year"]], probs = yr[["n"]])
})

