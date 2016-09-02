library(shiny)
library(ggplot2)
library(dplyr)
library(vegan)
library(bootr)
library(GPIdata)

shinyServer(function(input, output) {

  # Resample possible knoedler genres
  kg <- knoedler %>%
    select(pi_record_no, genre, sale_date_year, buy_auth_name_1) %>%
    filter(!is.na(sale_date_year))

  gnames <- na.omit(unique(kg$genre))
  gprobs <- c(1, 1, 1, 1, 1, 1)

  # Generate a sample of all new genres based on the given probabilities
  sim_genres <- sample(gnames, prob = gprobs, size = nrow(kg), replace = TRUE)

  # Where a genre is missing, copy in from sim_genres
  gdist <- kg %>%
    mutate(filled_genre = ifelse(is.na(genre), sim_genres, genre))

  calc_diversity <- function(df, subject, object, count_cutoff) {
    div_df <- df %>%
      count_(c(subject, object)) %>%
      group_by_(subject) %>%
      summarize(
        div = diversity(n, index = "shannon"),
        all_objects = sum(n)) %>%
      filter(all_objects >= count_cutoff)

    attr(div_df, "subject") <- subject
    attr(div_df, "object") <- object

    return(div_df)
  }

  calc_detrend <- function(df) {
    lm(div ~ log(all_objects), data = df)
  }

  # Use the model from calc_detrend() to calculate the predicted diversity, and
  # then subtract that from the actual diversity
  add_detrend <- function(df, model) {
    df %>%
      mutate(
        expected_div = predict(model, data.frame(all_objects = all_objects)),
        div_diff = div - expected_div)
  }

  # Create a wrapper method that takes the output of calc_diversity() and
  # intelligently reads it's 'subject' and 'object' attributes in order to
  # preserve them through add_detrend()'s mutate()
  detrend_diversity <- function(df) {

    subject <- attr(df, "subject")
    object <- attr(df, "object")

    div_model <- calc_detrend(df)

    amended_df <- add_detrend(df, div_model)

    attr(amended_df, "subject") <- subject
    attr(amended_df, "object") <- object

    return(amended_df)
  }

  run_window <- function(k, range, window, div_fun, model) {
    data_frame(window_center = range, window_start = window_center - window, window_end = window_center) %>%
      group_by(window_center, window_start, window_end) %>%
      do(filter(k, between(sale_year, .$window_start, .$window_end))) %>%
      do(div_fun(.)) %>%
      ungroup() %>%
      add_detrend(model = model)
  }

  # range: a vector of start_year:end_year
  # window: the size of the rolling window
  # div_fun: A function wrapping calc_diversity() with the desired subject, object, and count_cutoff values
  div_over_time <- function(k, range, window, div_fun) {

    whole_div_model <- k %>%
      div_fun() %>%
      lm(div ~ log(all_objects), data = .)

    run_window(k, range, window, div_fun, whole_div_model)
  }

  simple_window_div <- function(k, range, window) {
    data_frame(window_center = range, window_start = window_center - window, window_end = window_center) %>%
      group_by(window_center, window_start, window_end) %>%
      do(filter(k, between(sale_year, .$window_start, .$window_end))) %>%
      ungroup() %>%
      count(window_center, genre) %>%
      group_by(window_center) %>%
      summarize(annual_div = diversity(n, index = "shannon"))
  }

  output$distPlot <- renderPlot({

  })

})
