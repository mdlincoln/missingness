library(purrr)
library(shiny)
library(ggplot2)
library(gganimate)
library(dplyr)
library(vegan)
library(GPIdata)
library(dateSampler)
library(doFuture)

shinyServer(function(input, output) {

  registerDoFuture()
  plan(multiprocess)

  # Prerequisites ----

  na_indices <- reactive({
    sample(x = c(TRUE, FALSE), size = nrow(kg), replace = TRUE,
           prob = c(input$percent_missing, 1 - input$percent_missing))
  })

  # Generate a dataset with artificially missing data
  kna <- reactive({
    kg %>%
      inner_join(knoedler_stockbook_years, by = "stock_book_no") %>%
      mutate(genre = if_else(na_indices(), true = NA_character_, false = genre)) %>%
      select(stock_book_no, sale_date_year, genre)
  })

  # Create a vector of probabilities for the newly assigned genre values
  gprobs <- reactive({
    c(input$abstract, input$genre, input$history, input$landscape,
      input$portrait, input$still_life)
  })

  # Simulation ----

  # This helper function randomly samples n years from the distribution found in
  # book_no
  generate_years <- function(book_no, n) {
    yp <- yearly_probs[[book_no]]
    sample(yp[["years"]], size = n, replace = TRUE, prob = yp[["probs"]])
  }

  # Given a dataframe, creates n imputed years - replicating years when known, and
  # sampling new ones when unknown.
  impute_year <- function(df, n, year_name = "sale_date_year", book_name = "stock_book_no", rep_name = "year_replicate") {
    new_years <- map2(df[[year_name]], df[[book_name]], function(x, y) {
      if (is.na(x)) {
        generate_years(y, n)
      } else {
        rep(x, times = n)
      }
    }) %>% flatten_int()

    repped_df <- row_rep(df, n = n, .id = rep_name) %>% ungroup()
    repped_df$imputed_year <- new_years
    repped_df
  }

  generate_genres <- function(n) {
    sample(gnames, prob = gprobs(), size = n, replace = TRUE)
  }

  impute_genre <- function(df, n, genre_name = "genre", rep_name = "genre_replicate") {
    new_genres <- map(df[[genre_name]], function(x) {
      if (is.na(x)) {
        generate_genres(n)
      } else {
        rep(x, times = n)
      }
    }) %>% flatten_chr()

    repped_df <- row_rep(df, n = n, .id = rep_name) %>% ungroup()
    repped_df$imputed_genre <- new_genres
    repped_df
  }

  # Bootstrap ----

  boot <- function(df, n, rep_name = "boot_iteration") {
    df %>%
      row_rep(n = n, .id = rep_name) %>%
      group_by(boot_iteration) %>%
      sample_frac(1, replace = TRUE) %>%
      ungroup()
  }

  window_range <- reactive({
    seq(
      from = min(kna()$sale_date_year, na.rm = TRUE) + input$window_size,
      to = max(kna()$sale_date_year, na.rm = TRUE))
  })

  stream_boot <- function(df) {
    imputed_years <- df %>% impute_year(n = 1)
    imputed_genres <- imputed_years %>% impute_genre(n = 1)

    # Roll a window across these new data
    roll_k <- map_df(set_names(window_range()), function(w) {
      imputed_genres %>% filter(between(imputed_year, w - input$window_size, w))
    }, .id = "window_point") %>%
      mutate(window_point = as.integer(as.character(window_point)))

    roll_k %>%
      count(year_replicate, genre_replicate, boot_iteration, window_point, genre) %>%
      group_by(year_replicate, genre_replicate, boot_iteration, window_point) %>%
      summarize(div = diversity(n, index = "shannon")) %>%
      ungroup()
  }


  boot_df <- reactive({

    withProgress({

      setProgress(message = "Bootstrap sampling data..")
      bootstrapped <- boot(kna(), n = input$n_boot)

      setProgress(message = "Imputing missing values...")
      bootstrapped %>%
        group_by(boot_iteration) %>%
        do(stream_boot(.))

      # setProgress(message = "Calculating diversity...")
      # diversities %>%
      #   group_by(window_point) %>%
      #   summarize(
      #     dl = quantile(div, 0.025),
      #     dm = quantile(div, 0.5),
      #     dh = quantile(div, 0.975))
    })
  })

  # Plots ----

  output$static_plot <- renderPlot({
    ggplot(kna(), aes(x = sale_date_year, fill = genre)) +
      geom_histogram(binwidth = 3) +
      scale_fill_brewer(type = "qual", na.value = "gray50")
  })

  output$sim_plot <- renderPlot({
    ggplot(impute_genre(kna(), 1), aes(x = sale_date_year, fill = imputed_genre)) +
      geom_histogram(binwidth = 3) +
      scale_fill_brewer(type = "qual", na.value = "gray50")
  })

  output$div_plot <- renderImage({

    # Isolate this calculation and plot behind an actionButton
    if (input$calc == 0) {
      stop("Set your inputs, then click \"Calculate!\" to run the simulation and plot the results")
    }

    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = ".gif")


    isolate({
      p <- ggplot(boot_df(), aes(x = window_point, y = div, frame = boot_iteration)) +
        geom_line(aes(cumulative = TRUE, group = boot_iteration), alpha = 7 / input$n_boot) +
        geom_line(aes(frame = boot_iteration), color = "red", size = 1)
    })

    withProgress(message = "Rendering plot...", {
      gg_animate(p, "outfile.gif", interval = c(rep(0.1, input$n_boot - 1), 3))
    })

    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
})
