library(shiny)
library(ggplot2)
library(dplyr)
library(vegan)
library(GPIdata)
library(purrr)

shinyServer(function(input, output) {

  # Prerequisites ----

  na_indices <- reactive({
    sample(x = c(TRUE, FALSE), size = nrow(kg), replace = TRUE,
           prob = c(input$percent_missing, 1 - input$percent_missing))
  })

  # Generate a dataset with artificially missing data
  kna <- reactive({
    kg %>%
      mutate(genre = if_else(na_indices(), true = NA_character_, false = genre)) %>%
      select(sale_date_year, genre)
  })

  # Create a vector of probabilities for the newly assigned genre values
  gprobs <- reactive({
    c(input$abstract, input$genre, input$history, input$landscape,
      input$portrait, input$still_life)
  })

  # Simulation ----

  sim_df <- function(df) {
    # Generate a sample of all new genres based on the given weights
    sim_genres <- sample(gnames, prob = gprobs(), size = nrow(df), replace = TRUE)
    # Produce a version of kna() with all NA genres filled in. Works with an
    # assigned genre value will keep that value.
    df %>% mutate(genre = if_else(is.na(genre), sim_genres, genre))
  }

  boot_df <- reactive({
    withProgress(message = "Simulating missing values...", value = 0, {

      # For each simulation iteration, produce a version of the source data with
      # genres filled in using the sim_df function. Returns a combined dataframe
      # that can be grouped by "iteration"
      na_simmed_df <- map_df(seq_len(input$n_sims), function(x) {
        incProgress(1/input$n_sims, detail = paste0(x, " of ", input$n_sims, " iterations..."))
        sim_df(kna())
      }, .id = "iteration")

      message("Rows of simulated data: ", nrow(na_simmed_df))

      # Produce rolling window before taking bootstrap samples for diversity
      # calculations

      setProgress(message = "Producing windowed replicates...", value = 0, detail = "")

      window_range <- (start_year + input$window_size):end_year

      windowed_list <- map_df(set_names(window_range), function(x) {
        incProgress(amount = 1/length(window_range), detail = paste0("Year: ", x))
        na_simmed_df %>% filter(between(sale_date_year, x - 10, x))
      }, .id = "window_pos") %>%
        mutate(window_pos = as.integer(as.character(window_pos))) %>%
        select(-sale_date_year)

      message("Rows of windowed replicates: ", nrow(windowed_list))

      setProgress(message = "Bootstrapping replicates...", value = 0)
      # For each of the simulation iterations, bootstrap n_boot samples from
      # each year (sampling 80% of records from each year)
      booted_list <- map_df(seq_len(input$n_boot), function(x) {
        incProgress(1/input$n_boot, detail = paste0(x, " of ", input$n_boot, " replicates..."))
        windowed_list %>%
          group_by(iteration, window_pos) %>%
          sample_frac(size = 0.8, replace = TRUE)
      }, .id = "boot")

      message("Rows of bootstrapped replicates: ", nrow(booted_list))

      setProgress(message = paste("Calculating diversity of", format(nrow(windowed_list), big.mark = ","), " replicates..."), detail = "")
      diversities <- booted_list %>%
        count(iteration, boot, window_pos, genre) %>%
        group_by(iteration, boot, window_pos) %>%
        summarize(div = diversity(n, index = "shannon"))

      setProgress(message = "Calculating diversity ranges...")
      diversities %>%
        group_by(window_pos) %>%
        summarize(
          dl = quantile(div, 0.025),
          dm = quantile(div, 0.5),
          dh = quantile(div, 0.975))
    })
  })

  # Plots ----

  output$static_plot <- renderPlot({
    ggplot(kna(), aes(x = sale_date_year, fill = genre)) +
      geom_histogram(binwidth = 3) +
      scale_fill_brewer(type = "qual", na.value = "gray50")
  })

  output$sim_plot <- renderPlot({
    ggplot(sim_df(kna()), aes(x = sale_date_year, fill = genre)) +
      geom_histogram(binwidth = 3) +
      scale_fill_brewer(type = "qual", na.value = "gray50")
  })

  output$div_plot <- renderPlot({

    # Isolate this calculation and plot behind an actionButton
    if (input$calc == 0) {
      stop("Set your inputs, then click \"Calculate!\" to run the simulation and plot the results")
    }

    isolate({
      ggplot(boot_df(), aes(x = window_pos)) +
        geom_ribbon(aes(ymin = dl, ymax = dh), alpha = 0.5) +
        geom_line(aes(y = dm))
    })
  })

})
