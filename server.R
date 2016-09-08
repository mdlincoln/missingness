library(shiny)
library(ggplot2)
library(dplyr)
library(vegan)
library(bootr)
library(broom)
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
      na_simmed_df <- map_df(seq_len(input$n_sims), function(x) {
        incProgress(1/input$n_sims, detail = paste0(x, " of ", input$n_sims, " iterations..."))
        sim_df(kna())
      }, .id = "iteration")

      message("Rows of simulated data: ", nrow(na_simmed_df))

      setProgress(message = "Bootstrapping replicates...", value = 0)
      booted_list <- map_df(seq_len(input$n_boot), function(x) {
        incProgress(1/input$n_boot, detail = paste0(x, " of ", input$n_boot, " replicates..."))
        na_simmed_df %>%
          group_by(iteration, sale_date_year) %>%
          sample_frac(size = 0.8, replace = FALSE)
      }, .id = "boot")

      message("Rows of bootstrapped replicates: ", nrow(booted_list))

      setProgress(message = "Calculating diversity of all replicates...", detail = "")
      diversities <- booted_list %>%
        count(iteration, boot, sale_date_year, genre) %>%
        group_by(iteration, boot, sale_date_year) %>%
        summarize(div = diversity(n, index = "shannon"))

      setProgress(message = "Calculating diversity ranges...")
      diversities %>%
        group_by(sale_date_year) %>%
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
      stop("Check your inputs, then click \"Calculate!\" to run the simulation and plot the results")
    }

    isolate({
      ggplot(boot_df(), aes(x = sale_date_year)) +
        geom_ribbon(aes(ymin = dl, ymax = dh), alpha = 0.5) +
        geom_line(aes(y = dm))
    })
  })

})
