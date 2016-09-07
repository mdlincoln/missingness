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
      mutate(genre = if_else(na_indices(), true = NA_character_, false = genre))
  })

  # Create a vector of probabilities for the newly assigned genre values
  gprobs <- reactive({
    c(input$abstract, input$genre, input$history, input$landscape,
      input$portrait, input$still_life)
  })

  # Simulation ----

  sim_df <- reactive({
    # Generate a sample of all new genres based on the given probabilities
    sim_genres <- sample(gnames, prob = gprobs(), size = nrow(kna()), replace = TRUE)
    # Produce a version of kna() with all NA genres filled in. Works with an
    # assigned genre value will keep that value.
    kna() %>% mutate(genre = if_else(is.na(genre), sim_genres, genre))
  })

  boot_df <- reactive({
    withProgress(message = "Simulating missing values...", value = 0, {
      boot_list <- map_df(seq_len(input$n_sims), function(x) {
        incProgress(1/input$n_sims, detail = paste0(x, " of ", input$n_sims, " iterations..."))
        # Generate a sample of all new genres based on the given probabilities
        sim_genres <- sample(gnames, prob = gprobs(), size = nrow(kna()), replace = TRUE)
        # Produce a version of kna() with all NA genres filled in. Works with an
        # assigned genre value will keep that value.
        kna() %>% mutate(genre = if_else(is.na(genre), sim_genres, genre))
      }, .id = "iteration")

      boot_div <- function(df) {
        df %>%
          group_by(iteration, sale_date_year) %>%
          summarize(div = diversity(n, index = "shannon"))
      }

    setProgress(value = input$n_sims, message = "Summarizing diversity boundaries...", detail = NULL)
    # Reduce the boot_list in order to calculate variables
    boot_list %>%
      count(iteration, sale_date_year, genre) %>%
      group_by(iteration, sale_date_year) %>%
      bootstrap(m = input$n_boot) %>%
      do(boot_div(.)) %>%
      group_by(sale_date_year) %>%
      summarize(
        boot_low = quantile(div, 0.025),
        boot_med = quantile(div, 0.5),
        boot_high = quantile(div, 0.975)
      )
    })
  })

  # Plots ----

  output$static_plot <- renderPlot({
    ggplot(kna(), aes(x = sale_date_year, fill = genre)) +
      geom_histogram(binwidth = 3) +
      scale_fill_brewer(type = "qual", na.value = "gray50")
  })

  output$sim_plot <- renderPlot({
    ggplot(sim_df(), aes(x = sale_date_year, fill = genre)) +
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
        geom_ribbon(aes(ymin = boot_low, ymax = boot_high), alpha = 0.5) +
        geom_line(aes(y = boot_med))
    })
  })

})
