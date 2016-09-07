library(shiny)
library(ggplot2)
library(dplyr)
library(vegan)
library(bootr)
library(GPIdata)

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

  # Generate a sample of all new genres based on the given probabilities
  sim_genres <- reactive({
    sample(gnames, prob = gprobs(), size = nrow(kna()), replace = TRUE)
  })

  # Produce a version of kna() with all NA genres filled in. Works with an
  # assigned genre value will keep that value.
  sim_df <- reactive({
    kna() %>% mutate(genre = if_else(is.na(genre), sim_genres(), genre))
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

})
