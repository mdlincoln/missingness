library(shiny)

section_missing <- div(
  p("What percentage of these records should have missing genre?"),
  inputPanel(sliderInput("percent_missing", "Percent missing", min = 0, max = 1, value = 0.2)),
  p("The original distribution of genres over time, including missing values."),
  plotOutput("static_plot"))

section_example <- div(
  p("Now, what is the balance of genres that we want to assume when guessing the value of these next ones? If we assume that any genre has an equal chance of missing from the dataset, then all sliders should be set to the same value. If, on the other hand, we assume that one genre has a greater chance of being missing - e.g., that missing paintings have a greater chance of actually being abstract than still life - then we would set a higher value for abstract artworks and a lower value for still lifes."),
  p("The default values for these sliders match the overall ratios of these genres as observed in the original dataset."),
  inputPanel(
    sliderInput("abstract", "Abstract", min = 0, max = 1, value = gstart[["abstract"]]),
    sliderInput("genre", "Genre", min = 0, max = 1, value = gstart[["Genre"]]),
    sliderInput("history", "History", min = 0, max = 1, value = gstart[["abstract"]]),
    sliderInput("landscape", "Landscape", min = 0, max = 1, value = gstart[["Landscape"]]),
    sliderInput("portrait", "Portrait", min = 0, max = 1, value = gstart[["Portrait"]]),
    sliderInput("still_life", "Still Life", min = 0, max = 1, value = gstart[["Still Life"]])),
  p("The simulated distribution of genres over time, using new probabilities."),
  plotOutput("sim_plot"))

section_simulated <- div(
  p("Finally, to calculate the new diversity values, we must specify how many simulation runs to perform"),
  inputPanel(
    sliderInput("n_sims", "Missing data simulations", min = 10, max = 100, value = 50),
    sliderInput("n_boot", "Bootstrap iterations", min = 10, max = 100, value = 10),
    actionButton("calc", "Simulate!")),
  p("A bootstrapped estimation of genre diversity over time"),
  plotOutput("div_plot"))

shinyUI(fluidPage(
  # Application title
  titlePanel("Handling missing genres"),
  section_missing,
  section_example,
  section_simulated))
