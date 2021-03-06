library(shiny)

section_missing <- div(
  p("A sizable number of Knoedler records are missing crucial information, such as precise sale dates, prices, and artwork genres. We will encounter this same issue in artist and collector life dates, auction sale dates, and so forth. Normally, we would simply discard those records when doing analyses that require the presence of those values. However, simply discarding records means that we would base our summary claims (about, say, the influence of artwork genre on sale price) on a small sample of all the sales that we know did, indeed, take place. How can we determine whether those missing records might invalidate the conclusions we draw?"),
  p("One intuitive way to address this issue is through what is known as multiple imputation, in which we articulate informed guesses at what those missing values might be, and then run dozens or hundreds of simulations that stochastically generate values for those missing records within the boundaries set by those guesses, and then return a range of likely results (using whichever metric we were computing in the first place) that take in to account the uncertainty produced by those missing values."),
  tags$hr(),
  p("The original Knoedler data have very few missing gnere labels. For the purposes of demonstration, we can randomly add a few more. What additinoal percentage of these records should have missing genre?"),
  inputPanel(sliderInput("percent_missing", "Percent missing", min = 0, max = 1, value = 0, step = 0.1)),
  p("The original distribution of genres over time, including missing values."),
  plotOutput("static_plot", width = "800px"))

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
  p("Below, missing values have been replaced randomly, following the weights for each genre set above."),
  plotOutput("sim_plot", width = "800px"))

section_simulated <- div(
  p("Because the missing value replacement process is randomized, we can't just do it once. We need to repeat it many times over, generating a ", tags$em("range"), " of possible values."),
  p("To reduce the noise from year-to-year fluctuations, we can also use a moving window average to smooth the results."),
  inputPanel(
    sliderInput("n_boot", "Bootstrap iterations", min = 1, max = 100, value = 1, step = 10),
    sliderInput("window_size", "Rolling window size", min = 1, max = 20, value = 10),
    actionButton("calc", "Simulate!")),
  textOutput("window_number"),
  p("The black lines represent the values returned by each individual simulation. The red line is the median value of each annual result."),
  plotOutput("div_plot", width = "800px"))

shinyUI(fluidPage(
  # Application title
  titlePanel("Handling missing genres"),
  section_missing,
  section_example,
  section_simulated))
