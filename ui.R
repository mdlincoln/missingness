library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Handling missing genres"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("What percentage of these records should have missing genre?"),
      sliderInput("percent_missing",
                  "Percent missing",
                  min = 0,
                  max = 1,
                  value = 0.2),
      p("Now, what is the balance of genres that we want to assume when guessing the value of these next ones? If we assume that any genre has an equal chance of missing from the dataset, then all sliders should be set to the same value. If, on the other hand, we assume that one genre has a greater chance of being missing - e.g., that missing paintings have a greater chance of actually being abstract than still life - then we would set a higher value for abstract artworks and a lower value for still lifes."),
      sliderInput("abstract",
                  "Abstract",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("genre",
                  "Genre",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("history",
                  "History",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("landscape",
                  "Landscape",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("portrait",
                  "Portrait",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("still_life",
                  "Still Life",
                  min = 0,
                  max = 1,
                  value = 0.5)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      p("The original distribution of genres over time, including missing values."),
      plotOutput("static_plot"),
      p("The simulated distribution of genres over time, using new probabilities."),
      plotOutput("sim_plot")
    )
  )
))
