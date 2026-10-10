# Shiny

library(shiny)
# Install once from GitHub (run in the console, not in the app):
# pak::pak("JonasDanielsson97/advanced-r-lab05")
library(lab05)


# Load zones data form turf_get_all_zones_data() fun.
# Slow the first time, but cached for later use.
# Falls back to the example data.
zones_data <- tryCatch(
  turf_get_all_zones_data(),
  error = function(e) {
    message("Could not get zones data, using example data instead")
    zones_all
  }
)



ui <- fluidPage(
  titlePanel("Turf users online"),
  sidebarLayout(
    # Sidebar with current number of users online, Leaderboard below
    sidebarPanel(
      "Online now",
      h1(strong(textOutput("current"))),
      hr(), # Line between the number and the leaderboard
      "Leaderboard",
      selectInput("country", "Country", choices = c("se", "fi", "no", "dk")),
      numericInput("to", "Number of players", value = 10, min = 1, max = 50),
      tableOutput("top")
    ),
    # Main panel with plot of users online over time, search below
    mainPanel(
      plotOutput("plot"),
      hr(),

      # Search for one user
      textInput("name", "Search user"),
      actionButton("search", "Search"),
      tableOutput("user"),
      hr(),

      # Map of zones and active players around a city
      textInput("city", "City", value = "Linköping"),
      sliderInput("radius", "Radius (km)", min = 1, max = 50, value = 10),
      actionButton("show_map", "Show map"),
      leaflet::leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  # Empty data that will fill up periodicly
  history <- reactiveVal(data.frame(time = Sys.time()[0], users = integer()))

  # observe() runs the code inside it repeatedly
  observe({
    invalidateLater(5000) # Run this block again in 5 seconds
    # Request the statistics from the Turf API, trycatch() to avoid crashing if fail
    stats <- tryCatch(turf_statistics(), error = function(e) NULL)
    req(stats) # If the request failed, skip this time and try again later

    # Adds new row to data frame
    new <- data.frame(time = Sys.time(), users = stats$usersOnline)
    history(rbind(isolate(history()), new))
  })

  output$current <- renderText({
    req(nrow(history()) > 0)
    tail(history()$users, 1) # The latest value
  })

  output$plot <- renderPlot({
    req(nrow(history()) > 0) # Wait until the first value has arrived
    plot(history()$time, history()$users, type = "b",
         xlab = "Time", ylab = "Users online")
  })

 # Leaderboard table, This snapshots when left alone, might wanna change
  output$top <- renderTable({
      top <- turf_top(1, input$to, country = input$country)
      top[, c("place", "name", "points")]
  })


  # User search
  # eventReactive() only runs when the Search button is clicked
  user <- eventReactive(input$search, {     # $search answer to the actionButton() in the UI
    req(input$name) # Do nothing if the box is empty
    # warn = FALSE, the "User not found" message below handles it instead
    tryCatch(turf_users(input$name, warn = FALSE), error = function(e) NULL)
  })

  output$user <- renderTable({
    # Not found gives an empty list, a failed request gives NULL
    validate(need(length(user()) > 0, "User not found"))
    stats <- list(
      name = user()$name,
      region = user()$region$name, # region is a nested data frame
      place = user()$place,
      points = user()$points,
      pointsPerHour = user()$pointsPerHour,
      taken = user()$taken,
      uniqueZonesTaken = user()$uniqueZonesTaken
    )
    stats[lengths(stats) == 0] <- NA # fixes empty values
    as.data.frame(stats)
  })


  # Map
  # Only runs when the Show map button is clicked
  output$map <- leaflet::renderLeaflet({
    req(input$show_map > 0)
    # isolate() so changing the city or radius doesn't redraw until the button is clicked
    isolate({
      req(input$city)
      zones_data |>
        display_zones_and_active_players(
          address = input$city,
          turfarea_radius = input$radius
        )
    })
  })

}

shinyApp(ui, server)
