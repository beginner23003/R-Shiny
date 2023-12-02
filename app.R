library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
# load data sets
listings <- read.csv("listings.csv")
neighbourhoods <- read.csv("neighbourhoods.csv")

# data set cleaning
listings <- listings |>
  mutate(star = ifelse(grepl("★", name), sub(".*★(\\d+\\.\\d+).*", "\\1", name), NA),
         .before = name,
         star = as.numeric(star,  na.rm = TRUE)) |>
  mutate(lease_type = if_else(minimum_nights <= 10, "short", "long"),
         license_type = if_else(license == "", "unlicensed", if_else(
           license == "Exempt", "exempt", "licensed"
         ))
  ) |>
  mutate(id = as.character(id))



# ui 
ui <- fluidPage(
  # page 0 - introduction
  navbarPage(
    "Airbnb listings in NYC", 
    tabPanel("Introduction",
             
             tags$style(type = "text/css", 
                        "
                          body{
                          background: #11ffee00; 
                          background-size: cover;
                          background-repeat: no-repeat;
                          background-attachment: fixed;
                          color: #191919;
                          margin: 0;
                          font-family: 'Georgia', serif;
                          style: 'border-radius: 50%;'
                          }
                          .bottom-rectangle {
                            position: absolute;
                            bottom: 0;
                            left: 0;
                            width: 100%;
                            background: #333;
                            color: #fff;
                            text-align: center;
                            padding: 30px;
                            font-family: 'URW Chancery L', cursive;
                          }
                          .author-info {
                            display: flex;
                            justify-content: center; 
                            align-items: center; 
                            font-family: 'Chalkduster', fantasy;
                          }
                          .author-info-text {
                            color: rgb(8, 51, 68);
                            margin-left: 20px;
                          }
              "),
             class = "center-box",
             style = "text-align: center;",
             div(
               style = "text-align: center;",
               img(src = "https://i.gifer.com/I66V.gif", height = "230px", width = "60%", style = "border-radius: 8px;")
             ),
             h2("Welcome to the NYC Listings Searching App"),
             p("Explore and analyze Airbnb listings in New York City!"),
             p("Navigate through different tabs to discover information about neighborhoods, listing numbers, and popular listings."),
             p("Use filters to customize your search."),
             p("Enjoy exploring!"),
             br(),
             
             div(
               style = "text-align: center;",
               div(
                 class = "author-info",
                 img(src = "https://i.imgur.com/S5yLiVh.jpeg", height = "150px", width = "200px", style = 'border-radius: 50%;'),
                 div(
                   class = "author-info-text",
                   p("Author: "),
                   p("Rapidash")
                 )
               )
             ),
             
             absolutePanel(class = "bottom-rectangle",
                           "Disclaimer: This app is for educational purposes and does not provide real-time data."
             )
    ),
    
    # first page
    tabPanel("Map",  
             
             titlePanel("New York City Airbnb Listing Map"),
             h4("Click on blue points on the map for listing details!"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("neighborhood_group", "Select Neighborhood Group:",
                             choices = c("all", unique(listings$neighbourhood_group))),
                 selectizeInput("neighborhood", "Select Neighborhood:",
                                choices = unique(listings$neighbourhood),
                                multiple = TRUE),
                 selectInput("price_range", "Select Price Range:", 
                             choices = c("all", "Under $200", "$200-$500", "$500-$1000", "Over $1000"),
                             multiple = FALSE,
                             selected = "all"),
                 selectInput("room_type", "Select Room Type:", 
                             choices = c("all", "Entire home/apt", "Private room", "Shared room", "Hotel room")),
                 selectInput("min_nights_range", "Select Minimum Nights Range:",
                             choices = c("all", "Short Term", "Long Term"),
                             selected = "all"),
                 sliderInput("availability_slider", "Select Availability Range:",
                             min = 0, max = 365, value = c(0, 365)),
                 actionButton("reset", "Reset Selection")
               ),
               mainPanel(
                 leafletOutput("map"),
                 dataTableOutput("results_table")
               )
             )
    ),
    
    # second page
    tabPanel("Listing Number & Star",
             
             #h2("Listing Number by Neighbourhood Group"),
             checkboxGroupInput("room_type_plot", "Select Room Type:",
                                choices = c("all", unique(listings$room_type)),
                                inline = TRUE,
                                selected = "all"),
             checkboxGroupInput("license_type_plot", "Select License Type:",
                                choices = c("all", unique(listings$license_type)),
                                inline = TRUE,
                                selected = "all"),
             checkboxGroupInput("lease_type_plot", "Select Lease Type:",
                                choices = c("all", unique(listings$lease_type)),
                                inline = TRUE,
                                selected = "all"),
             actionButton("reset_filters_plot", "Reset Filters"),
             
             plotlyOutput("bar_chart_plot"),
             plotlyOutput("jitter_plot")
             
    ),
    # third page
    tabPanel("Most Popular Listings",
             sidebarLayout(
               sidebarPanel(
                 selectInput("neighbourhood_group_top", "Select Neighbourhood Group:",
                             choices = c("all", unique(listings$neighbourhood_group))),
                 selectizeInput("neighbourhood_top", "Select Neighbourhood:",
                                choices = unique(listings$neighbourhood),
                                multiple = TRUE),
                 actionButton("reset_filters_top", "Reset Filters")
               ),
               mainPanel(
                 fluidRow(
                   column(12, h3("Top 20 Listings with Most Reviews"))
                 ),
                 DTOutput("top_listings_table") 
               )
             )
    )
  )
)


server <- function(input, output, session) {
  
  
  # page 1
  observeEvent(input$neighborhood_group, {
    selected_neighbourhood_group <- input$neighborhood_group
    available_neighbourhoods <- neighbourhoods$neighbourhood[neighbourhoods$neighbourhood_group == selected_neighbourhood_group]
    updateSelectizeInput(session, "neighborhood", choices = c("", available_neighbourhoods))
  })
  
  # page 3
  observeEvent(input$neighbourhood_group_top, {
    selected_neighbourhood_group <- input$neighbourhood_group_top
    available_neighbourhoods <- neighbourhoods$neighbourhood[neighbourhoods$neighbourhood_group == selected_neighbourhood_group]
    updateSelectizeInput(session, "neighbourhood_top", choices = c("", available_neighbourhoods))
  })
  
  
  # reset page 1
  observeEvent(input$reset, {
    updateSelectInput(session, "neighborhood_group", selected = "all")
    updateSelectizeInput(session, "neighborhood", selected = "")
    updateSelectizeInput(session, "price_range", selected = "all")
    updateSelectInput(session, "room_type", selected = "all")
    updateSelectInput(session, "min_nights_range", selected = "all")
    updateSliderInput(session, "availability_slider", value = c(0, 365))
  })
  
  # reset page 2
  observeEvent(input$reset_filters_plot, {
    updateSelectInput(session, "room_type_plot", selected = "all")
    updateSelectInput(session, "license_type_plot", selected = "all")
    updateSelectInput(session, "lease_type_plot", selected = "all")
  })
  
  # reset page 3
  observeEvent(input$reset_filters_top, {
    updateSelectInput(session, "neighbourhood_group_top", selected = "all")
    updateSelectizeInput(session, "neighbourhood_top", selected = "")
  })
  
  # filter data page 1
  filtered_data <- reactive({
    data <- listings
    
    
    if (input$neighborhood_group != "all") {
      data <- data %>% filter(neighbourhood_group %in% input$neighborhood_group)
    }
    
    
    if ("" %in% input$neighborhood) {
      # Do not perform neighborhood filtering
    } else {
      data <- data %>% filter(neighbourhood %in% input$neighborhood)
    }
    
    
    if ("" %in% input$price_range) {
      # Do not perform price filtering
    } else {
      data <- data %>% filter(price >= 0)  # Avoid filtering no data when conditions are empty
      if ("Under $200" %in% input$price_range) {
        data <- data %>% filter(price < 200)
      }
      if ("$200-$500" %in% input$price_range) {
        data <- data %>% filter(price >= 200, price <= 500)
      }
      if ("$500-$1000" %in% input$price_range) {
        data <- data %>% filter(price > 500, price <= 1000)
      }
      if ("Over $1000" %in% input$price_range) {
        data <- data %>% filter(price > 1000)
      }
    }
    
    # Filter data based on selected room type
    if (input$room_type != "all") {
      data <- data %>% filter(room_type %in% input$room_type)
    }
    
    # Filter data based on selected minimum nights range
    if (input$min_nights_range != "all") {
      data <- data %>% filter(minimum_nights >= 0)  # Avoid filtering no data when conditions are empty
      if (input$min_nights_range == "Short Term") {
        data <- data %>% filter(minimum_nights < 10)
      } else if (input$min_nights_range == "Long Term") {
        data <- data %>% filter(minimum_nights >= 10)
      }
    }
    
    data <- data %>% filter(availability_365 >= input$availability_slider[1],
                            availability_365 <= input$availability_slider[2])
    return(data)
  })
  
  # map page 1
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = filtered_data(),
                       lat = ~latitude, lng = ~longitude,
                       radius = 3, clusterOptions = markerClusterOptions(),
                       popup = ~paste("Name: ", ifelse(is.na(name), "N/A", name),
                                      "<br>ID: ", ifelse(is.na(id), "N/A", id),
                                      "<br>Price: $", ifelse(is.na(price), "N/A", price),
                                      "<br>Star: ", ifelse(is.na(star), "N/A", star),
                                      "<br>Host Name: ", ifelse(is.na(host_name), "N/A", host_name),
                                      "<br>Number of Reviews: ", ifelse(is.na(number_of_reviews), "N/A", number_of_reviews),
                                      '<br><a href="https://www.google.com/maps?q=', latitude, ',', longitude, '" target="_blank">Open in Google Map</a>'
                       ))
  })
  
  # table page 1
  output$results_table <- renderDataTable({
    
    filtered_data() %>% select(id, name, star, neighbourhood, neighbourhood_group, room_type, price, minimum_nights)
  },
  options = list(
    lengthMenu = c(5, 10, 25, 50), 
    pageLength = 5  
  ))
  
  # filter page 2
  filtered_data_plot <- reactive({
    data <- listings
    
    if (!"all" %in% input$room_type_plot) {
      data <- data[data$room_type %in% input$room_type_plot, ]
    }
    
    if (!"all" %in% input$license_type_plot) {
      data <- data[data$license_type %in% input$license_type_plot, ]
    }
    
    if (!"all" %in% input$lease_type_plot) {
      data <- data[data$lease_type %in% input$lease_type_plot, ]
    }
    
    return(data)
  })
  
  # group page 2
  grouped_data_plot <- reactive({
    
    grouped <- aggregate(id ~ neighbourhood_group + room_type + star, data = filtered_data_plot(), FUN = length)
    return(grouped)
  })
  
  # plot page 2
  output$bar_chart_plot <- renderPlotly({
    
    p <- ggplot(data = grouped_data_plot(), aes(x = neighbourhood_group, y = id, fill = room_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")  
    
    p2 <- ggplot(data = grouped_data_plot(), aes(x = room_type, y = star)) +
      geom_jitter(color = "orange") +
      theme_minimal() +
      labs(
        x = "Room type of New York City",
        y = "Star",
        title = "New York City"
        
      ) 
    
    #ggplotly(p)
    subplot(p, p2, nrows = 1)
  })
  
  # filter page 3
  filtered_data_top <- reactive({
    data <- listings
    
    
    if (input$neighbourhood_group_top != "all") {
      data <- data %>% filter(neighbourhood_group == input$neighbourhood_group_top)
    }
    
    
    if ("" %in% input$neighbourhood_top) {
      # Do not perform neighborhood filtering
    } else {
      data <- data %>% filter(neighbourhood %in% input$neighbourhood_top)
    }
    
    data <- arrange(data, desc(number_of_reviews))
    data <- head(data, 20)
    #data <- data |>
    #  select(id, name, host_name, star, number_of_reviews) 
    return(data)
  })
  
  output$top_listings_table <- DT::renderDataTable({
    data <- filtered_data_top()
    data$google_map_link <- sprintf('<a href="https://www.google.com/maps?q=%f,%f" target="_blank">Open in Google Map</a>', data$latitude, data$longitude)
    
    datatable(data[, c("id", "name", "host_name", "star", "number_of_reviews", "google_map_link")],
              escape = FALSE,
              options = list(
                lengthMenu = c(5, 10, 20),
                pageLength = 10 
              )
    )
  })
  
}

shinyApp(ui = ui, server = server)
