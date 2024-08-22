# Load your libraries
library(shiny) # Allows us to build a shinyapp
library(tidyverse) # Allows for data manipulation
library(janitor) # Allows for data cleaning
library(plotly) # Gives interactive plots

# Defining UI
ui <- fluidPage(
  
  # Creating a title
  titlePanel("Mount Tremper Donor Analysis"),
  
  # Defining the sidebar layout
  sidebarLayout(
    
    # Creating a panel inside the sidebar layout
    sidebarPanel(
      
      # Creating an input slider
      sliderInput("dateRange",
                  "Select a Year Range:",
                  min = min(df$year),
                  max = max(df$year),
                  value = c(min(df$year), max(df$year)),
                  step = 1)
    ),
    
    # Defining the main panel
    mainPanel(
      
      # Defining the tab panels
      tabsetPanel(
        
        # Category panel
        tabPanel("Category",
                 plotlyOutput("categoryPlot")),
        
        # Party panel
        tabPanel("Party",
                 plotlyOutput("partyPlot")),
        
        # Location panel
        tabPanel("Location",
                 plotlyOutput("locationPlot")),
        
        # Table Panel
        tabPanel("Table",
                 DTOutput("statsTable"))
      )
    )
  )
)

# Defining the server
server <- function(input, output) {
  
  # Filtering our database on our input slider
  filtered_data <- reactive({
    df %>%
      filter(year >= input$dateRange[1] & year <= input$dateRange[2])
  })
  
  # Defining our output table
  output$statsTable <- renderDT({
    filtered_data()
  })
  
  # Defining our category plot
  output$categoryPlot <- renderPlotly({
    p <- filtered_data() %>%
      group_by(category) %>%
      summarize(n = sum(amount)) %>%
      ggplot(aes(x = reorder(category, n), y = n, fill = category)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Donations by Category", x = "Category", y = "Sum") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Defining our party plot
  output$partyPlot <- renderPlotly({
    p <- filtered_data() %>%
      group_by(party) %>%
      summarize(n = sum(amount)) %>%
      ggplot(aes(x = reorder(party, n), y = n, fill = party)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Donations by Party", x = "Party", y = "Sum") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Defining our location plot
  output$locationPlot <- renderPlotly({
    p <- filtered_data() %>%
      group_by(recipient_jurisdiction) %>%
      summarize(n = sum(amount)) %>%
      ggplot(aes(x = reorder(recipient_jurisdiction, n), y = n, fill = recipient_jurisdiction)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Donations by Location", x = "Location", y = "Sum") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Running the application
shinyApp(ui = ui, server = server)










