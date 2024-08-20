# Load your libraries
library(shiny) # Allows us to build a shinyapp
library(tidyverse) # Allows for data manipulation
library(janitor) # Allows for data cleaning
library(plotly) # Gives interactive plots


boxplot_filtering <- function(data){
  data %>%
    mutate(
      Q1 = quantile(amount, 0.25),
      Q3 = quantile(amount, 0.75),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR
    ) %>%
    filter(amount >= lower_bound & amount <= upper_bound) %>%
    select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)
}

# Define UI
ui <- fluidPage(
  titlePanel("Mount Tremper Donor Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filter Data"),
      sliderInput("dateRange",
                  "Select a Donation Year Range:",
                  min = min(df$year),
                  max = max(df$year),
                  value = c(min(df$year), max(df$year)),
                  step = 1),
      br(),
      h3("Additional Information"),
      p("This website provides an analysis of political donor contributions coming from the Zip Code: 12457. This includes breakdowns by donation category, political party, and recipient location. The data is filtered based on the year range selected."),
      h3("Source"),
      p("This data was scraped from Open Secrets"), tags$a(href="https://www.opensecrets.org/donor-lookup/results?zip=12457&order=desc&sort=D&cycle=", "Source")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Donation Categories",
                 h3("Donations by Category"),
                 p("There are a number of different types of donations. Understanding what donation types people are engaging in may help narrow down the focus of a donation campaign."),
                 selectInput("categoryselector", "Data Selection", choices = c("Category Count", "Total Funding","Funding Distribution")),
                 plotlyOutput("categoryPlot"),
        ),
        tabPanel("Party",
                 h3("Donations by Party"),
                 p("It is crucial to understand which parties people are donating too in this region so that we can set up advocacy campaigns to collect more progressive funding."),
                 selectInput("partyselector", "Data Selection", choices = c("Party Count", "Total Funding","Funding Distribution")),
                 plotlyOutput("partyPlot"),
        ),
        tabPanel("Location",
                 h3("Total Donations by Location"),
                 p("Where are people in this region donating? Is most of it going back into New York or to Federal campaigns? These are important questions to ask to narrow down our donation metrics even further."),
                 selectInput("locationselector", "Data Selection", choices = c("Location Count", "Total Funding","Funding Distribution")),
                 plotlyOutput("locationPlot"),
        ),
        tabPanel("Table",
                 h3("Donation Table"),
                 p("This table provides donation information for individual donors from the 12457 Zip Code."),
                 DTOutput("statsTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    df %>%
      filter(year >= input$dateRange[1] & year <= input$dateRange[2])
  })
  
  output$statsTable <- renderDT({
    filtered_data() %>%
      mutate(recipient = str_to_title(recipient),
             contributor = str_to_title(contributor)) %>%
      select(Category = category,
             Contributor = contributor,
             "Donation Date" = date_cleaned,
             "Donation Amount" = amount,
             Recipient = recipient,
             "Recipient Party" = party,
             "Recipient Jurisdiction" = recipient_jurisdiction) %>%
      datatable(
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          responsive = TRUE,
          scrollX = TRUE
        ),
        width = '100%' 
      )
  })
  
  # Render plot for Category based on selection
  output$categoryPlot <- renderPlotly({
    if (input$categoryselector == "Category Count") {
      p <- filtered_data() %>%
        count(category) %>%
        ggplot(aes(x = reorder(category, n), y = n, fill = category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Number of Donations by Category", x = "Category", y = "Count") +
        theme_minimal()
      
      ggplotly(p)
      
    } else if (input$categoryselector == "Funding Distribution") {
      filtered_data_no_outliers <- filtered_data() %>%
        group_by(category) %>%
        boxplot_filtering()
      
      # Create box plot without outliers
      p <- filtered_data_no_outliers %>%
        ggplot(aes(x = category, y = amount, fill = category)) +
        geom_boxplot(outlier.shape = NA) +
        labs(title = "Distribution of Donations by Category", x = "Category", y = "Funding Amount") +
        theme_minimal()
      
      ggplotly(p)
      
    } else {
      p <- filtered_data() %>%
        group_by(category) %>%
        summarize(n = sum(amount)) %>%
        ggplot(aes(x = reorder(category, n), y = n, fill = category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Total Value of Donations by Category", x = "Category", y = "Sum") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Render plot for Party based on selection
  output$partyPlot <- renderPlotly({
    if (input$partyselector == "Party Count") {
      p <- filtered_data() %>%
        count(party) %>%
        ggplot(aes(x = reorder(party, n), y = n, fill = party)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Number of Donations by Party", x = "Party", y = "Count") +
        theme_minimal()
      
      ggplotly(p)
      
    } else if (input$partyselector == "Funding Distribution") {
      filtered_data_no_outliers <- filtered_data() %>%
        group_by(party) %>%
        boxplot_filtering()
      
      # Create box plot without outliers
      p <- filtered_data_no_outliers %>%
        ggplot(aes(x = party, y = amount, fill = party)) +
        geom_boxplot(outlier.shape = NA) +
        labs(title = "Distribution of Donations by Party", x = "Party", y = "Funding Amount") +
        theme_minimal()
      
      ggplotly(p)
      
    } else {
      p <- filtered_data() %>%
        group_by(party) %>%
        summarize(n = sum(amount)) %>%
        ggplot(aes(x = reorder(party, n), y = n, fill = party)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Total Value of Donations by Party", x = "Party", y = "Sum") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Render plot for Location based on selection
  output$locationPlot <- renderPlotly({
    if (input$locationselector == "Location Count") {
      p <- filtered_data() %>%
        count(recipient_jurisdiction) %>%
        ggplot(aes(x = reorder(recipient_jurisdiction, n), y = n, fill = recipient_jurisdiction)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Number of Donations by Location", x = "Location", y = "Count") +
        theme_minimal()
      
      ggplotly(p)
      
    } else if (input$locationselector == "Funding Distribution") {
      filtered_data_no_outliers <- filtered_data() %>%
        group_by(recipient_jurisdiction) %>%
        boxplot_filtering()
      
      # Create box plot without outliers
      p <- filtered_data_no_outliers %>%
        ggplot(aes(x = recipient_jurisdiction, y = amount, fill = recipient_jurisdiction)) +
        geom_boxplot(outlier.shape = NA) +
        labs(title = "Distribution of Donations by Location", x = "Location", y = "Funding Amount") +
        theme_minimal()
      
      ggplotly(p)
      
    } else {
      p <- filtered_data() %>%
        group_by(recipient_jurisdiction) %>%
        summarize(n = sum(amount)) %>%
        ggplot(aes(x = reorder(recipient_jurisdiction, n), y = n, fill = recipient_jurisdiction)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Total Value of Donations by Location", x = "Location", y = "Sum") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
