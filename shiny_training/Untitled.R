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
      h3("Filter"),
      sliderInput("dateRange",
                  "Select Year Range:",
                  min = min(df$year),
                  max = max(df$year),
                  value = c(min(df$year), max(df$year)),
                  step = 1),
      br(),
      h3("Additional Information"),
      p("This website provides an analysis of political donor contributions coming from the Zip Code: 12457. This includes breakdowns by donation category, and political party. The data is filtered based on the year range selected.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Donation Categories",
                 h3("Total Donations by Category"),
                 p("There are a number of different types of donations. Understanding what donation types people are engaging in may help narrow down the focus of a donation campaign."),
                 selectInput("categoryselector", "Plot type", choices = c("Category Count", "Total Funding","Funding Distribution")),
                 plotlyOutput("categoryPlot"),
                 plotlyOutput("categoryPlotfunding")
        ),
        tabPanel("Party",
                 h3("Donations by Party"),
                 p("Analyze donations by political party."),
                 selectInput("partyselector", "Plot type", choices = c("Party Count", "Total Funding","Funding Distribution")),
                 plotlyOutput("partyPlot"),
                 plotlyOutput("partyPlotfunding")
        ),
        tabPanel("Location",
                 h3("Total Donations by Location"),
                 p("Analyze donations by location."),
                 selectInput("locationselector", "Plot type", choices = c("Location Count", "Total Funding","Funding Distribution")),
                 plotlyOutput("locationPlot"),
                 plotlyOutput("locationPlotfunding")
        ),
        tabPanel("Statistics",
                 h3("Donation Statistics"),
                 DTOutput("statsTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactively filter data based on date range input
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
        width = '100%'  # Ensure the table takes up full width of the container
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
