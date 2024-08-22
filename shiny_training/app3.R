# Load your libraries
library(shiny) # Allows us to build a shinyapp
library(tidyverse) # Allows for data manipulation
library(janitor) # Allows for data cleaning
library(plotly) # Gives interactive plots

donor_data <- read_csv("donor_data.csv")
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

custom_theme <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),  # Transparent plot background
      panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
      plot.title = element_text(family = "Oswald", size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      axis.title = element_text(family = "Oswald", size = 12),
      axis.text = element_text(family = "Oswald", size = 10),
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      axis.ticks = element_blank()  # Remove axis ticks
    )
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;600;700&display=swap"),
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Helvetica Neue', sans-serif;
        color: #343a40;
      }
      .title-panel {
        background-color: #0055AA;
        color: white;
        padding: 20px;
        text-align: center;
        font-size: 2em;
        font-weight: bold;
        border-radius: 8px;
        margin-bottom: 30px;
        font-family: 'Oswald', sans-serif;
      }
      h3 {
        color: #0055AA;
        font-weight: bold;
        font-family: 'Oswald', sans-serif;
      }
      p {
        font-size: 1.1em;
        line-height: 1.6em;
      }
      .sidebarPanel {
        background-color: #ffffff;
        border: 1px solid #dee2e6;
        padding: 20px;
        border-radius: 8px;
      }
      .mainPanel {
        background-color: #ffffff;
        border: 1px solid #dee2e6;
        padding: 20px;
        border-radius: 8px;
      }
      .tab-content {
        margin-top: 20px;
      }
      .tab-content h3 {
        border-bottom: 2px solid #0055AA;
        padding-bottom: 10px;
        margin-bottom: 20px;
      }
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        background-color: #0055AA;
        color: white;
        border: 1px solid #dee2e6;
        border-bottom-color: transparent;
      }
      .nav-tabs > li > a {
        color: #0055AA;
        font-weight: bold;
        border-radius: 8px 8px 0 0;
        padding: 10px 15px;
      }
      .plot-container {
        margin-top: 20px;
      }
    "))
  ),
  
  div(class = "title-panel", "Mount Tremper Donor Analysis"),
  
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
                 div(class = "plot-container", plotlyOutput("categoryPlot")),
        ),
        tabPanel("Party",
                 h3("Donations by Party"),
                 p("It is crucial to understand which parties people are donating to in this region so that we can set up advocacy campaigns to collect more progressive funding."),
                 selectInput("partyselector", "Data Selection", choices = c("Party Count", "Total Funding","Funding Distribution")),
                 div(class = "plot-container", plotlyOutput("partyPlot")),
        ),
        tabPanel("Location",
                 h3("Total Donations by Location"),
                 p("Where are people in this region donating? Is most of it going back into New York or to Federal campaigns? These are important questions to ask to narrow down our donation metrics even further."),
                 selectInput("locationselector", "Data Selection", choices = c("Location Count", "Total Funding","Funding Distribution")),
                 div(class = "plot-container", plotlyOutput("locationPlot")),
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
        geom_bar(stat = "identity", color = "black", fill = "#0055AA") +  # Set bars to one color and add border
        coord_flip() +
        labs(title = "Number of Donations by Category", x = "Category", y = "Count") +
        custom_theme()
      
      ggplotly(p)
      
    } else if (input$categoryselector == "Funding Distribution") {
      filtered_data_no_outliers <- filtered_data() %>%
        group_by(category) %>%
        boxplot_filtering()
      
      # Create box plot without outliers
      p <- filtered_data_no_outliers %>%
        ggplot(aes(x = category, y = amount, fill = category)) +
        geom_boxplot(fill = "white", color = "#0055AA", outlier.shape = NA) +  # Set all boxplot elements to one color
        labs(title = "Distribution of Donations by Category", x = "Category", y = "Funding Amount") +
        custom_theme()
      
      ggplotly(p)
      
    } else {
      p <- filtered_data() %>%
        group_by(category) %>%
        summarize(n = sum(amount)) %>%
        ggplot(aes(x = reorder(category, n), y = n, fill = category)) +
        geom_bar(stat = "identity", color = "black", fill = "#0055AA") +  # Set bars to one color and add border
        coord_flip() +
        labs(title = "Total Value of Donations by Category", x = "Category", y = "Sum") +
        custom_theme()
      
      ggplotly(p)
    }
  })
  
  # Render plot for Party based on selection
  output$partyPlot <- renderPlotly({
    if (input$partyselector == "Party Count") {
      p <- filtered_data() %>%
        count(party) %>%
        ggplot(aes(x = reorder(party, n), y = n, fill = party)) +
        geom_bar(stat = "identity", color = "black", fill = "#0055AA") +  # Set bars to one color and add border
        coord_flip() +
        labs(title = "Number of Donations by Party", x = "Party", y = "Count") +
        custom_theme()
      
      ggplotly(p)
      
    } else if (input$partyselector == "Funding Distribution") {
      filtered_data_no_outliers <- filtered_data() %>%
        group_by(party) %>%
        boxplot_filtering()
      
      # Create box plot without outliers
      p <- filtered_data_no_outliers %>%
        ggplot(aes(x = party, y = amount, fill = party)) +
        geom_boxplot(fill = "white", color = "#0055AA", outlier.shape = NA) +  # Set all boxplot elements to one color
        labs(title = "Distribution of Donations by Party", x = "Party", y = "Funding Amount") +
        custom_theme()
      
      ggplotly(p)
      
    } else {
      p <- filtered_data() %>%
        group_by(party) %>%
        summarize(n = sum(amount)) %>%
        ggplot(aes(x = reorder(party, n), y = n, fill = party)) +
        geom_bar(stat = "identity", color = "black", fill = "#0055AA") +  # Set bars to one color and add border
        coord_flip() +
        labs(title = "Total Value of Donations by Party", x = "Party", y = "Sum") +
        custom_theme()
      
      ggplotly(p)
    }
  })
  
  # Render plot for Location based on selection
  output$locationPlot <- renderPlotly({
    if (input$locationselector == "Location Count") {
      p <- filtered_data() %>%
        count(recipient_jurisdiction) %>%
        ggplot(aes(x = reorder(recipient_jurisdiction, n), y = n, fill = recipient_jurisdiction)) +
        geom_bar(stat = "identity", color = "black", fill = "#0055AA") +  # Set bars to one color and add border
        coord_flip() +
        labs(title = "Number of Donations by Location", x = "Location", y = "Count") +
        custom_theme()
      
      ggplotly(p)
      
    } else if (input$locationselector == "Funding Distribution") {
      filtered_data_no_outliers <- filtered_data() %>%
        group_by(recipient_jurisdiction) %>%
        boxplot_filtering()
      
      # Create box plot without outliers
      p <- filtered_data_no_outliers %>%
        ggplot(aes(x = recipient_jurisdiction, y = amount, fill = recipient_jurisdiction)) +
        geom_boxplot(fill = "white", color = "#0055AA", outlier.shape = NA) +  # Set all boxplot elements to one color
        labs(title = "Distribution of Donations by Location", x = "Location", y = "Funding Amount") +
        custom_theme()
      
      ggplotly(p)
      
    } else {
      p <- filtered_data() %>%
        group_by(recipient_jurisdiction) %>%
        summarize(n = sum(amount)) %>%
        ggplot(aes(x = reorder(recipient_jurisdiction, n), y = n, fill = recipient_jurisdiction)) +
        geom_bar(stat = "identity", color = "black", fill = "#0055AA") +  # Set bars to one color and add border
        coord_flip() +
        labs(title = "Total Value of Donations by Location", x = "Location", y = "Sum") +
        custom_theme()
      
      ggplotly(p)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
