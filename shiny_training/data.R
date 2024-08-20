# ------------------------ Loading Libraries ------------------

# Loading Libraries
library(rvest)
library(dplyr)

# --------------------- Creating Scraper Function ------------

# Defining a scraper function from OpenSecrets 
scrape_donor_data <- function(zip) {
  
  # Defining base URL from OpenSecrets donor lookup
  base_url <- "https://www.opensecrets.org/donor-lookup/results?cycle=&order=desc&sort=D"
  
  # Adding zip to URL 
  zip_url <- paste0(base_url, "&zip=", zip)
  
  # Calculating number of entries
  n_entries <- read_html(zip_url) %>% 
    html_nodes("strong+ span") %>% 
    html_text() %>%
    as.integer()
  
  # Giving the number of pages to loop through
  n_pages <- ifelse(n_entries < 500, round(n_entries / 50), 10)+1
  
  # Creating an empty list of DFs
  dfs <- list()
  
  # Looping through page 1 through n_pages
  for (i in seq(1, n_pages)) {
    
    # Adding the page URL
    extended_url <- paste0(zip_url, "&page=", i)
    
    # Reading the page
    page <- read_html(extended_url)
    
    # Pulling our specific data
    category <- page %>% html_nodes(".category") %>% html_text()
    contributor <- page %>% html_nodes("td:nth-child(2)") %>% html_text()
    employer <- page %>% html_nodes("td:nth-child(3)") %>% html_text()
    occupation <- page %>% html_nodes("td:nth-child(4)") %>% html_text()
    date <- page %>% html_nodes("td:nth-child(5)") %>% html_text()
    amount <- page %>% html_nodes("td:nth-child(6)") %>% html_text()
    recipient <- page %>% html_nodes("td:nth-child(7)") %>% html_text()
    recipient_jurisdiction <- page %>% html_nodes("td:nth-child(8)") %>% html_text()
    
    # Putting our data into a single dataframe
    df <- data.frame(
      category,
      contributor,
      employer,
      occupation,
      date,
      amount,
      recipient,
      recipient_jurisdiction,
      stringsAsFactors = FALSE
    )
    
    # Appending the data to a list
    dfs[[i]] <- df
  }
  
  # Binding all dataframes
  combined_df <- bind_rows(dfs) %>%
    
    # Cleaning text
    mutate(across(everything(), ~ str_trim(.))) %>%
    mutate(across(everything(), ~ str_replace_all(., "\\n|\\t", ""))) %>%
    mutate(across(everything(), ~ str_squish(.))) %>%
    
    # Creating a party variable
    mutate(party = case_when(
      str_detect(recipient, "\\(D\\)") ~ "Democrat",
      str_detect(recipient, "\\(R\\)") ~ "Republican",
      TRUE ~ NA_character_
    ),
    
    # Cleaning amount to be numeric
    amount = gsub("[^0-9-]", "", amount),  # Remove any characters that are not digits or negative sign
    amount = as.numeric(amount)
    )
  
  return(combined_df)
}

# ----------------------- Creating Dataframe ----------------------
# Creating dataframe
zip <- 12457
donor_data <- scrape_donor_data(zip)

write.csv(donor_data, "donor_data.csv")

# ------------------------- Analysis ---------------------------

# Where were donations sent?
donor_data %>%
  tabyl(recipient_jurisdiction) %>%
  arrange(-n)

# How much was sent to each location?
donor_data %>%
  group_by(recipient_jurisdiction) %>%
  summarize(sum = sum(amount),
            mean = mean(amount),
            median = median(amount)) %>%
  arrange(-sum)

# What years had the most donations?
donor_data %>%
  mutate(year = str_sub(date, -4)) %>%
  tabyl(year) %>%
  arrange(-n)

# How much was sent during those years?
donor_data %>%
  mutate(year = str_sub(date, -4)) %>%
  group_by(year) %>%
  summarize(sum = sum(amount),
            mean = mean(amount),
            median = median(amount)) %>%
  arrange(-sum)

# What party had the most donations?
donor_data %>%
  tabyl(party) %>%
  arrange(-n)

# How much was sent to those parties?
donor_data %>%
  group_by(party) %>%
  summarize(sum = sum(amount),
            mean = mean(amount),
            median = median(amount)) %>%
  arrange(-sum)