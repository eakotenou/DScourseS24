#git remote add upstream https://github.com/tyleransom/DScourseS24
#git merge upstream/master
  
  ## Install development version of rvest if necessary
  if (numeric_version(packageVersion("rvest")) < numeric_version('0.99.0')) {
    remotes::install_github('tidyverse/rvest')
  }
  ## Load and install the packages that we'll be using today
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, dplyr, httr,jsonlite, rvest, lubridate, janitor, data.table, hrbrthemes)
  ## My preferred ggplot2 plotting theme (optional)
  theme_set(hrbrthemes::theme_ipsum())

  library(rvest) ## Already loaded
  library(janitor) ## Already loaded
  library(lubridate) ## Already loaded
  library(tidyverse) ## Already loaded
  library(httr) ## Already loaded
  library(lubridate) ## Already loaded
  library(dplyr)
  library(tidyr)
 #Q3 

#  setwd("/Users/emilienakotenou/Documents/D_ECON_2/My project 1")
  
  # Specify the URL of the Wikipedia page
  url13 <- "https://www.kenyaforum.net/politics/iebc-presidential-election-full-results-by-county-kenyatta-odinga-mudavadi/"
  url17 <- "https://en.wikipedia.org/wiki/2017_Kenyan_general_election"
  url22 <- "https://en.wikipedia.org/wiki/2022_Kenyan_general_election"
  
  # Read the HTML content of the page
  webpage <- read_html(url22)
  webpage
  # Extract the election results table
  election_table22 <- html_nodes(webpage, xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  election_table22
  # Print the extracted table
  print(election_table22)
  
  # Specify the file path where the Excel file will be saved
  file_path <- 
    
    
  # Read the HTML content of the page
  webpage <- read_html(url17)
  
  # Extract the election results table
  election_table17 <- html_nodes(webpage, xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  # Print the extracted table
  print(election_table17)
  
  
  # Specify the URL of the website
  url13 <- "https://www.kenyaforum.net/politics/iebc-presidential-election-full-results-by-county-kenyatta-odinga-mudavadi/"
  
  # Read the HTML content of the page
  webpage <- read_html(url13)
  
  # Extract the first table on the page (you may need to adjust the XPath)
  election_table13 <- html_nodes(webpage, "table") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Print the extracted table
  print(election_table13)
  
  #Q4
  library(jsonlite) ## Already loaded above
  
  endpoint = "https://fantasy.premierleague.com/api/bootstrap-static/"
  epl = jsonlite::fromJSON(endpoint)
  str(epl)
  sta =
    bind_cols(
      epl$events,
      epl$events %>% select(matches:previousPos)
    ) %>%
    clean_names() %>%
    select(-c(id, alt_id, annotations)) %>% ## These columns aren't adding much of interest
    select(pos, pts, everything()) %>% ## Reorder remaining columns
    as_tibble()  ## "Enhanced" tidyverse version of a data frame
 sta
  