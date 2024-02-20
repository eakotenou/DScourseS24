# (a) Download the file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# (b) Print the file to the console
system('cat dates.json')

# (c) Convert to a data frame in R
# Install the required libraries if not already installed
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(jsonlite)
library(tidyverse)

# Convert JSON to list
mylist <- fromJSON('dates.json')

# Convert list to data frame
mydf <- bind_rows(mylist$result[-1])

# (d) Check object types
class(mydf)   # Type of mydf
class(mydf$date)   # Type of mydf$date

# (e) List the first n rows
head(mydf, n = 5)  # Displaying the first 5 rows, you can adjust n as needed

