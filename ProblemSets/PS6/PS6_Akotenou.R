install.packages("listviewer")
library(listviewer)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(httr)

 endpoint = "https://fantasy.premierleague.com/api/bootstrap-static/"
 epl = fromJSON(endpoint)
 str(epl)
 jsonedit(epl, mode = "view")
 head(epl$elements)
 #extract this elements data frame
 epl_elements = epl$elements
 
  #put into a data frame
  epl_elements = as.data.frame(epl_elements)
  View(epl_elements)
  #place id, first_name, Second_name and web_name as first colums of the data frame
  epl_elements = epl_elements[,c("id", "first_name", "second_name", "web_name", names(epl_elements)[!(names(epl_elements) %in% c("id", "first_name", "second_name", "web_name"))])]
  View(epl_elements)
  #add the element_type column to the data frame
  epl_elements$element_type = epl_elements$element_type
  View(epl_elements)
  #show me the first 6 rows of the data frame
  head(epl_elements)
  #show me the last 6 rows of the data frame
  tail(epl_elements)
  #show me the labels of the data frame
  names(epl_elements)
  #descriptive satistics of the data, get to know the data
  summary(epl_elements)
  

 #ggplot2 

#visualize the relationship between in_dreamteam and goals_scored
 plot1 <- ggplot(epl_elements, aes(x = in_dreamteam, y = goals_scored)) + geom_boxplot()
  # save the plot as a png file
ggsave("PS6a_Akotenou.png", plot1)
    #plot the 10 most valuable players
top10 <- epl_elements %>% arrange(desc(now_cost)) %>% head(10)
  #plot the 10 most valuable players
plot2 <- ggplot(top10, aes(x = now_cost, y = web_name)) + geom_point()
  # save the plot as a png file
ggsave("PS6b_Akotenou.png", plot2)
  #miniutes played and goals scored
plot3 <- ggplot(epl_elements, aes(x = minutes, y = goals_scored)) + geom_smooth(method = lm)
plot3
ggsave("PS6c_Akotenou.png", plot3)
