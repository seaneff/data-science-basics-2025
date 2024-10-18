#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("dplyr")
## The code above will install the package you need on your computer
## you only need to run it once, if you haven't downloaded the package before

## within any new R session, you need to load the libraries you need
library(dplyr)

#######################################################################
### Read in course datasets ###########################################
#######################################################################

## this code tells R to read in a tab-delimited file as a data frame
per_capita_energy_mix <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/per_capita_energy_mix.tsv")
energy_consumption <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/energy_consumption.tsv")
energy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/energy_2022.tsv")

#######################################################################
### Scatterplot of GDP vs. total population ###########################
#######################################################################

energy %>%
  filter(country %in% c("Indonesia", "Vietnam")) %>%
  ggplot(aes(x = gdp, y = population)) +
  ## plot points
  geom_point(color = "#8B4F80") +
  ## xlab specifies the x axis label
  xlab("") +
  ## ylab specifies the y axis label
  ylab("") +
  ## ggtitle specifies the main title
  ggtitle("") 

#######################################################################
### Top 10 countries based on per capita renewable energy use ##########
#######################################################################

## select the top ten countries based on renewable energy use per capita in 2022
top_10_data <- per_capita_energy_mix %>%
  filter(year == 2022) %>%
  arrange((desc(renewable_per_capita_kwh))) %>%
  top_n(10)

## treat the country name as a factor so it stays ordered the way we want it to
top_10_data$entity <- factor(top_10_data$entity, levels = rev(top_10_data$entity))

## basic plot
ggplot(data = top_10_data, aes(y = factor(entity), x = renewable_per_capita_kwh)) +
  geom_bar(stat = "identity")

## your turn -- what else can we add to this plot?


