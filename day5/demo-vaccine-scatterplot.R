#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("dplyr")
## The code above will install the package you need on your computer
## you only need to run it once, if you haven't downloaded the package before

## within any new R session, you need to load the libraries you need
library(dplyr)
library(scales)

#######################################################################
### Read in course datasets ###########################################
#######################################################################

## this code tells R to read in a tab-delimited file as a data frame
measles_gdp <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_coverage_cases_gdp.tsv")

measles_gdp %>%
  arrange(desc(total_population)) %>%
  mutate(country_name = factor(country_name, country_name)) %>%
  ggplot(aes(x=mcv1_coverage, y=cases_total, size=total_population, color=who_region)) +
  geom_point(alpha = 0.5) +
  ylab("Measles Cases in 2022") +
  xlab("Vaccine Coverage") +
  labs(color = "WHO Region") +
  scale_size_continuous(name = "Population size",
                        labels = scales::comma,
                        range = c(.01, 15),
                        breaks = c(min(measles_gdp$total_population, na.rm = TRUE), 
                                   max(measles_gdp$total_population, na.rm = TRUE)))

