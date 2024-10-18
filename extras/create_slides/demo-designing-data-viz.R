#######################################################################
### Load required libraries ###########################################
#######################################################################

## if you don't already have readxl downloaded, start by running:
#install.packages("ggplot2")

library(ggplot2) ## for making plots
library(dplyr) ## for manipulating data
library(camcorder) ## for making gif of figure development

#######################################################################
### Read in course datasets ###########################################
#######################################################################

countries <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/countries.tsv")
measles_cases <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_cases.tsv")
measles_policy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_policy.tsv")
measles_coverage <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_coverage.tsv")

#######################################################################
### View the datasets #################################################
#######################################################################

View(countries)
View(measles_cases)
View(measles_policy)

#######################################################################
### Explore dataset generally #########################################
#######################################################################

## What are the columns/fields?
names(countries)
names(measles_cases)

#######################################################################
### Start recording ###################################################
#######################################################################

gg_record(
  dir = file.path("extras/create_slides/day4_figure_versions_build"),
  device = "png",
  width = 6.4,
  height = 3.6,
  units = "in",
  dpi = 300
)

#######################################################################
### Set up dataset ####################################################
#######################################################################

cases_since_2022 <- measles_cases %>%
  filter(month >= as.Date("2022-01-01")) %>%
  group_by(iso_code) %>%
  summarize(cases_total = sum(measles_cases, na.rm = TRUE))

recent_coverage <- measles_coverage %>%
  filter(is_latest_year == TRUE) %>%
  select(country_name, iso_code, who_region, world_bank_region, income_group, mcv1_coverage)

coverage_cases <- merge(merge(recent_coverage, cases_since_2022, by = "iso_code"), countries[,c(1,6,9)], by = "iso_code")

#######################################################################
### Build step: Explore coverage data #################################
#######################################################################

## try 1: first look
ggplot(coverage_cases, aes(x = mcv1_coverage, y = cases_total)) +
  geom_point()

## try 2: who is the country with zero coverage?
## exclude North Korea
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total)) +
  geom_point() 

## try 3: color by region
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = world_bank_region)) +
  geom_point() 

## try 4: color by income group
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = income_group)) +
  geom_point() 

## try 5: exclude missing income group data
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(complete.cases(income_group)) %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = income_group)) +
  geom_point() 

## try 6: order income group sensibly
coverage_cases$income_group <- factor(coverage_cases$income_group, 
                                      levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(complete.cases(income_group)) %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = income_group)) +
  geom_point() 

## try 7: color income levels appropriately
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(complete.cases(income_group)) %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = income_group)) +
  geom_point() +
  scale_color_manual(values = c("#0c2c84", "#225ea8", "#1d91c0", "#41b6c4"))

## try 8: color by vaccine policy
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = measles_vaccine_policy)) +
  geom_point() 

## try 9: filter out cases with unknown policy data
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = measles_vaccine_policy)) +
  geom_point() 

## try 10: size by population
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() 

## try 11: add axis labels
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size")

## try 12: scale axes with commas and with percentages
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma)

## try 13: scale total population size with commas
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma) 

## try 14: move legends to bottom
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma) +
  theme(legend.position = "bottom") 

## try 15: move the legend back to the right
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma) +
  theme(legend.position = "right") 

## get rid of the black legend
coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, y = cases_total, color = measles_vaccine_policy, size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma) +
  guides(size = guide_legend(override.aes = list(colour = "#FF7A69"))) +
  theme(legend.position = "right") 

## update sizing of population bar
build_final <- coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, 
             y = cases_total, 
             color = measles_vaccine_policy, 
             size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma,
                        breaks = c(100000000, 500000000, 1000000000),
                        range = c(.05, 5)) +
  guides(size = guide_legend(override.aes = list(colour = "#FF7A69"))) +
  theme(legend.position = "right") 

ggsave(build_final, file = "extras/create_slides/build_stage.png",
       width = 4.5,
       height = 3.6,
       units = "in",
       dpi = 300)

#######################################################################
### Refine step: Adjust colors and final labels #######################
#######################################################################

original_build <- coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, 
             y = cases_total, 
             color = measles_vaccine_policy, 
             size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma,
                        breaks = c(100000000, 500000000, 1500000000),
                        range = c(.05, 5)) +
  theme(legend.position = "right") +
  guides(size = guide_legend(override.aes = list(colour = "#FF7A69"))) 

ggsave(original_build, file = "extras/create_slides/day4_original_build.png",
       width = 6.4,
       height = 3.6,
       units = "in",
       dpi = 300)

## update colors
update_colors <-  coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, 
             y = cases_total, 
             color = measles_vaccine_policy, 
             size = total_population)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma,
                        breaks = c(100000000, 500000000, 1500000000),
                        range = c(.05, 5)) +
  guides(size = guide_legend(override.aes = list(colour = "#008dc9"))) +
  scale_color_manual(values = c("#f26829", "#008dc9")) +
  theme(legend.position = "right") 

ggsave(update_colors, file = "extras/create_slides/day4_update_colors.png",
  width = 6.4,
  height = 3.6,
  units = "in",
  dpi = 300)

## update text
library(ggtext)
library(ggrepel)

coverage_cases$label <- NA
labeled_countries <- c("India", "Yemen", "Nigeria", "Ethiopia", "Somalia", "Angola", "Indonesia", "Pakistan", "Kazakhstan", "Iraq", "Russia", "Democratic Republic of the Congo")
coverage_cases$label[which(coverage_cases$country_name %in% labeled_countries)] <- coverage_cases$country_name[which(coverage_cases$country_name %in% labeled_countries)]
coverage_cases$label[which(coverage_cases$country_name == "Democratic Republic of the Congo")] <- "DRC"

update_text <- coverage_cases %>%
  filter(country_name != "North Korea") %>%
  filter(measles_vaccine_policy != "no data") %>%
  ggplot(aes(x = mcv1_coverage/100, 
             y = cases_total, 
             color = measles_vaccine_policy, 
             size = total_population,
             label = label)) +
  geom_point() +
  labs(title = "Measles vaccination coverage vs. caseload",
       subtitle = "Countries where vaccination is <span style = 'color:#f26829;'>required</span> and <span style = 'color:#008dc9;'>not required</span>",
       x = "Measles vaccination coverage (MCV1 %)",
       y = "Reported measles cases\n(2022-present)",
       color = "Measles vaccine policy",
       size = "Total population size") +
    geom_text_repel(size = 2.8, min.segment.length = 0.1, family = "Barlow",
                    seed = 12,
                    force_pull = 2) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma,
                        breaks = c(100000000, 500000000, 100000000),
                        range = c(.05, 5)) +
  guides(size = guide_legend(override.aes = list(colour = "#008dc9")),
         colour = FALSE) +
  scale_color_manual(values = c("#f26829", "#008dc9")) +
  theme(legend.position = "bottom",
        text = element_text(colour = "grey10", family = "Barlow"),
        plot.subtitle = element_markdown(size = rel(0.95)),
        legend.title = element_text(size = rel(0.65)),
        legend.text = element_text(size = rel(0.65)))

ggsave(update_text, file = "extras/create_slides/update_text.png",
       width = 4.5,
       height = 3.6,
       units = "in",
       dpi = 300)

#######################################################################
### Stop recording and save GIF #######################################
#######################################################################

gg_stop_recording()

gg_playback(
  name = file.path("extras/create_slides/day4_figure_versions_build/figure_versions.gif"),
  first_image_duration = 1,
  last_image_duration = 5,
  frame_duration = 1,
  background = "white",
  last_as_first = FALSE
)
