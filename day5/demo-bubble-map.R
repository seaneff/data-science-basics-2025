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
library(forcats)
library(sf)

#######################################################################
### Read in course datasets ###########################################
#######################################################################

## borrowing from UNHCR online code here: https://dataviz.unhcr.org/tools/r/r_bubble_map.htmls
df_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/bubble_map.csv"
lat_long <- read.csv("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/longitude-latitude.csv")
poly_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_polygons_simplified.json"
line_url <- "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_lines_simplified.json"
countries <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/countries.tsv")
measles_cases <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_cases.tsv")
measles_policy <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_policy.tsv")
measles_coverage <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_vaccine_coverage.tsv")
measles_big <- read.delim("https://raw.githubusercontent.com/seaneff/data-science-basics-2024/main/course-datasets/measles_coverage_cases_gdp.tsv")

#######################################################################
### Reformat data #####################################################
#######################################################################

# Read and transform data
df <- lat_long |> 
  filter(complete.cases(Longitude)) |> 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326)

poly <- read_sf(poly_url) |> 
  st_set_crs(4326)

line <- read_sf(line_url) |>
  mutate(
    type = as_factor(type) |>
      fct_relevel("solid", "dashed", "dotted", "dashed-dot")
  ) |> 
  st_set_crs(4326)

#######################################################################
### Add in data on measles to the GIS info we need to map #############
#######################################################################

bubble_data <- merge(df, measles_big, by.x = "ISO.ALPHA.3", by.y = "iso_code")

#######################################################################
### Try out a plot ####################################################
#######################################################################

# Plot
ggplot() +
  geom_sf(data = poly,
          fill = "gray80",
          color = "transparent") +
  geom_sf(data = line,
          aes(linetype = type),
          color = "white",
          linewidth = .25,
          show.legend = FALSE) +
  geom_sf(data = bubble_data[which(bubble_data$cases_total > 0),],
          aes(size = cases_total, fill = measles_vaccine_policy),
          shape = 21,
          alpha = 0.3) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_size_area(max_size = 6,
                  labels = scales::label_number(
                    scale_cut = cut_short_scale()
                  ),
                  breaks = c(100, 1000, 10000, 100000)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(
    title = "Add title",
    caption = "Add caption",
    size = "Add size label",
    fill = "Add fill label"
  ) +
  coord_sf(crs = st_crs('ESRI:54030')) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) 
