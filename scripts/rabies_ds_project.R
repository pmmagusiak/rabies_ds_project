# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("skimr")
# install.packages('janitor')
#install.packages("countrycode")

# Loading R essential tidyverse packages and data.table for fast reading tables

library(tidyverse)
library(data.table)

# Importing files into R environment. Animal rabies quantitative data for future
# research

human_rabies_death <- fread("data/reported_number_of_human_rabies_death.csv")
hdi <- fread("data/human_development_index.csv")
# animal_rabies <- fread("data/animal_rabies_quantitative_data.csv")

# Using skimr package to summary data tables and overview missing values,
# duplicates and other information, janitor package to help tidying

library(skimr)
library(janitor)


# Focusing on human_rabies_death database ---------------------------------

# Checking if there are any rows with null values

human_rabies_death %>% filter(if_any(everything(), is.null))

# Assessing columns which are useful for my analysis

skim_without_charts(human_rabies_death)
glimpse(human_rabies_death)


human_rabies_death |>
  filter(Value != FactValueNumeric)

human_rabies_death <- human_rabies_death |>
  select(
    ParentLocationCode,
    ParentLocation,
    SpatialDimValueCode,
    Location,
    Period,
    FactValueNumeric
  ) |>
  clean_names()

# Focusing on hdi database --------------------------------------

# Checking if there are any rows with null values

hdi %>% filter(if_any(everything(), is.null))

# Assessing columns which are useful for my analysis

## glimpse(hdi)

# This database has multiple columns containing country development indicators
# per year, it is more suitable to pivot them

hdi <- hdi |>
  pivot_longer(
    cols = c(-(1:5), -166, -581),
    names_pattern = "^(.*)(\\(\\d+\\))$",
    names_to = c(".value", "year")
  ) |>
  mutate(year = parse_number(year)) |>
  clean_names()

# I prefer to leave many variables for future analysis, but for now I will only
# need HDI columns

hdi_rabies <- hdi |>
  select(1:5, 8, 9, 15, 20) |>
  filter(between(year, 2010, 2021))

# Joining the databases ---------------------------------------------------

human_rabies_death |>
  count(period)

human_rabies_death <- human_rabies_death |>
  filter(period != 2022)

# Checking if country names are equivalent

library(countrycode)

human_rabies_death |> count(n_distinct(location)) # this db has 194 countries

hdi_rabies |> count(n_distinct(country)) # this db has 195 countries

human_rabies_death |>
  anti_join(hdi_rabies, join_by(spatial_dim_value_code == iso3))

human_rabies_death_processed <- human_rabies_death |>
  filter(!spatial_dim_value_code %in% c("COK", "NIU")) |>
  mutate(location = countrycode(spatial_dim_value_code, "iso3c", "country.name"))

hdi_rabies_processed <- hdi_rabies |>
  filter(!human_development_groups == "") |>  # Filtering out Somalia
  mutate(human_development_groups = factor(
    human_development_groups,
    levels = c("Low", "Medium", "High", "Very High")
  )) |>
  mutate(country = countrycode(iso3, "iso3c", "country.name"))

hdi_rabies_death_joined <- hdi_rabies_processed |>
  left_join(human_rabies_death_processed,
            join_by(country == location, year == period)) |>
  filter(!is.na(fact_value_numeric))



# Visualizing the data ----------------------------------------------------

hdi_rabies_death_joined |>
  count(fact_value_numeric, sort = TRUE)

# Unfortunately (for my analysis), most of the human rabies death data is not available or "0"


human_rabies_death |>
  filter(fact_value_numeric > 200) |>
  arrange(desc(fact_value_numeric))

# China, India and Philippines are outliers

hdi_rabies_death_joined |> 
  filter(country %in% c("China", "India", "Philippines")) |> 
  distinct(country, human_development_groups)

# China is high developed country while India and Philippines are medium developed


hdi_rabies_death_joined |>
  filter(!country %in% c("China", "India", "Philippines")) |>
  ggplot(aes(x = as_factor(year), y = fact_value_numeric)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ human_development_groups) +
  labs(
    x = "", 
    y = "Number of deaths", 
    title = "Distribution of human rabies deaths per year facetted by country development group",
    subtitle = "Does not contain data from China, India and Philippines") +
  theme_minimal()

# hdi_rabies_death_joined |>
#   ggplot(aes(human_development_index, fact_value_numeric)) +
#   geom_bin2d(bins = 50) +
#   scale_fill_continuous(type = "viridis") +
#   labs(
#     x = "Human Development Index",
#     y = "Number of deaths",
#     title = "Distribution of HDI and number of deaths"
#   ) +
#   theme(
#     legend.title = element_blank()
#   )


median_deaths_per_hdg_per_year <- hdi_rabies_death_joined |>
  filter(!country %in% c("China", "India", "Philippines")) |> 
  group_by(human_development_groups, year) |>
  summarise(median_death_count = median(fact_value_numeric)) 


median_deaths_per_hdg_per_year |>
  ggplot(aes(year, median_death_count, color = human_development_groups)
) +
  geom_line() +
  labs(
    x = "Year",
    y = "Median number of deaths",
    color = "Human development groups",
    title = "Timeline of median human rabies deaths in different human development groups",
    subtitle = "Does not contain data from China, India and Philippines"
  ) +
  theme_minimal()

# China, India and Philippines deserve their own graphs

hdi_rabies_death_joined |>
  filter(country %in% c("China", "India", "Philippines")) |>
  ggplot(aes(year, fact_value_numeric, color = country)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Number of deaths",
    color = "Country",
    title = "Timeline of human rabies deaths in China, India and Philippines"
  ) 


# Using plotly package ----------------------------------------------------

#install.packages("plotly")
library(plotly)

hdi_rabies_death_joined_plotly <- hdi_rabies_death_joined |>
  filter(year == 2021 & fact_value_numeric > 0)


plot_ly(
  hdi_rabies_death_joined_plotly,
  type = 'choropleth',
  locations = hdi_rabies_death_joined_plotly$iso3,
  z = hdi_rabies_death_joined_plotly$fact_value_numeric,
  text = hdi_rabies_death_joined_plotly$country,
  colorscale = "Reds"
) |> 
  layout(title = "Human rabies deaths in 2021") |> 
  colorbar(title = "Number of deaths")


