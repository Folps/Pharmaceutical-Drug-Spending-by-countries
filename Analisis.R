library(tidyverse)
data <- read_csv("population.csv")
glimpse(data)

latam <- pop %>% rename(country_name = `Country Name`, country_code = `Country Code`)
