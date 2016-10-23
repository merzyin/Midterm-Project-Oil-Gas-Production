# County-level oil and gas production
# Group Member: Mengze Yin, Tianchi Zhang, Yilin Li
# Objective of this project : 
# Discover the relationship between urbanization level and oil and gas production


library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(doBy)

# Read file "Oilgascounty.csv" (Reference: www.ers.usda.gov)
raw <- read.csv("oilgascounty.csv",stringsAsFactors = FALSE)
raw <- raw[, c("FIPS","geoid","Stabr","County_Name","Rural_Urban_Continuum_Code_2013","Urban_Influence_2013","Metro_Nonmetro_2013","Metro_Micro_Noncore_2013","oil2000","oil2001","oil2002","oil2003","oil2004","oil2005","oil2006","oil2007","oil2008","oil2009","oil2010","oil2011","gas2000","gas2001","gas2002","gas2003","gas2004","gas2005","gas2006","gas2007","gas2008","gas2009","gas2010","gas2011","oil_change_group","gas_change_group","oil_gas_change_group")]

# Extract column names in order to revise them 
names(raw)[3] <- "State"
names(raw)[4] <- "County"
names(raw)[5] <- "Population_Level"
names(raw)[6] <- "Urban_Influence"
names(raw)[7] <- "If_Metro"
names(raw)[8] <- "Metro_Micro_Noncore"

raw


# Split the table into two because there are multiple types in one table: County & Oil and Gas Production
# Create two tables

# Select the information of County to form a new table  
county <- raw %>% 
  select(FIPS, State, County,Population_Level,Urban_Influence, If_Metro,Metro_Micro_Noncore) %>%
  unique() %>%
  mutate(county_id = row_number())

# To reduce repetition and enhance readability, eliminate the word "County" from the County column
county$County <- sub("County","",county$County)
county


# Select the information of Oil and Gas Production to form a new table
oil_gas0 <- raw %>% 
  select(FIPS, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  unique() %>%
  mutate(production_id = row_number())

# Because column headers include values (oil/gas + year, eg: oil2003), we melted the column heads into rows 
# First, melt down the oil info into two columns: oil + oil_production
oil_gas1 <- oil_gas0 %>% 
  gather(oil,oil_production, oil2000:oil2011, na.rm = TRUE)

# Second, melt down the gas info into two columns: gas + gas_production
oil_gas2 <- oil_gas1 %>%   
  gather(gas, gas_production, gas2000:gas2011, na.rm = TRUE)

# Extract the year information from "oil" or "gas" column (eg. oil2001) and created a "year" column
oil_gas3 <- oil_gas2 %>%
  mutate(
    year = parse_number(oil)
  )

# Omit "oil" and "gas" columns by selecting the rest information and create a new data table called "oil gas"
oil_gas_tidy <- oil_gas3 %>% 
  select(FIPS, year,oil_production,gas_production) %>%
  unique() %>%
  mutate(id = row_number())
oil_gas_tidy


# From here, we give some basic summarized statistics of the dataset as a preparation for futher exploration

# average annual oil and gas production by each county
dframe = data.frame(county = oil_gas_tidy$FIPS, oil=oil_gas_tidy$oil_production, gas=oil_gas_tidy$gas_production, year=oil_gas_tidy$year)
# mean annual oil production for each county
moil_county <- summaryBy(oil~county, data=dframe, FUN = mean)
# mean annual gas production for each county
mgas_county <- summaryBy(gas~county, data=dframe, FUN = mean)


# nationalwide mean oil production (per county) for each year
moil_year <- colMeans(raw[10:20], na.rm = FALSE, dims =1L)
moil_year
# nationalwide mean gas production (per county) for each year
mgas_year <- colMeans(raw[21:32], na.rm = FALSE, dims =1L)
mgas_year


# Form a new table including population level to explore oil and gas prodution based on different county level
# Sum all production through 2000 to 2011 for each county
raw$oil_production <- rowSums(raw[10:20])
raw$gas_production <- rowSums(raw[21:32])

# Create a table with county, county level defined by population, sum of oil production in 12 years and sum of gas production in 12 years
county_production <- raw %>% 
  select(FIPS,Population_Level, oil_production, gas_production) %>%
  unique() %>%
  mutate(county_id = row_number())
county_production

# average total oil and gas production in 12 years for counties by each population level 
dframe2 = data.frame(countylevel = county_production$Population_Level, oil=county_production$oil_production, gas=county_production$gas_production)
# mean total oil prouction in 12 years by each county level
soil_level <- summaryBy(oil~countylevel, data=dframe2, FUN = mean)
# mean total gas production in 12 years by each county level 
sgas_level <- summaryBy(gas~countylevel, data=dframe2, FUN = mean)

# mean oil and gas production for counties by each population level in each year
County_Level_Mean <- raw %>% 
  select(Population_Level, oil2000,oil2001,oil2002,oil2003,oil2004,oil2005,oil2006,oil2007,oil2008,oil2009,oil2010,oil2011,gas2000,gas2001,gas2002,gas2003,gas2004,gas2005,gas2006,gas2007,gas2008,gas2009,gas2010,gas2011) %>%
  mutate(county_id = row_number())
County_Level_Mean <- aggregate(County_Level_Mean[,2:25],list(County_Level_Mean$Population_Level), mean)
names(County_Level_Mean)[1] <- paste("County_Level")
County_Level_Mean
