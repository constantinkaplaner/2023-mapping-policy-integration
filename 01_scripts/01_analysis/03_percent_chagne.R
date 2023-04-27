#
require(tidyverse)

#
load("00_data/ceps_full_mapped_all_versions.Rdata")


ceps_full_mapped_subset_1977$year <- lubridate::year(ceps_full_mapped_subset_1977$Date_document)

# Calculate percentage change between two fields between two time points

percentage_change <-
  function(data, year_from, year_to, field_1, field_2) {
    n_year_from <-  data %>%
      filter(year == year_from &
               get(field_1) == 1) %>%
      count()
    
    n_year_from_combined <- data %>%
      filter(year == year_from &
               get(field_1) == 1 &
               get(field_2) == 1) %>%
      count()
    
    
    percent_from <- n_year_from_combined / n_year_from
    
    n_year_to <-  data %>%
      filter(year == year_to,
             get(field_1) == 1) %>%
      count()
    
    n_year_to_combined <- data %>%
      filter(year == year_to &
               get(field_1) == 1 &
               get(field_2) == 1) %>%
      count()
    
    percent_to <- n_year_to_combined / n_year_to
    
    
    percent_diff <- (percent_to - percent_from) / percent_from
    
    labels <- 
      data.frame(label=c(
                paste("Percentage",year_from),
                paste("Percentage", year_to),
                paste("Percentage Difference")))
    
    values <- rbind(percent_from,
                 percent_to,
                 percent_diff)
    
    return(cbind(labels, values))
    
    
  }

# Apply to examples from the text

percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2007,
                  year_to = 2015,
                  field_1 = "44 EMPLOYMENT AND WORKING CONDITIONS",
                  field_2 = "24 FINANCE")

percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2007,
                  year_to = 2015,
                  field_1 = "44 EMPLOYMENT AND WORKING CONDITIONS",
                  field_2 = "16 ECONOMICS")

percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2007,
                  year_to = 2015,
                  field_1 = "44 EMPLOYMENT AND WORKING CONDITIONS",
                  field_2 = "68 INDUSTRY")


percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2007,
                  year_to = 2015,
                  field_1 = "24 FINANCE",
                  field_2 = "20 TRADE")

percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2007,
                  year_to = 2015,
                  field_1 = "24 FINANCE",
                  field_2 = "40 BUSINESS AND COMPETITION")


percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2010,
                  year_to = 2015,
                  field_1 = "60 AGRI-FOODSTUFFS",
                  field_2 = "20 TRADE")

percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2010,
                  year_to = 2015,
                  field_1 = "60 AGRI-FOODSTUFFS",
                  field_2 = "24 FINANCE")

percentage_change(ceps_full_mapped_subset_1977,
                  year_from = 2010,
                  year_to = 2015,
                  field_1 = "60 AGRI-FOODSTUFFS",
                  field_2 = "56 AGRICULTURE, FORESTRY AND FISHERIES")

