library(tidyverse)
library(janitor)

tidy_wards <- function(df) {
  require(janitor)
  require(dplyr)
  
  clean_df <- df %>% 
    clean_names() %>%
    select(-starts_with('x')) %>%
    na.omit() %>%
    select(-precinct_7) %>%
    rename(precinct = precinct_1) %>%
    filter(!str_detect(precinct, 'Total'))
  return(clean_df)
}

ward1 <- tidy_wards(readxl::read_excel('statementofvotescastoctober242018.xls', sheet = 3, skip = 2))

# To tackle problem: 
# Find a measure to define "close races" in wards (aka fraction of votes is evenly divided)
# Use relative turn-out metric from A4 to measure turnout in ward
# Do regression on this to find out if close races -> greater turnout
# Include advance voters

# total_votes_ward1 <- 
#   ward1 %>%
#   summarise(sum(total_votes)) %>% 
#   pull()
# 
# ward1 %>%
#   pivot_longer(5:(ncol(ward1)-1), names_to = 'candidate', values_to = 'votes') %>%
#   group_by(candidate) %>%
#   summarise(total_votes = sum(votes)) %>%
#   mutate(frac_won = total_votes / total_votes_ward1) %>% 
#   arrange(desc(frac_won))


file <- 'SDS/a4/statementofvotescastoctober242018.xls'

ward_summary <- 
  read_excel(file, sheet = 1, skip = 2) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  filter(!is.na(registered_voters),
         !str_detect(precinct, 'City / Ville - Total'))  %>% 
  separate(precinct, c('precinct_id', 'precinct_name'), ' - ', extra = 'merge') %>%
  filter(!str_detect(precinct_id, 'Adv'))

frac_elig_voters <- 
  ward_summary %>%
  separate(precinct_id, c('ward', 'polling_station_no'), sep = '-') %>%
  mutate(ward = as.numeric(ward)) %>%
  group_by(ward) %>%
  summarise(total_registered_voters = sum(registered_voters),
            total_cards_cast = sum(cards_cast)) %>%
  mutate(frac_votes = total_cards_cast/total_registered_voters)
