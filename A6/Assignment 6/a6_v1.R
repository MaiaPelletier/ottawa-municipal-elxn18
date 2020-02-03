library(tidyverse)
library(janitor)
library(readxl)
library(tidyr)
library(ggthemes)

file <- 'statementofvotescastoctober242018.xls'


# Get ward winners --------------------------------------------------------

num_of_wards <- 23

# Function to clean and gather ward winner(s)
get_winners <- function(ward, num_of_winners = 1){
  
  clean_ward <- 
    ward %>%
    clean_names() %>%
    select(-starts_with('x')) %>%
    na.omit() %>%
    select(-precinct_7) %>%
    rename(precinct = precinct_1) %>%
    filter(!str_detect(precinct, 'Total'))
  
  ward_long <- 
    clean_ward %>%
    gather('candidate', 'votes', 5:(ncol(clean_ward)-1)) %>%
    #pivot_longer(5:(ncol(clean_ward)-1), names_to = 'candidate', values_to = 'votes') %>%
    mutate(candidate = str_to_title(str_replace_all(candidate, '_', ' '))) %>% 
    separate(precinct, c('precinct_id', 'precinct_name'), sep = ' - ', extra = 'merge')
  
  ward_winner <- 
    ward_long %>%
    group_by(precinct_name, candidate) %>%
    summarise(votes = sum(votes)) %>%
    ungroup() %>%
    group_by(candidate) %>%
    summarise(total_candidate_votes = sum(votes)) %>%
    mutate(total_ward_votes = sum(total_candidate_votes),
           frac_candidate_votes = total_candidate_votes/total_ward_votes) %>%
    top_n(num_of_winners, total_candidate_votes)
  
  return(ward_winner)
}

# Iterate over number of wards to build df of ward winners
ward_winners_df <- tibble()
for (i in 1:num_of_wards) {
  add_ward <- get_winners(read_excel(file, sheet = i + 2, skip = 2), num_of_winners = 2)
  ward_winners_df <- bind_rows(ward_winners_df, mutate(add_ward, ward = i))
}

# Widen ward winner df
ward_winners_wide <- 
  ward_winners_df %>%
  group_by(ward) %>%
  mutate(maxvotes = max(total_candidate_votes)) %>%
  mutate(rank = ifelse(total_candidate_votes == maxvotes, 'winner', 'runner-up')) %>%
  select(-maxvotes, -candidate, -frac_candidate_votes, -total_ward_votes) %>%
  #pivot_wider(names_from = rank, values_from = total_candidate_votes)
  spread(rank, total_candidate_votes)

ggplot(ward_winners_wide) +
  geom_segment(aes(x = factor(ward), xend = ward, y = `runner-up`, yend = winner), size = 1, color = 'grey55') +
  geom_point(aes(x = ward, y = `runner-up`, color = 'runner-up'), size = 5, alpha = 0.7) +
  geom_point(aes(x = ward, y = winner, color = 'winner'), size = 5, alpha = 0.7) +
  coord_flip() +
  labs(x = NULL, y = 'Votes', title = 'Vote totals for winner & runner-up in each ward', 
       subtitle = '2018 Ottawa municipal election') +
  scale_x_discrete(labels = paste('Ward', 1:23)) +
  scale_color_canva(palette = 'Summer sunflower', name = NULL) +
  theme_hc() +
  theme(plot.title = element_text(size = 22, face = 'italic', color = 'grey30'),
        plot.subtitle = element_text(size = 11, color = 'grey30'),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10))


margin_df <-
  ward_winners_df %>%
  group_by(ward) %>%
  mutate(minvotes = min(frac_candidate_votes)) %>%
  mutate(margin = frac_candidate_votes-minvotes) %>%
  top_n(1, total_candidate_votes)

ggplot(margin_df, aes(reorder(candidate, 1-margin), 1-margin)) +
  geom_col(alpha = 0.8, fill = '#34888c', width = 0.8) +
  geom_text(aes(label = paste('Ward', ward)), size = 3, hjust = 1.25, color = 'white') +
  labs(x = NULL, y = 'Race closeness') +
  ggtitle('Which candidates had the closest races?', subtitle = 'Ottawa municipal election 2018') +
  coord_flip() +
  theme_hc() +
  theme(plot.title = element_text(size = 22, face = 'italic', color = 'grey30'),
        plot.subtitle = element_text(size = 11, color = 'grey30'),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9))


#Let's pretend its fine to remove the Spc Adv (its not)

elxn_summary <- 
  read_excel(file, sheet = 1, skip = 2) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  filter(!is.na(registered_voters),
         !str_detect(precinct, 'City / Ville - Total'))  %>% 
  separate(precinct, c('precinct_id', 'precinct_name'), ' - ', extra = 'merge') %>%
  filter(!str_detect(precinct_id, 'Spc Adv')) %>%
  mutate(precinct_id = str_remove(precinct_id, 'Adv 1')) %>% 
  separate(precinct_id, c('ward', 'polling_station_no'), sep = '-') %>%
  mutate(ward = as.numeric(ward)) %>%
  group_by(ward) %>%
  summarise(total_registered_voters = sum(registered_voters),
            total_cards_cast = sum(cards_cast)) %>%
  mutate(frac_votes = total_cards_cast/total_registered_voters) %>%
  right_join(read_csv('SDS/ward_info_2011.csv'), by = c('ward' = 'number'))

final_df <-
  margin_df %>% 
  right_join(elxn_summary, by = 'ward')

final_df %>% 
  ggplot(aes(frac_votes, margin)) +
  geom_point(aes(color = margin), size = 5, alpha = 0.8) +
  geom_smooth(formula = y ~ x, method = 'lm', alpha = 0.1, color = 'lightblue') +
  geom_text(aes(label = paste('Ward', ward)), nudge_y = 0.040, nudge_x = 0.0025, size = 3, check_overlap = TRUE, color = 'grey55') +
  labs(x = 'Fraction of registered voters who cast votes', y = 'Margin between candidates') +
  ggtitle('Do closer races lead to higher voter turnouts?', subtitle = 'Analysis of Ottawa 2018 municipal election') +
  theme_minimal() +
  viridis::scale_color_viridis(name = 'Margin') +
  theme(axis.title = element_text(color = 'grey40', face = 'italic', size = 10),
        axis.text = element_text(color = 'grey40', size = 8),
        plot.title = element_text(color = 'grey20', face = 'italic', size = 16, hjust = 0.5),
        plot.subtitle = element_text(color = 'grey30', size = 10))

final_df <-
  margin_df %>% 
  right_join(elxn_summary, by = 'ward')

mod <- lm(margin ~ frac_votes, data = final_df)
summary(mod)



