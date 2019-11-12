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

clean_ward1 <- 
  ward1 %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  na.omit() %>%
  select(-precinct_7) %>%
  rename(precinct = precinct_1) %>%
  filter(!str_detect(precinct, 'Total'))

ward_long1 <- 
  clean_ward1 %>%
  gather('candidate', 'votes', 5:(ncol(clean_ward1)-1)) %>%
  #pivot_longer(5:(ncol(clean_ward)-1), names_to = 'candidate', values_to = 'votes') %>%
  mutate(candidate = str_to_title(str_replace_all(candidate, '_', ' '))) %>% 
  separate(precinct, c('precinct_id', 'precinct_name'), sep = ' - ', extra = 'merge')

ward_winner1 <-
  ward_long1 %>%
    group_by(precinct_name, candidate) %>%
    summarise(votes = sum(votes)) %>%
    mutate(total_prec_votes = sum(votes),
           frac_candidate_votes = votes/total_prec_votes) %>%
    top_n(2, frac_candidate_votes)
 
margin1_df <-
  ward_winner1 %>%
  group_by(precinct_name) %>%
  mutate(minvotes = min(frac_candidate_votes)) %>%
  mutate(margin = frac_candidate_votes-minvotes) %>%
  top_n(1, frac_candidate_votes)%>% 
  add_count(precinct_name)

ggplot(margin1_df, aes(reorder(precinct_name, 1-margin), 1-margin)) +
  geom_col(alpha = 0.8, fill = '#34888c', width = 0.8) +
  labs(x = NULL, y = 'Race closeness') +
  ggtitle('Which candidates had the closest races?', subtitle = 'Ottawa municipal election 2018') +
  coord_flip() +
  theme_hc() +
  facet_wrap(.~candidate, scales = 'free_y') +
  theme(plot.title = element_text(size = 22, face = 'italic', color = 'grey30'),
        plot.subtitle = element_text(size = 11, color = 'grey30'),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9))  

elxn_summary <- 
  read_excel(file, sheet = 1, skip = 2) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  filter(!is.na(registered_voters),
         !str_detect(precinct, 'City / Ville - Total'))  %>% 
  separate(precinct, c('precinct_id', 'precinct_name'), ' - ', extra = 'merge') %>%
  group_by(precinct_name) %>%
  summarise(total_registed_voters = sum(registered_voters),
            total_cards_cast = sum(cards_cast))

final_df <-
  margin1_df %>% 
  right_join(elxn_summary, by = 'precinct_name') %>%
  filter(!is.na(candidate))

read_excel(file, sheet = 1, skip = 2) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  filter(!is.na(registered_voters),
         !str_detect(precinct, 'City / Ville - Total'))  %>% 
  separate(precinct, c('precinct_id', 'precinct_name'), ' - ', extra = 'merge') %>%
  filter(!str_detect(precinct_id, 'Spc'), registered_voters == 0)

