# Load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(ggthemes)
library(viridis)
library(kableExtra)
library(broom)
library(sf)
library(ggsflabel)
library(gghighlight)

# Set theme for graphics
theme_set(theme_minimal())

col <- nationalparkcolors::park_palettes$Arches

# Set data paths to be read
ward_data <- 'statementofvotescastoctober242018.xls'
shpfile <- 'WardsShp/Wards.shp'
census_data <- 'census_2016.csv'

get_winners <- function(ward, num_of_winners = 1) {
  
  # Function to retrieve the sepcified number of winners in a ward, given a
  # dataframe containing voting totals.
  
  
  clean_ward <-                                        # Clean voting data
    ward %>%
    clean_names() %>%
    select(-starts_with('x')) %>%
    na.omit() %>%
    select(-precinct_7) %>%
    rename(precinct = precinct_1) %>%
    filter(!str_detect(precinct, 'Total'))
  
  
  ward_long <-
    clean_ward %>%                                      
    filter(!str_detect(precinct, 'Spc')) %>%            # Filter out special advanced polling
    pivot_longer(                                       # Make dataframe longer
      5:(ncol(clean_ward)-1), 
      names_to = 'candidate', 
      values_to = 'votes'
    ) %>%
    mutate(
      candidate = str_replace_all(candidate, '_', ' '),  # Make candidate names more readable
      candidate = str_to_title(candidate)
    ) %>% 
    separate(                                            # Seperate poll station number from name
      precinct, 
      c('precinct_id', 'precinct_name'), 
      sep = ' - ', 
      extra = 'merge'
    )
  
  ward_winner <-                                        # Select row containing ward winner(s) 
    ward_long %>%
    group_by(precinct_name, candidate) %>%
    summarise(votes = sum(votes)) %>%
    ungroup() %>%
    group_by(candidate) %>%
    summarise(total_candidate_votes = sum(votes)) %>%
    mutate(
      total_ward_votes = sum(total_candidate_votes),
      frac_candidate_votes = total_candidate_votes/total_ward_votes
    ) %>%
    top_n(num_of_winners, total_candidate_votes)         # Take defined number of top vote-getters
  
  return(ward_winner)
}

num_of_wards <- 23 # Number of wards in Ottawa

ward_winners_df <- tibble() # Empty df to append to

# Iterate over number of wards to build df of ward winners
for (i in 1:num_of_wards) {
  add_ward <- get_winners(read_excel(ward_data, sheet = i + 2, skip = 2), num_of_winners = 2)
  ward_winners_df <- bind_rows(ward_winners_df, mutate(add_ward, ward = i))
}

# Widen ward winner df
ward_winners_df <- 
  ward_winners_df %>%
  group_by(ward) %>%
  mutate(maxvotes = max(total_candidate_votes)) %>%
  mutate(rank = ifelse(total_candidate_votes == maxvotes, 'winner', 'runner-up'))

ward_winners_names <- 
  ward_winners_df %>%
  pivot_wider(names_from = rank, values_from = candidate) %>%
  select(ward, `runner-up`, winner, total_candidate_votes)

ward_winners_wide <-
  ward_winners_df %>%
  select(-maxvotes,
         -candidate,
         -frac_candidate_votes,
         -total_ward_votes) %>%
  pivot_wider(names_from = rank, values_from = total_candidate_votes)

# Plot ward winner and runner-up
ggplot(ward_winners_wide) +
  geom_segment(
    aes(
      x = factor(ward),
      xend = ward,
      y = `runner-up`,
      yend = winner
    ),
    size = 1,
    color = 'grey55'
  ) +
  geom_point(aes(x = ward, y = `runner-up`, color = 'Runner-up'),
             size = 4.5,
             alpha = 0.8) +
  geom_point(aes(x = ward, y = winner, color = 'Winner'),
             size = 5,
             alpha = 0.8) +
  geom_text(data = ward_winners_names, 
            aes(x = ward, y = total_candidate_votes, label = winner),
            size = 3, hjust = -0.25, color = 'grey30') +
  geom_text(data = ward_winners_names,
            aes(x = ward, y = total_candidate_votes, label = `runner-up`),
            size = 2.5, hjust = 1.25, color = 'grey50') +
  coord_flip() +
  labs(
    x = NULL,
    y = 'Votes',
    title = 'Vote totals for winner & runner-up in each ward',
    subtitle = '2018 Ottawa municipal election'
  ) +
  scale_x_discrete(labels = paste('Ward', 1:23)) +
  scale_color_manual(values = col[c(2, 3)], name = NULL) +
  ylim(-2500, 14000) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = 'italic', color = 'grey30'),
    plot.subtitle = element_text(size = 11, color = 'grey30'),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    legend.position = 'bottom'
  )

# Find margin between winner and runner-up
margin_df <-
  ward_winners_df %>%
  group_by(ward) %>%
  mutate(minvotes = min(frac_candidate_votes)) %>%
  mutate(margin = frac_candidate_votes - minvotes) %>%
  top_n(1, total_candidate_votes)

# Show 10 closest races
margin_df %>%
  select(candidate, ward, frac_candidate_votes, margin) %>%
  arrange(margin) %>%
  head(10) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    fixed_thead = T
  )


# Read and tidy summary data
clean_summary <- 
  read_excel(ward_data, sheet = 1, skip = 2) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  filter(!is.na(registered_voters),
         !str_detect(precinct, 'City / Ville - Total'))

# Massage summary data to get turn-out
elxn_summary <-
  clean_summary %>%
  # Separate polling station number from name
  separate(precinct, c('precinct_id', 'precinct_name'), sep = ' - ', extra = 'merge') %>%
  # Remove special advance polling
  filter(!str_detect(precinct_id, 'Spc Adv')) %>%
  # Remove advance polling identifier
  mutate(precinct_id = str_remove(precinct_id, 'Adv 1')) %>% 
  # Separate ward number from polling station number
  separate(precinct_id, c('ward', 'polling_station_no'), sep = '-') %>% 
  mutate(ward = as.numeric(ward)) %>%
  group_by(ward) %>%
  summarise(
    total_registered_voters = sum(registered_voters),
    total_cards_cast = sum(cards_cast)
  ) %>%
  # Calculate voter turn-out
  mutate(frac_votes = total_cards_cast / total_registered_voters)


wrdshp <-   # Read Wards shapefile
  st_read(shpfile, quiet = TRUE) %>% 
  clean_names()


ward_df <-  # Massage data to join with shape data
  elxn_summary %>% 
  mutate(ward_num = as.character(ward)) %>%
  select(ward_num, frac_votes)

# Join with shape data
wrd_shp_df <- right_join(wrdshp, ward_df, by = 'ward_num')

# Plot sf 
ggplot() +
  geom_sf(data = wrd_shp_df,
          aes(fill = frac_votes),
          alpha = 0.8,
          color = 'black') +
  geom_sf_label_repel(data = wrd_shp_df,
                      aes(label = ward_num),
                      size = 3,
                      force = 25) +
  scale_fill_viridis_c(option = 'B',
                       breaks = seq(0.28, 0.5, 0.05),
                       name = 'Voter turn-out') +
  ggtitle('What was the voter turn-out in each ward?',
          subtitle = 'Ottawa 2018 municipal election') +
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(color = 'grey20', face = 'italic', size = 16, hjust = 0.5),
        plot.subtitle = element_text(color = 'grey30', size = 10, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, vjust = 1),
        legend.key.height = unit(2.2, 'mm'),
        legend.key.width = unit(13.6, 'mm'))

# Join data together
final_df <-
  margin_df %>% 
  right_join(elxn_summary, by = 'ward') %>%
  right_join(read_csv(census_data), by = c('ward' = 'number')) %>%
  rename(name = Ward)

final_df$name[final_df$name == 'Gloucester-Southgate'] <- 'Gloucester\nSouthgate'

# Final plot to investigate relationship
final_df %>% 
  ggplot(aes(frac_votes, margin)) +
  geom_point(
    aes(color = margin), 
    size = 6, 
    alpha = 0.8
  ) +
  geom_smooth(
    formula = y ~ x, 
    method = 'lm', 
    alpha = 0.1, 
    color = '#edae01'
  ) +
  geom_text(
    aes(label = paste0(name,' (Ward ', ward, ')')), 
    nudge_y = 0.040, 
    nudge_x = 0.0025, 
    size = 3, 
    check_overlap = TRUE, 
    color = 'grey45') +
  labs(
    x = 'Fraction of registered voters who cast votes', 
    y = 'Margin between candidates'
  ) +
  ggtitle('Do closer races lead to higher voter turnouts?', 
          subtitle = 'Analysis of Ottawa 2018 municipal election') +
  scale_color_viridis(name = 'Margin', option = 'B') +
  xlim(c(0.32, 0.525)) +
  theme_minimal() +
  theme(
    axis.title = element_text(color = 'grey40', face = 'italic', size = 10),
    axis.text = element_text(color = 'grey40', size = 8),
    plot.title = element_text(color = 'grey20', face = 'italic', size = 16, hjust = 0.5),
    plot.subtitle = element_text(color = 'grey30', size = 10, hjust = 0.5),
    legend.key.height = unit(12, 'mm'),
    legend.key.width = unit(2.2, 'mm'),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank()
  ) 

get_spcadv_winners <- function(ward, num_of_winners = 1){
  
  # Function to retrieve the sepcified number of winners in a ward, given a
  # dataframe containing voting totals.
  
  # Clean voting data
  clean_ward <- 
    ward %>%
    clean_names() %>%
    select(-starts_with('x')) %>%
    na.omit() %>%
    select(-precinct_7) %>%
    rename(precinct = precinct_1) %>%
    filter(!str_detect(precinct, 'Total'))
  
  # Make dataframe longer (gather candidates into one column) 
  ward_long <- 
    clean_ward %>%
    # Filter out special advanced polling
    filter(str_detect(precinct, 'Spc')) %>%
    pivot_longer(5:(ncol(clean_ward)-1), names_to = 'candidate', values_to = 'votes') %>%
    # Make candidate names more readable
    mutate(candidate = str_to_title(str_replace_all(candidate, '_', ' '))) %>% 
    # Seperate polling station number from name
    separate(precinct, c('precinct_id', 'precinct_name'), sep = ' - ', extra = 'merge')
  
  # Select row containing ward winner(s) 
  ward_winner <- 
    ward_long %>%
    group_by(precinct_name, candidate) %>%
    summarise(votes = sum(votes)) %>%
    ungroup() %>%
    group_by(candidate) %>%
    summarise(total_candidate_votes = sum(votes)) %>%
    mutate(total_ward_votes = sum(total_candidate_votes),
           frac_candidate_votes = total_candidate_votes/total_ward_votes) %>%
    # Take defined number of top vote-getters
    top_n(num_of_winners, total_candidate_votes)
  
  return(ward_winner)
}

spcadv_winners_df <- tibble() # Empty df to append to

# Iterate over number of wards to build df of ward winners
for (i in 1:num_of_wards) {
  add_ward <- get_spcadv_winners(read_excel(ward_data, sheet = i + 2, skip = 2), num_of_winners = 1)
  spcadv_winners_df <- bind_rows(spcadv_winners_df, mutate(add_ward, ward = i))
}

combine_polling <- 
  bind_rows(
    ward_winners_df %>%
      group_by(ward) %>%
      top_n(1, total_candidate_votes) %>%
      mutate(poll_type = 'normal'),
    spcadv_winners_df %>%
      mutate(poll_type = 'spc adv')
  ) 

combine_polling %>%
  count(candidate, ward) %>%
  ungroup() %>%
  ggplot(aes(factor(ward), factor(n))) +
  geom_point(aes(color = factor(n)), size = 5, alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = ward), vjust = -1.25) +
  gghighlight(n == 1) +
  coord_flip() +
  labs(y = NULL, x = 'Ward') +
  ggtitle('Did election winner also win in advanced polling?') +
  scale_color_canva(palette = 'Jewel tones') +
  scale_y_discrete(labels = c('No', 'Yes'), position = 'right') +
  theme(
    axis.title = element_text(color = 'grey40', face = 'italic', size = 10),
    axis.text.y = element_text(color = 'grey40', size = 8),
    axis.text.x = element_text(color = 'grey40', size = 10),
    plot.title = element_text(color = 'grey20', face = 'italic', size = 16, hjust = 0.5)
  )

