Ottawa 2018 Municipal Election Analysis
================
Maia Pelletier
03/02/2020

  - [Voter turnout for close election
    races](#voter-turnout-for-close-election-races)
      - [Election winners and
        competitors](#election-winners-and-competitors)
      - [Voter turnout](#voter-turnout)
      - [Analysis of race closeness and voter turn-out
        relationship](#analysis-of-race-closeness-and-voter-turn-out-relationship)
  - [Comments](#comments)
      - [Special Advance polling](#special-advance-polling)
      - [Outliers](#outliers)
  - [Possible further explorations](#possible-further-explorations)
  - [Data sources](#data-sources)
  - [Appendix: sessionInfo](#appendix-sessioninfo)

Politics globally have changed a lot this last decade. Federal elections
are important but so are provincial and municipal ones, because they
play a huge roll in our city (*see the LRT*). Using data from the Ottawa
2018 municipal election, we are interested in exploring and analyzing
the city’s voter turn-out and the candidacy race.

**Data used**: Data used is *Ottawa municipal election data from 2018*,
*Ottawa census data from 2016*, and *Ottawa wards shape data* (from City
of Ottawa).

``` r
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

# Colour palette for plots
col <- nationalparkcolors::park_palettes$Arches

# Set data paths to be read
ward_data <- 'statementofvotescastoctober242018.xls'
shpfile <- 'WardsShp/Wards.shp'
census_data <- 'census_2016.csv'
```

# Voter turnout for close election races

It would make sense that in most cases, close election races result in
higher voter turnout. Was this the case from a ward to ward basis for
the 2018 election? We hypothesize that this was the case.

### Election winners and competitors

The first thing we are interested in investigating is how close the
races in each ward actually were. It is important to note that I made
the assumption to remove the *Special Advanced polls*. This is discussed
in detail in the **Comments** section.

``` r
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
```

<img src="OttawaElectionAnalysis_files/figure-gfm/get winners and plot-1.png" style="display: block; margin: auto;" />

We can see here the margin between the winner and the runner-ups in each
ward in Ottawa. Wards where there are particularly close races include
1, 17, and 18. We will be interested in looking at the voter turnout in
these wards in our analysis to see if they follow our hypothesis that
their turnout will be higher.

The metric of interest here is the *difference between the votes for the
winner and the votes for the runner up*. Let’s look at the top 10
closest races in Ottawa in 2018.

``` r
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
  head(10)
```

<div class="kable-table">

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate

</th>

<th style="text-align:right;">

ward

</th>

<th style="text-align:right;">

frac\_candidate\_votes

</th>

<th style="text-align:right;">

margin

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Matthew Luloff

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.2369352

</td>

<td style="text-align:right;">

0.0115206

</td>

</tr>

<tr>

<td style="text-align:left;">

Jean Cloutier

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

0.3269588

</td>

<td style="text-align:right;">

0.0165560

</td>

</tr>

<tr>

<td style="text-align:left;">

Shawn Menard

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

0.2815160

</td>

<td style="text-align:right;">

0.0314342

</td>

</tr>

<tr>

<td style="text-align:left;">

Carol Anne Meehan

</td>

<td style="text-align:right;">

22

</td>

<td style="text-align:right;">

0.4208580

</td>

<td style="text-align:right;">

0.0352769

</td>

</tr>

<tr>

<td style="text-align:left;">

Rick Chiarelli

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

0.4661665

</td>

<td style="text-align:right;">

0.0803144

</td>

</tr>

<tr>

<td style="text-align:left;">

Scott Moffatt

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

0.5562083

</td>

<td style="text-align:right;">

0.1124167

</td>

</tr>

<tr>

<td style="text-align:left;">

Laura Dudas

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0.4163383

</td>

<td style="text-align:right;">

0.1325944

</td>

</tr>

<tr>

<td style="text-align:left;">

Allan Hubley

</td>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

0.4542789

</td>

<td style="text-align:right;">

0.1393626

</td>

</tr>

<tr>

<td style="text-align:left;">

Jenna Sudds

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0.4642788

</td>

<td style="text-align:right;">

0.1404647

</td>

</tr>

<tr>

<td style="text-align:left;">

Glen Gower

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.5795969

</td>

<td style="text-align:right;">

0.1591937

</td>

</tr>

</tbody>

</table>

</div>

``` r
  # kable() %>%
  # kable_styling(
  #   bootstrap_options = c("striped", "hover", "condensed"),
  #   fixed_thead = T
  # )
```

I decided to look at the difference between the fraction of total votes
cast in the ward here. We can see the top 10 closest races. As we noted
before: Wards 1, 17, and 18 had extremely close races. They all came
within 3% of their closest competitor in share of votes, excluding votes
from special advance polls.

### Voter turnout

We are also interested in the voter turn-out for each ward, so that we
can compare it to the margin each ward winner won by. Which wards showed
up for the election in 2018?

``` r
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
```

<img src="OttawaElectionAnalysis_files/figure-gfm/voter turn out-1.png" style="display: block; margin: auto;" />

We see that Ward 17 (Capital) had the highest voter turn-out in the city
and Ward 10 (Gloucester-Southgate) had the lowest. These will be
interesting wards to look at when comparing close election races and
voter turnout. It’s interesting that the ward with the highest voter
turnout borders the ward with the lowest turnout.

## Analysis of race closeness and voter turn-out relationship

Finally, we combine the voter turn-out with the margin of votes the
winner won by and we are able to investigate the relationship between
voter turn-out and close elections.

``` r
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
  geom_smooth(
    formula = y ~ x, 
    method = 'lm', 
    alpha = 0.1, 
    color = '#edae01'
  ) +
  geom_point(
    aes(color = margin), 
    size = 6, 
    alpha = 0.8
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
```

<img src="OttawaElectionAnalysis_files/figure-gfm/final plot-1.png" style="display: block; margin: auto;" />

Indeed, it seems that our hypothesis is supported by our plot. There is
a trend that shows as the margin between the winners and their runner-up
decreases, the voter turn-out in the ward increases. Note Ward 17: had
the highest voter turn-out and was also one of the closest races.

To further investigate our initial claim, we preform a regression
analysis to determine if the relationship between close election races
and high voter turnout is significant.

``` r
# Linear model
mod <- lm(margin ~ frac_votes, data = final_df)

# Coefficient summary
mod %>% 
  tidy()
```

<div class="kable-table">

<table>

<thead>

<tr>

<th style="text-align:left;">

term

</th>

<th style="text-align:right;">

estimate

</th>

<th style="text-align:right;">

std.error

</th>

<th style="text-align:right;">

statistic

</th>

<th style="text-align:right;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:right;">

1.393054

</td>

<td style="text-align:right;">

0.4480226

</td>

<td style="text-align:right;">

3.109339

</td>

<td style="text-align:right;">

0.0053076

</td>

</tr>

<tr>

<td style="text-align:left;">

frac\_votes

</td>

<td style="text-align:right;">

\-2.679651

</td>

<td style="text-align:right;">

1.1054720

</td>

<td style="text-align:right;">

\-2.423988

</td>

<td style="text-align:right;">

0.0244645

</td>

</tr>

</tbody>

</table>

</div>

``` r
  # kable() %>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Full model summary
mod %>%
  glance() %>%
  select(1:6)
```

<div class="kable-table">

<table>

<thead>

<tr>

<th style="text-align:right;">

r.squared

</th>

<th style="text-align:right;">

adj.r.squared

</th>

<th style="text-align:right;">

sigma

</th>

<th style="text-align:right;">

statistic

</th>

<th style="text-align:right;">

p.value

</th>

<th style="text-align:right;">

df

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0.2186255

</td>

<td style="text-align:right;">

0.1814172

</td>

<td style="text-align:right;">

0.2252747

</td>

<td style="text-align:right;">

5.875717

</td>

<td style="text-align:right;">

0.0244645

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

</div>

``` r
  # kable() %>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

We can see that all of the coefficients of the model are significant at
standard significance level \(\alpha = 0.05\). The p-value for the model
is 0.0244645, which is also significant at \(\alpha\). Thus, we conclude
that there is a significant relationship between the response variable,
*vote margin between the winner and runner-up*, and the predictor,
*voter turn-out*.

# Comments

### Special Advance polling

``` r
clean_summary  %>% 
  separate(precinct, c('precinct_id', 'precinct_name'), ' - ', extra = 'merge') %>%
  filter(str_detect(precinct_id, 'Spc Adv')) %>%
  head(6)
```

<div class="kable-table">

<table>

<thead>

<tr>

<th style="text-align:left;">

precinct\_id

</th>

<th style="text-align:left;">

precinct\_name

</th>

<th style="text-align:right;">

registered\_voters

</th>

<th style="text-align:right;">

cards\_cast

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Spc Adv 1 99-001

</td>

<td style="text-align:left;">

François Dupuis Recreation Centre

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1047

</td>

</tr>

<tr>

<td style="text-align:left;">

Spc Adv 1 99-002

</td>

<td style="text-align:left;">

City Hall

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

947

</td>

</tr>

<tr>

<td style="text-align:left;">

Spc Adv 1 99-003

</td>

<td style="text-align:left;">

Greenboro Community Centre

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

841

</td>

</tr>

<tr>

<td style="text-align:left;">

Spc Adv 1 99-004

</td>

<td style="text-align:left;">

Ben Franklin Place

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1003

</td>

</tr>

<tr>

<td style="text-align:left;">

Spc Adv 1 99-005

</td>

<td style="text-align:left;">

Minto Recreation Complex-Barrhaven

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

698

</td>

</tr>

<tr>

<td style="text-align:left;">

Spc Adv 1 99-006

</td>

<td style="text-align:left;">

Richcraft Recreation Complex-Kanata

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

773

</td>

</tr>

</tbody>

</table>

</div>

``` r
  # kable() %>%
  # kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

The reason I chose to remove the special advance polls is because it
seems like they were day-of registration only and therefore there was no
data recorded for registered voters. This was skewing the voter
registration for the analysis, so I decided to exclude these polling
stations.

Just out of curiousity, who were the winners of the special advance
polls?

``` r
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
    pivot_longer(5:(ncol(clean_ward) - 1), 
                 names_to = 'candidate', 
                 values_to = 'votes') %>%
    # Make candidate names more readable
    mutate(candidate = str_to_title(str_replace_all(candidate, '_', ' '))) %>%
    # Seperate polling station number from name
    separate(precinct,
             c('precinct_id', 'precinct_name'),
             sep = ' - ',
             extra = 'merge')
  
  # Select row containing ward winner(s) 
  ward_winner <-
    ward_long %>%
    group_by(precinct_name, candidate) %>%
    summarise(votes = sum(votes)) %>%
    ungroup() %>%
    group_by(candidate) %>%
    summarise(total_candidate_votes = sum(votes)) %>%
    mutate(
      total_ward_votes = sum(total_candidate_votes),
      frac_candidate_votes = total_candidate_votes / total_ward_votes
    ) %>%
    # Take defined number of top vote-getters
    top_n(num_of_winners, total_candidate_votes)
  
  return(ward_winner)
}

spcadv_winners_df <- tibble() # Empty df to append to

# Iterate over number of wards to build df of ward winners
for (i in 1:num_of_wards) {
  add_ward <-
    get_spcadv_winners(read_excel(ward_data, sheet = i + 2, skip = 2), num_of_winners = 1)
  spcadv_winners_df <-
    bind_rows(spcadv_winners_df, mutate(add_ward, ward = i))
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
  geom_point(
    aes(color = factor(n)),
    size = 5,
    alpha = 0.8,
    show.legend = FALSE
  ) +
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
```

<img src="OttawaElectionAnalysis_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

This makes total sense\! The Capital ward (Ward 17) had the second
closest race and the highest voter turn-out. It’s no surprise that the
winner of the election may not have won in the special advance polls.
Who were the competitors?

``` r
combine_polling %>%
  filter(ward == 17) %>%
  pull(candidate)
```

    ## [1] "Shawn Menard"          "Christine Mc Allister"

Shawn Menard ended up winning the ward in the election but Christine
McAllister beat him in the advanced polls.

### Outliers

  - **Ward 15** had a large margin of votes between the winner and the
    runner-up and also had a large voter turn-out. Upon further
    investigation, I discovered that the winner in this ward (Jeff
    Leiper) was up for re-election. It would make sense if the voters in
    Ward 15 *really* liked having him as their counciller during his
    previous term, they would show up in large numbers to ensure his
    re-election.

# Possible further explorations

  - It would be interesting to collect demographic data for each ward
    (the average age, average income, etc.) to add to our model and see
    if this data adds information to the likeliness of a close race
    within a ward. i.e. Are the wards very similar or dissimilar where
    there are closer elections?

  - It would be interesting to compare/contract the policies and
    promises of candidates who ran close races.

# Data sources

  - [The City of Ottawa](https://open.ottawa.ca/datasets/wards)

# Appendix: sessionInfo

``` r
sessionInfo()
```

    ## R version 3.6.2 (2019-12-12)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 18362)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_Canada.1252  LC_CTYPE=English_Canada.1252   
    ## [3] LC_MONETARY=English_Canada.1252 LC_NUMERIC=C                   
    ## [5] LC_TIME=English_Canada.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] gghighlight_0.2.0 ggsflabel_0.0.1   sf_0.8-1          broom_0.5.3      
    ##  [5] kableExtra_1.1.0  viridis_0.5.1     viridisLite_0.3.0 ggthemes_4.2.0   
    ##  [9] readxl_1.3.1      janitor_1.2.1     forcats_0.4.0     stringr_1.4.0    
    ## [13] dplyr_0.8.3       purrr_0.3.3       readr_1.3.1       tidyr_1.0.0      
    ## [17] tibble_2.1.3      ggplot2_3.2.1     tidyverse_1.3.0  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] ggrepel_0.8.1            Rcpp_1.0.3               lubridate_1.7.4         
    ##  [4] lattice_0.20-38          class_7.3-15             assertthat_0.2.1        
    ##  [7] zeallot_0.1.0            digest_0.6.23            R6_2.4.1                
    ## [10] cellranger_1.1.0         backports_1.1.5          reprex_0.3.0            
    ## [13] evaluate_0.14            e1071_1.7-3              highr_0.8               
    ## [16] httr_1.4.1               pillar_1.4.3             rlang_0.4.2             
    ## [19] lazyeval_0.2.2           rstudioapi_0.10          nationalparkcolors_0.1.0
    ## [22] rmarkdown_2.0            labeling_0.3             webshot_0.5.2           
    ## [25] munsell_0.5.0            compiler_3.6.2           modelr_0.1.5            
    ## [28] xfun_0.11                pkgconfig_2.0.3          htmltools_0.4.0         
    ## [31] tidyselect_0.2.5         gridExtra_2.3            fansi_0.4.1             
    ## [34] crayon_1.3.4             dbplyr_1.4.2             withr_2.1.2             
    ## [37] grid_3.6.2               nlme_3.1-142             jsonlite_1.6            
    ## [40] gtable_0.3.0             lifecycle_0.1.0          DBI_1.1.0               
    ## [43] magrittr_1.5             units_0.6-5              scales_1.1.0            
    ## [46] KernSmooth_2.23-16       cli_2.0.1                stringi_1.4.3           
    ## [49] farver_2.0.1             fs_1.3.1                 snakecase_0.11.0        
    ## [52] xml2_1.2.2               ellipsis_0.3.0           generics_0.0.2          
    ## [55] vctrs_0.2.1              tools_3.6.2              glue_1.3.1              
    ## [58] hms_0.5.3                yaml_2.2.0               colorspace_1.4-1        
    ## [61] classInt_0.4-2           rvest_0.3.5              knitr_1.26              
    ## [64] haven_2.2.0
