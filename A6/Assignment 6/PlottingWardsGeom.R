library(tidyverse)
library(sf)
library(janitor)
library(Cairo)
library(patchwork)

wrdshp <-  read_sf("WardsShp/Wards.shp") %>% 
  clean_names()

ward_df <- final_df %>% 
  ungroup() %>%
  mutate(ward_num = as.character(ward)) %>%
  select(ward_num, margin, frac_votes)

wrd_shp_df <- right_join(wrdshp, ward_df)

p1 <- 
ggplot() +
  geom_sf(data = wrd_shp_df, aes(fill = margin), alpha = 0.8, color = 'black') +
  scale_fill_gradient(low = '#190537', high = 'white', name = 'Closeness of election race') +
  ggtitle('Ottawa 2018 municipal election race') +
  #scale_fill_gradientn(colors = sf.colors(n = 5)) +
  theme_void() +
  theme(legend.position = 'bottom')

p2 <-
ggplot() +
  geom_sf(data = wrd_shp_df, aes(fill = frac_votes), alpha = 0.8, color = 'black') +
  scale_fill_gradient(high = '#024C59', low = 'white', name = 'Voter turnout') +
  ggtitle('Ottawa 2018 municipal election race') +
  #scale_fill_gradientn(colors = sf.colors(n = 5)) +
  theme_void() +
  theme(legend.position = 'bottom')

p1 + p2

ggsave('ottgeom.png', type = 'cairo')


