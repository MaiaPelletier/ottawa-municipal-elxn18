library(tidyverse)
library(sf)
library(janitor)
library(Cairo)
library(ggthemes)
library(viridis)


wrdshp <-  st_read("SDS/a6/Wards/Wards.shp") %>% 
  clean_names()

ward_df <- final_df %>% 
  ungroup() %>%
  mutate(ward_num = as.character(ward)) %>%
  select(ward_num, margin, frac_votes)

wrd_shp_df <- right_join(wrdshp, ward_df)

p1 <- 
ggplot() +
  geom_sf(data = wrd_shp_df, aes(fill = margin), alpha = 0.8, color = 'black') +
  geom_sf_text(data = wrd_shp_df, aes(label = ward_num), size = 3, check_overlap = TRUE, color = 'white') +
  #geom_sf_label_repel(wrd_shp_df, aes(label = name)) +
  ggtitle('Ottawa 2018 municipal election race') +
  scale_fill_viridis_c(option = 'B') +
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

ggsave('ottgeom.png', type = 'cairo')




# Plot Spc Adv for investigation ------------------------------------------

# Who was popular in the advance polling?





