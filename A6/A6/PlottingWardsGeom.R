library(sf)
library(janitor)
library(Cairo)

wrdshp <-  read_sf("WardsShp/Wards.shp") %>% 
  clean_names()

ward_df <- final_df %>% 
  ungroup() %>%
  mutate(ward_num = as.character(ward)) %>%
  select(ward_num, margin, frac_votes)

wrd_shp_df <- right_join(wrdshp, ward_df)

ggplot() +
  geom_sf(data = wrd_shp_df, aes(fill = margin), alpha = 0.8) +
  scale_fill_gradient(low = '#34888c', high = 'white', name = 'Closeness of election race') +
  ggtitle('Ottawa 2018 municipal election race') +
  #scale_fill_gradientn(colors = sf.colors(n = 5)) +
  theme_void() +
  theme(legend.position = 'bottom')

ggsave('ottgeom.png', type = 'cairo')
