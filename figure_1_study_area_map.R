library(cowplot)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(mapview)

# delimit study area------------
farm_bbox <-
  st_bbox(c(
    xmin = 0,
    xmax = 35,
    ymin = 70,
    ymax = 50
  ))

# read fish raw data and convert to sf--------
raw <- read_csv('data/raw_data_percentage.csv')

raw_sf <-
  st_as_sf(raw,
           coords = c("Longitude" , "Latitude"),
           crs = 4326)



# read and crop Norway farms shapefile-------
farms <-
  read_sf(
    'C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/Norwegian farms/Akvakultur - tillatelser.shp'
  ) %>%
  st_transform (4326) %>%
  st_crop(st_bbox(c(
    xmin = 21.5,
    xmax = 21.9,
    ymin = 70.3,
    ymax = 70.15
  )))

# Map of Norway from europa.eu--------
norway_high_res <-
  st_read(
    'C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/Europe_coastline_shapefile/Europe_coastline.shp'
  ) %>%
  st_transform (4326) %>%
  st_crop(st_bbox(c(
    xmin = 21.1,
    xmax = 22.4,
    ymin = 70.0,
    ymax = 70.5
  )))

mapview(norway_high_res)

# medium res map of Norway------------
norway_med <-
  ne_countries(country = 'norway',
               scale = "medium",
               returnclass = "sf") %>%
  st_crop(st_bbox(farm_bbox)) %>%
  st_transform (4326)

st_bbox(raw_sf)

mapview(norway_high_res) + mapview(farms, color = 'darkgreen') + mapview(raw_sf, zcol = "Treatment")

norway_map <-
  ggplot() +
  geom_sf(data = norway_med, fill = 'transparent') +
  geom_point(
    aes(21.8, 70.2),
    size = 5,
    alpha = .5,
    color = 'grey10'
  ) +
  theme_void() +
  NULL

# map of the study area-----------
study_area <-
  raw %>%
  ggplot() +
  geom_sf(
    data = st_cast(norway_high_res, 'POLYGON'),
    size = 0.1,
    fill = 'gray97'
  ) +
  geom_point(
    aes(Longitude, Latitude, fill = Treatment),
    position = position_jitter(
      width = .005,
      height = .005,
      seed = 123
    ),
    alpha = .7,
    pch = 21
  ) +
  geom_sf(
    data = farms,
    color = 'darkgreen',
    fill = 'transparent',
    shape = 0,
    size = 2.5
  ) +
  coord_sf(
    xlim = c(21.1, 22.1),
    ylim = c(70.12, 70.45),
    expand = F
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.1,
    style = 'ticks',
    text_cex = 0.5
  ) +
  scale_fill_discrete(name = NULL) +
  labs(x = NULL, y = NULL) +
  annotate(
    "text",
    x = 21.95,
    y = 70.21,
    label = "Inner \nLangfjord",
    size = 2.5
  ) +
  annotate(
    "text",
    x = 21.6,
    y = 70.38,
    label = "Outer \nLangfjord",
    size = 2.5
  ) +
  annotate(
    "text",
    x = 21.4,
    y = 70.285,
    label = "Frakkfjord",
    size = 2.5
  ) +
  annotate(
    "text",
    x = 21.33,
    y = 70.15,
    label = "Olderfjord",
    size = 2.5
  ) +
  theme_minimal(base_size = 7) +
  scale_y_continuous(breaks = c(70.2, 70.4)) +
  scale_x_continuous(breaks = c(21.3, 21.6, 21.9)) +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_rect(
      colour = "gray80",
      fill = NA,
      linewidth = .3
    ),
    legend.position = c(.09, .1)
  )

study_area

# combine both maps----
ggdraw() +
  draw_plot(study_area) +
  draw_plot(
    norway_map,
    x = .075,
    y = .6,
    width = .3,
    height = .4
  )

# save figure----------
ggsave(
  last_plot(),
  filename = "figures/figure_1_study_sites.png",
  height = 5,
  width = 5,
  dpi = 600,
  bg = "white"
)