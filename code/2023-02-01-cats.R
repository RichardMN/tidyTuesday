# Tidy Tuesday 2023-02-01
# Cats
# Tracking cats in the UK
library(tidyverse)
library(camcorder)
library(maps)
library(sf)
library(geom_sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(units)
library(ggbeeswarm)
library(patchwork)
#library(treemapify)


gg_record(
  dir = file.path(tempdir(), "recording"), 
  device = "png", # we need to set the Cairo device
  width = 8,
  height = 5
)
# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-01-31')
#tuesdata <- tidytuesdayR::tt_load(2023, week = 5)

filenamestub <- "202304_cats"

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Chalkduster",
              items = "Gill Sans",
              subtitle = "Copperplate")
cat_dat <- tuesdata$cats_uk
cat_sf <- cat_dat |>
  filter(manually_marked_outlier==FALSE, algorithm_marked_outlier==FALSE) |>
#  rename(longitude=location_long, latitute=location_lat) |>
  st_as_sf(coords = c("location_long", "location_lat"), 
                   crs = 4326, agr="constant") |>
  group_by(tag_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

uk_sf <- ne_countries(scale = "large", country="United Kingdom", returnclass = "sf")
p_map <- ggplot(data = uk_sf) +
  geom_sf() +
  geom_sf(data = cat_sf, size = 1, 
              shape = 23, aes(color=tag_id)) +
  coord_sf(xlim = c(-6, -4), ylim = c(49.8, 51), expand = FALSE) +
  theme_minimal() +
  guides(colour="none") +
  ggtitle("Cat tracks")
  # labs(
  #   title="Cats of Cornwall",
  #   subtitle="Once you filter out outliers, the cats don't get around that much",
  #   caption="Visualisation: @rmartinnielsen | Data: Kays R, Dunn RR,\nParsons AW, Mcdonald B, Perkins T, Powers S, Shell L, McDonald JL, Cole H, Kikillus H, Woods L, Tindle H, Roetman P (2020)\nThe small home ranges and large local ecological impacts of pet cats. Animal Conservation. doi:10.1111/acv.12563") 

cat_centroids <- st_centroid(cat_sf) 
cat_nrml <- st_geometry(cat_sf) - st_geometry(cat_centroids) |>
  bind_cols(tuesdata$cats_uk_reference)

# ggplot(data=cat_nrml) +
#   geom_sf(size=1, aes)
# Calculate length of each track and assign against the cats
#tuesdata$cats_uk_reference |>
#  select(tag_id) |>
cat_path_lengths <- bind_cols(cat_sf |> select(tag_id), tibble(distance=st_length(cat_sf)) ) |>
 left_join(tuesdata$cats_uk_reference, by=c("tag_id")) |>
  mutate(deploy_duration = lubridate::int_length(lubridate::interval(deploy_on_date, deploy_off_date))) |>
  mutate(deploy_speed = distance / deploy_duration * 60)

p_hrs_vs_speed <- cat_path_lengths |>
  filter(!is.na(hunt), !is.na(animal_reproductive_condition)) |> #, deploy_speed < as_units("m",5)
  ggplot(aes(hrs_indoors, deploy_speed, colour=hunt,shape=animal_reproductive_condition)) +
  geom_beeswarm(size=2,dodge.width = 2) +
  theme_minimal() +
  coord_flip() +
  # xlim(0,5) +
#  coord_cartesian(xlim(c(0,5))) +
  scale_color_brewer(palette="Accent", "Hunter") +
  scale_shape_discrete("Reproductive\ncondition") +
  labs(y = "Average speed when tracked (m/min)",
       x = "Hours indoors per day"
  ) +
  ggtitle("Activity compared to home time")
p_hrs_vs_speed
p_map + p_hrs_vs_speed + plot_annotation(
    title="Cats of Cornwall",
    subtitle="Once you filter out outliers, the cats don't get around that much",
    caption="Visualisation: @rmartinnielsen | Data: Kays R, Dunn RR,\nParsons AW, Mcdonald B, Perkins T, Powers S, Shell L, McDonald JL, Cole H, Kikillus H, Woods L, Tindle H, Roetman P (2020)\nThe small home ranges and large local ecological impacts of pet cats. Animal Conservation. doi:10.1111/acv.12563")

alt_text <- "Dual chart showing: (at left) a map of Cornwall, UK, with small specks indicating the very small tracks of domestic cats; (at right) a beeswarm plot which shows little correlation between how much time cats spend indoors during the day and how quickly they move when outside"

ggsave(filename = file.path("plots/", paste(filenamestub, ".png", sep="")), 
       dpi = 400, width = 8, height = 5, bg = palette$bg)
gg_resize_film( width = 8, height = 5 )
gg_playback(name = file.path("making_of/", paste(filenamestub, ".gif", sep="")),
            first_image_duration = 4,
            last_image_duration = 12,
            frame_duration = .5,
            background = palette$bg)
gg_stop_recording()
