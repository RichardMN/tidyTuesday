# Tidy Tuesday 2023-02-28
# African languages
# African language sentiments
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


filenamestub="africa"
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

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-02-28')
#tuesdata <- tidytuesdayR::tt_load(2023, week = 9)

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

# c("positive", "neutral", "negative"))

sentiment_counts <- afrisenti |> 
  mutate(label=factor(label, levels = c("positive", "neutral", "negative"))) |>
  group_by(language_iso_code, label) |>
  count() |>
  left_join(languages) 

language_regions <- full_join(country_regions, language_countries) |>
  select(-country)

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Chalkduster",
              items = "Gill Sans",
              subtitle = "Copperplate")

ggplot(sentiment_counts, aes(fill=label, x=n, y=language)) +
  geom_bar(position="fill", stat="identity") +
  theme_minimal()
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
