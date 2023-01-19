# Tidy Tuesday 2023-01-17
# Artists

library(tidyverse)
library(camcorder)
library(treemapify)

gg_record(
  dir = file.path(tempdir(), "recording"), 
  device = "png", # we need to set the Cairo device
  width = 8,
  height = 5
)
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-01-17')
tuesdata <- tidytuesdayR::tt_load(2023, week = 03)

arthistory <- tuesdata$artists

# Or read in the data manually

#artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')
# arthistory |>
#   ggplot(aes(x=year, y=space_ratio_per_page_total, color=artist_nationality_other)) +
#   geom_jitter() +
#   theme_minimal()
# 
# arthistory |>
#   group_by(year, artist_nationality_other) |>
#   summarise(area = sum(space_ratio_per_page_total)) |>
#   ggplot(aes(area=area, fill=artist_nationality_other)) +
#   geom_treemap() +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Blues") +
#   facet_wrap(vars(year))

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Copperplate",
              items = "Gill Sans",
              subtitle = "Copperplate")

arthistory |>
  group_by(year, artist_nationality_other) |>
  summarise(area = sum(space_ratio_per_page_total)) |>
  ggplot(aes(area=area, fill=artist_nationality_other)) +
  geom_treemap() +
  coord_fixed() +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues", name="") +
  labs(title="Taking up space",
       subtitle="How much of each history of art book is assigned to the top 5 nations",
       caption="Visualisation: @rmartinnielsen Data: Lemus S, Stam H (2022). arthistory: Art History Textbook Data.") +
  theme(text = element_text(family = fonts$items, colour = palette$light_text),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10, colour = palette$red_text),
      panel.grid = element_blank(), 
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.key.size = unit(0.75, "line"),
      legend.spacing = unit(0.5, "line"),
      legend.position = c(0.4,0.075)#,
      # plot.caption = element_text(family = "Lucida Handwriting", size = 6,
      #                             colour = palette$dark_text, margin = margin(12, 0, 0, 0),
      #                             hjust = 0.92)
      ) +
  facet_wrap(vars(year))

arthistory |>
  mutate(yearbook=sprintf("%d %s", year, ifelse(book == "Gardner", "G", "J"))) |>
  group_by(yearbook, artist_nationality_other) |>
  summarise(area = sum(space_ratio_per_page_total)) |>
  ggplot(aes(area=area, fill=artist_nationality_other)) +
  geom_treemap() +
  coord_fixed() +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu", name="") +
  labs(title="Taking up space",
       subtitle="How much of each history of art book is shared among the top 5 nations\nGardner’s Art Through the Ages (G) & Janson’s History of Art (J)",
       caption="Visualisation: @rmartinnielsen Data: Lemus S, Stam H (2022). arthistory: Art History Textbook Data.") +
  theme(text = element_text(family = fonts$items, colour = palette$light_text),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = palette$red_text),
        panel.grid = element_blank(), 
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.key.size = unit(1, "line"),
        legend.spacing = unit(0.5, "line"),
        legend.position = c(0.2,0.075)#,
        # plot.caption = element_text(family = "Lucida Handwriting", size = 6,
        #                             colour = palette$dark_text, margin = margin(12, 0, 0, 0),
        #                             hjust = 0.92)
  ) +
  facet_wrap(ncol = 6, vars(yearbook))

alt_text <- "A faceted set of treemaps showing the varying distribution of the page space accorded to artists of five different nations (and \"other\") in 25 art history text books from 1926 to 2020 [Gardner’s Art Through the Ages (G) & Janson’s History of Art (J)]. American and French artists predominate, followed by British, German and Spanish."

ggsave(filename = file.path("plots/", 
                            "202302_artists.png"), 
       dpi = 400, width = 10, height = 6, bg = palette$bg)

gg_playback(name = file.path("making_of/", "202302_artists.gif"),
            first_image_duration = 4,
            last_image_duration = 12,
            frame_duration = .5)
gg_stop_recording()
