# Tidy Tuesday 2023-01-17
# Artists
# This is a revised version which tries to present the same graphs in
# a drawn-out "gallery-like" presentation

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

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Copperplate",
              items = "Gill Sans",
              subtitle = "Copperplate")

arthistory |>
  #mutate(yearbook=sprintf("%d %s", year, ifelse(book == "Gardner", "G", "J"))) |>
  mutate(book = ifelse(book == "Gardner", "G", "J")) |>
  group_by(year, book, artist_nationality_other) |>
  summarise(area = sum(space_ratio_per_page_total)) |>
  ggplot(aes(area=area, fill=artist_nationality_other)) +
  geom_treemap() +
  coord_fixed() +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu", name="") +
  labs(title="Taking up space",
       subtitle ="How much of each history of art book\nis shared among the top 5 nations\nGardner’s Art Through the Ages (G)\n& Janson’s History of Art (J)",

       #subtitle="How much of each history of art book is shared among the top 5 nations\nGardner’s Art Through the Ages (G) & Janson’s History of Art (J)",
       caption="Visualisation: @rmartinnielsen\nData: Lemus S, Stam H (2022).\narthistory: Art History Textbook Data.") +
  theme(text = element_text(family = fonts$items, colour = palette$light_text),
        aspect.ratio = 1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.x = element_text(size = 10, colour = palette$red_text),
        axis.text.x = element_blank(),
        panel.grid = element_blank(), 
        plot.subtitle = element_text(size=8),
        plot.caption = element_text(size=8),
        legend.direction = "vertical",
        legend.justification = "left",
        legend.key.size = unit(.5, "line"),
        legend.spacing = unit(0.125, "line"),
        legend.position = c(1.5,0.05)#,
        # plot.caption = element_text(family = "Lucida Handwriting", size = 6,
        #                             colour = palette$dark_text, margin = margin(12, 0, 0, 0),
        #                             hjust = 0.92)
  ) +
  facet_grid(cols = vars(book), rows = vars(year)) +
  theme(strip.text.x = element_text(size=6),
        strip.text.y = element_text(angle=0),
        ) 

alt_text <- "A faceted set of treemaps showing the varying distribution of the page space accorded to artists of five different nations (and \"other\") in 25 art history text books from 1926 to 2020 [Gardner’s Art Through the Ages (G) & Janson’s History of Art (J)]. American and French artists predominate, followed by British, German and Spanish."

ggsave(filename = file.path("plots/", 
                            "202302_artists_gallery.png"), 
       dpi = 400, width = 4, height = 12, bg = palette$bg)
gg_resize_film(height = 12, width = 4)
gg_playback(name = file.path("making_of/", "202302_artists_gallery.gif"),
            first_image_duration = 4,
            last_image_duration = 12,
            frame_duration = .5,
            background = palette$bg)
gg_stop_recording()
