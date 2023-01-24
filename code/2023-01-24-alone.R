# Tidy Tuesday 2023-01-24
# Alone
# Analysis and presentation of data from the TV series Alone
library(tidyverse)
library(camcorder)
library(treemapify)

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

tuesdata <- tidytuesdayR::tt_load('2023-01-24')
tuesdata <- tidytuesdayR::tt_load(2023, week = 4)

filenamestub <- "202303_alone"
# Or read in the data manually

# survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
# loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loudouts.csv')
# episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
# seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')


who_carries_what <- left_join(tuesdata$survivalists, tuesdata$loadouts, by = c("name", "season")) |>
  filter(name != 'Jim Baird')
  # UGLY fix to avoid duplicates for season 4, where the loadouts lists the same
  # items for both members of a team

winners_carry <- who_carries_what |> 
  filter(result==1) |>
  select(item) 

top_two_carry <- who_carries_what |> 
  filter(result<=2) |>
  select(item) 

winners_count <- winners_carry |>
  count(item) |>
  mutate(item = fct_reorder(item, n, max)) |>
  mutate(`Who carries`="Winners")
  
top_two_count <- top_two_carry |>
  count(item) |>
  mutate(item = fct_reorder(item, n, max)) |>
  mutate(`Who carries`="Winners and first runner-up")

counts <- bind_rows(winners_count, top_two_count)

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Chalkduster",
              items = "Gill Sans",
              subtitle = "Copperplate")

counts |>
  mutate(`Who carries`=factor(`Who carries`, levels=c('Winners and first runner-up', 'Winners'))) |>
  ggplot(aes(item, n, fill=`Who carries`)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(item, n - .5, label = n), family = fonts$items, size = 5, colour = palette$dark_text) +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2") +
  theme(
    title = element_text(family = fonts$title),
    #plot.caption = element_text(family = fonts$title),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(1, "line"),
    legend.spacing = unit(0.5, "line"),
    legend.position = c(.7,.2)
  ) +
  labs(
    title="What do you take to the woods?",
    subtitle="Which items winners and first runners-up took with them into\nthe wilderness across 9 seasons of Alone",
    caption="Visualisation: @rmartinnielsen Data: Dan Oehm, Alone package dataset for R") 
  #+
  #facet_grid(cols=vars(`Who carries`))

alt_text <- "Horizontal bar chart showing which items winners and first runners-up took with them into the wilderness in 9 seasons of Alone.
 Axe                  9
 Saw                  9
 Ferro rod            8
 Pot                  8
 Sleeping bag         8
 Fishing gear         7
 Multitool            7
 Trapping wire        6
 Bow and arrows       5
 Paracord             5
 Gillnet              4
 Rations              4
 Knife                3
 Bivy bag             1
 Canteen              1
 Frying pan           1
 Sharpening stone     1
 Slingshot            1
 Tarp                 1
 Wire                 1
 Winners and first runners-up:
 Pot                 18
 Fishing gear        17
 Saw                 17
 Axe                 16
 Ferro rod           16
 Sleeping bag        16
 Paracord            14
 Bow and arrows      12
 Trapping wire       12
 Multitool           11
 Rations             11
 Knife                9
 Gillnet              7
 Tarp                 6
 Canteen              2
 Slingshot            2
 Bivy bag             1
 Frying pan           1
 Sharpening stone     1
 Wire                 1"

ggsave(filename = file.path("plots/", paste(filenamestub, ".png", sep="")), 
       dpi = 400, width = 8, height = 5, bg = palette$bg)
gg_resize_film(height = 5, width = 8)
gg_playback(name = file.path("making_of/", paste(filenamestub, ".gif", sep="")),
            first_image_duration = 4,
            last_image_duration = 12,
            frame_duration = .5,
            background = palette$bg)
gg_stop_recording()
