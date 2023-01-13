# Tidy Tuesday 2023-01-10
# Birds
# Inspired - and in a few cases copied from - Cara Thompson's cleaner plot

library(tidyverse)
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-01-10')
tuesdata <- tidytuesdayR::tt_load(2023, week = 02)

feederwatch <- tuesdata$feederwatch

species_translation <- readr::read_csv('https://feederwatch.org/wp-content/uploads/2022/08/PFW-species-translation-table.csv')
# Or read in the data manually

# feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
# site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

#systemfonts::register_variant("Gill Sans", "Gill Sans", weight = "light")
#systemfonts::register_font("Lucida Handwriting")
#systemfonts::register_font("Copperplate Gothic Bold Regular")
# Look for birds which were seen in Ontario where the sighting is considered valid
# Mix in the American English name
# Count all sightings
# Look for those where there are only 5 or fewer sightings
rarely_spotted_counts <- tuesdata$PFW_2021_public %>%
  filter(valid == 1) %>%
  # split these two out to make it simpler to focus geographically
  filter( subnational1_code == 'CA-ON') %>%
  left_join(species_translation, by = "species_code") %>%
  count(american_english_name, sort=TRUE) %>%
  filter( n <= 5) %>%
  rename(count=n) %>%
  group_by(count) %>%
  mutate(y_pos = rank(desc(american_english_name)))

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Copperplate",
              items = "Gill Sans",
              subtitle = "Copperplate")

# Choose a bird which is seen in Ontario (if rarely)
bird_image <- "https://www.rspb.org.uk/globalassets/images/birds-and-wildlife/bird-species-illustrations/snow-bunting_male_breedingplumage_1200x675.jpg"

ggplot(rarely_spotted_counts) +
  # lifted from Cara Thompson's version
  cowplot::draw_image(bird_image, x = 3.25, y = 10, 
                      scale = 12, vjust = 0.5, hjust = 0.5) +
  ggtext::geom_textbox(aes(x = count,
                         y = y_pos,
                         label = gsub(" \\(.*?\\)", "", american_english_name)),
                     hjust = 0.5, 
                     halign = 0.5,
                     width = unit(35, "lines"),
                     size = 3,
                     fill = NA,
                     box.colour = NA,
                     colour = palette$dark_text) +
  ggtext::geom_textbox(aes(x = 5.5,
                           y = 17,
                           label = "Lesser (rarely) spotted birds in Ontario"),
                       hjust = 1,
                       halign = 1,
                       vjust = 1, valign = 1,
                       width = unit(35, "lines"),
                       size = 8,
                       lineheight = 1.3,
                       fill = NA,
                       box.colour = NA,
                       colour = palette$dark_text,
                       family = fonts$title) +
  ggtext::geom_textbox(aes(x = 5.5,
                            y = 15,
                            label = "Dataviz: @rmartinnielsen after @cararthompson<br>Source: Project FeederWatch | Image: rspb.org.uk"),
                        hjust = 1,
                        halign = 1,
                        vjust = 1, valign = 1,
                        width = unit(25, "lines"),
                        size = 4,
                        lineheight = 1.3,
                        fill = NA,
                        box.colour = NA,
                        colour = palette$light_text,
                        family = fonts$subtitle) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c("Once", "Twice", "Thrice", "Four times", "Five times"),
                     limits = c(0.5, 5.5)) +
  theme_minimal() +
  theme(text = element_text(family = fonts$items, colour = palette$light_text),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = palette$red_text),
        panel.grid = element_blank(), 
        plot.caption = element_text(family = "Lucida Handwriting", size = 6, colour = palette$light_text, margin = margin(12, 0, 0, 0),
                                    hjust = 0.92))

ggsave(filename = file.path("plots/", 
                            "202301_birds.png"), 
       dpi = 400, width = 10, height = 6, bg = palette$bg)
