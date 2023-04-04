# Tidy Tuesday 2023-03-21
# Programming Languages
# Looking at programming languages from the Programming Lanuages Database
library(tidyverse)
library(camcorder)
library(ggrepel)
library(scales)

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

tuesdata <- tidytuesdayR::tt_load('2023-03-21')
#tuesdata <- tidytuesdayR::tt_load(2023, week = 4)

filenamestub <- "202312_programming"
# Or read in the data manually

palette <- list(dark_text = "#3f4140",
                light_text = "#9d938a",
                bg = "#fbf9fa",
                red_text = "#7d3d3b")
# Make a list of fonts so it's easier to change them smoothly
fonts <- list(title = "Copperplate",
              items = "Gill Sans",
              subtitle = "Copperplate")

intercept=1945
slope = 2e4
tuesdata$languages |>
  #filter(github_language_repos > 1000) |>
  slice_max(github_language_repos, n=100) |>
  mutate(label_text = if_else(appeared < 1965 | github_language_repos > (appeared-intercept)*slope, title, NA)) |>
#  mutate(label_text = if_else(appeared < 1960 , title, NA)) |>
  ggplot(aes(x=appeared, y=github_language_repos,colour=type, label=label_text)) +
  geom_jitter() +
  theme_minimal() +
  xlim(1945,NA) +
  ylim(0,1.75e7) + 
  scale_y_continuous(name="Number of GitHub repos\nusing the language",labels=label_number(suffix=" million", scale = 1e-6)) +
  scale_x_continuous(name="When the language first appeared")+
#  scale_colour_brewer(palette="Dark2") +
  geom_label_repel(position="nudge", force_pull = .01, force=1.5, point.padding=1) +
  theme(
    title = element_text(family = fonts$title),
    #plot.caption = element_text(family = fonts$title),
    #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(1, "line"),
    legend.spacing = unit(0.5, "line"),
    legend.position = c(.2,.6)
  ) +
  labs(
    title="GitHub favours the new",
    subtitle="Comparison of how many GitHub repos use a language with when the language first appeared",
    caption="Visualisation: @rmartinnielsen Data: Programming Language DataBase") 


alt_text <- "Scatterplot comparing when languages appeared and how often they are used in GitHub. Regular expressions, Fortran, Assembly are old languages and rarely used. JaveScript, Java, HTML and Python (all since 1990) have more than 9 million repos; most other languages have 5 million or fewer"

ggsave(filename = file.path("plots/", paste(filenamestub, ".png", sep="")), 
       dpi = 400, width = 8, height = 5, bg = palette$bg)
gg_resize_film(height = 5, width = 8)
gg_playback(name = file.path("making_of/", paste(filenamestub, ".gif", sep="")),
            first_image_duration = 4,
            last_image_duration = 12,
            frame_duration = .5,
            background = palette$bg)
gg_stop_recording()
