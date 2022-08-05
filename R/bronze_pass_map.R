# Load libraries ---------------------------------------------------------------

library(StatsBombR) # github.com/statsbomb/StatsBombR
library(dplyr)
library(showtext)
library(ggplot2)
library(SBpitch) # github.com/FCrSTATS/SBpitch

# Define custom function -------------------------------------------------------

# Thanks to Thomas Mock!
# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.106 * plot_width
    y_pos = 0.03 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.105 * plot_width
    y_pos = plot_height - logo_height - 0.065 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

# Read data into R -------------------------------------------------------------

weuro_comp <- FreeCompetitions() %>%
  filter(competition_name == "UEFA Women's Euro")

weuro_matches <- FreeMatches(weuro_comp)

weuro_events <- free_allevents(weuro_matches)

# Tidy data --------------------------------------------------------------------

passes_bronze <- weuro_events %>%
  allclean() %>%
  janitor::clean_names() %>%
  filter(type_name == "Pass" &
           is.na(pass_outcome_name) &
           player_name == "Lucy Bronze")

# Prep to plot -----------------------------------------------------------------

font_add_google("Barlow Semi Condensed", "barlow")
showtext_auto()

# Build plot -------------------------------------------------------------------

# Base plot first
base_plot <- create_Pitch(
  grass_colour = "#a7c957", line_colour = "#FFFFFF",
  background_colour = "#6a994e", goal_colour = "#000000",
  goaltype = "line") +
  geom_segment(
    data = passes_bronze %>% filter(match_id == 3847567),
    aes(x = location_x, y = location_y,
        xend = pass_end_location_x, yend = pass_end_location_y),
    lineend = "round", size = 1, colour = "#000000",
    arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) +
  
  labs(
    title = "Lucy Bronze, completed passes",
    subtitle = "UEFA Women's EURO 2022 Final vs. Germany",
    caption = "Plot by @jacquietran") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100) +
  theme(
    plot.title = element_text(size = rel(8.5), family = "barlow", face = "bold"),
    plot.subtitle = element_text(
      size = rel(6), family = "barlow", margin = margin(b = 40, unit = "pt")),
    plot.caption = element_text(
      size = rel(5), family = "barlow", margin = margin(t = 20, unit = "pt")),
    plot.margin = margin(40,40,40,40, unit = "pt"),
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

ggsave(
  here::here("plots/passes_bronze_base.png"),
  base_plot, width = 12, height = 9, units = "in", dpi = 300)

showtext_auto(FALSE)

# Add logo ---------------------------------------------------------------------

plot_with_logo <- add_logo(
  plot_path = here::here("plots/passes_bronze_base.png"), # url or local file for the plot
  logo_path = here::here("img/SB - Core Wordmark - Colour positive.png"), # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 5
  #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(
  plot_with_logo, here::here("plots/passes_bronze_with_logo.png"))

# Add player headshot ----------------------------------------------------------

plot_with_headshot <- add_logo(
  plot_path = here::here("plots/passes_bronze_with_logo.png"), # url or local file for the plot
  logo_path = here::here("img/1908144.jpg"), # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 8
  #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(
  plot_with_headshot, here::here("plots/passes_bronze_combined.png"))
