# Load libraries ---------------------------------------------------------------

library(StatsBombR) # github.com/statsbomb/StatsBombR
library(dplyr)
library(ggraph)
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
    x_pos = plot_width - logo_width - 0.06 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.06 * plot_width
    y_pos = plot_height - logo_height - 0.03 * plot_height
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

# Extract England completed passes
passes_england <- weuro_events %>%
  allclean() %>%
  janitor::clean_names() %>%
  filter(stringr::str_detect(team_name, "England") &
           type_name == "Pass" &
           is.na(pass_outcome_name))

# Extract completed passes to and from Leah Williamson
passes_selected <- passes_england %>%
  filter(player_name == "Leah Williamson" | pass_recipient_name == "Leah Williamson") %>%
  select(match_id, player_name, pass_recipient_name) %>%
  rename(from = player_name, to = pass_recipient_name) %>%
  mutate(
    # Identify passing direction
    direction = case_when(
      from == "Leah Williamson" ~ "From Williamson",
      TRUE                      ~ "To Williamson"),
    # Simplify names
    from = stringr::word(from, -1),
    to = stringr::word(to, -1))

# Convert data frame to graph
passes_graph <- tidygraph::as_tbl_graph(passes_selected) %>%
  tidygraph::activate(nodes) %>%
  tidygraph::activate(edges) %>%
  group_by(match_id) %>%
  mutate(edge_weights = runif(n())) %>%
  ungroup()

# Prep to plot -----------------------------------------------------------------

font_add_google("Barlow Semi Condensed", "barlow")
showtext_auto()

# Build plot -------------------------------------------------------------------

# Set custom strip labels
opponents <- c(
  "3835319" = "VS. AUSTRIA",
  "3835327" = "VS. NORWAY",
  "3835335" = "VS. NORTHERN IRELAND",
  "3844384" = "VS. SPAIN",
  "3845506" = "VS. SWEDEN",
  "3847567" = "VS. GERMANY")

# Set custom colours for pass direction
direction_colours <- c(
  "From Williamson" = "#d62839",
  "To Williamson"   = "#8CC5DC")

base_plot <- ggraph(passes_graph, layout = 'linear') + 
  facet_wrap(~match_id, ncol = 3, labeller = labeller(match_id = opponents)) +
  geom_edge_arc(
    aes(colour = direction, edge_width = edge_weights),
    arrow = arrow(length = unit(4, 'mm'))) + 
  scale_edge_color_manual(values = direction_colours) +
  geom_node_point(size = 4, colour = "#780000") +
  geom_node_text(
    aes(label = name),
    angle = 90, hjust = 1, nudge_y = -0.4, size = 20, family = "barlow", colour = "#000000") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(4, units = "lines"),
    strip.text = element_text(size = rel(12), family = "barlow", face = "bold"),
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank())

ggsave(
  here::here("plots/passes_williamson_base.png"),
  base_plot, width = 20, height = 12, units = "in", dpi = 300)

# Import base plot and add titles ----------------------------------------------

# Import map image
base_img <- magick::image_read(here::here("plots/passes_williamson_base.png")) |>
  magick::image_rotate(degrees = 90)

base_raster <- grid::rasterGrob(base_img, width = unit(1,"npc"), height = unit(1,"npc"))

# Build plot with titles
plot_with_titles <- ggplot() +
  # Base plot image
  annotation_custom(base_raster, -Inf, Inf, -Inf, Inf) +
  labs(
    title = "Completed passes to & from Leah Williamson",
    subtitle = "UEFA Women's EURO 2022",
    caption = "Plot by @jacquietran") +
  theme_void() +
  theme(
    plot.title = element_text(size = rel(8.5), family = "barlow", face = "bold"),
    plot.subtitle = element_text(
      size = rel(6.5), family = "barlow", margin = margin(b = 20, unit = "pt")),
    plot.caption = element_text(
      size = rel(5.5), family = "barlow", margin = margin(t = 40, unit = "pt")),
    plot.margin = margin(40,40,40,40, unit = "pt"),
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

ggsave(
  here::here("plots/passes_williamson_base_with_titles.png"),
  plot_with_titles, width = 12, height = 20, units = "in", dpi = 300)

showtext_auto(FALSE)

# Add logo ---------------------------------------------------------------------

plot_with_logo <- add_logo(
  plot_path = here::here("plots/passes_williamson_base_with_titles.png"), # url or local file for the plot
  logo_path = here::here("img/SB - Core Wordmark - Colour positive.png"), # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 5
  #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(
  plot_with_logo, here::here("plots/passes_williamson_with_logo.png"))

# Add player headshot ----------------------------------------------------------

plot_with_headshot <- add_logo(
  plot_path = here::here("plots/passes_williamson_with_logo.png"), # url or local file for the plot
  logo_path = here::here("img/250047309.jpg"), # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 7
  #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(
  plot_with_headshot, here::here("plots/passes_williamson_combined.png"))
