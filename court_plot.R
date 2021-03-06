# plot NCAA basketball courts

library(data.table)
library(tidyverse)

# funciton to get x , y coords of cirles

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  
  data_frame(
    x = center[1] + radius * cos(angles),
    y = center[2] + radius * sin(angles)
  )
}

# court dims 

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14
college_three_radius = 20.75
college_three_radius_2020 = 21.65

# data frame with court elements

court_points = data_frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
  y = c(height, 0, 0, height, height),
  desc = "perimeter"
)

court_points = bind_rows(court_points , data_frame(
  x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
  y = c(0, key_height, key_height, 0),
  desc = "outer_key"
))

court_points = bind_rows(court_points , data_frame(
  x = c(-backboard_width / 2, backboard_width / 2),
  y = c(backboard_offset, backboard_offset),
  desc = "backboard"
))

court_points = bind_rows(court_points , data_frame(
  x = c(0, 0),
  y = c(backboard_offset, backboard_offset + neck_length),
  desc = "neck"
))

foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)

foul_circle_top = filter(foul_circle, y > key_height) %>%
  mutate(desc = "foul_circle_top")

foul_circle_bottom = filter(foul_circle, y < key_height) %>%
  mutate(desc = "foul_circle_bottom")

hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
  mutate(desc = "hoop")

restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted")

three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
  filter(y >= three_point_side_height)

college_three_circle = circle_points(center = c(0, hoop_center_y), radius = college_three_radius) %>%
  filter(y >= hoop_center_y)

college_three_circle_2020 = circle_points(center = c(0, hoop_center_y), radius = college_three_radius_2020) %>%
  filter(y >= hoop_center_y)

three_point_line = data_frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)

college_three_line = data_frame(
  x = c(college_three_radius, college_three_radius, college_three_circle$x, -college_three_radius, -college_three_radius ),
  y = c(0, hoop_center_y, college_three_circle$y, hoop_center_y, 0),
  desc = "college_three_line"
)

college_three_line_2020 = data_frame(
  x = c(college_three_radius_2020, college_three_radius_2020, college_three_circle_2020$x, -college_three_radius_2020 , -college_three_radius_2020),
  y = c(0, hoop_center_y, college_three_circle_2020$y, hoop_center_y, 0),
  desc = "college_three_line_2020"
)

college_key = data_frame(
  x = c(inner_key_width / 2, inner_key_width / 2, -inner_key_width / 2, -inner_key_width / 2),
  y = c(0, key_height, key_height, 0),
  desc = "college_key"
)

court_points = bind_rows(
  court_points,
  foul_circle_top,
  foul_circle_bottom,
  hoop,
  restricted,
  three_point_line,
  college_three_line,
  college_three_line_2020,
  college_key
) %>%
  mutate(dash = (desc == "foul_circle_bottom"))

college_court_points = court_points %>%
  filter(!(desc %in% c("three_point_line", "foul_circle_bottom", "outer_key")))

court_bg_color = "#EEEEEE" 
court_lines_color = "#000004"

theme_court = function(base_size = 12) {
  theme_bw(base_size) +
    theme(
      #text = element_text(color = "#f0f0f0"),
      plot.background = element_rect(fill = court_bg_color, color = court_bg_color),
      panel.background = element_rect(fill = court_bg_color, color = court_bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      #legend.background = element_rect(fill = court_bg_color, color = court_bg_color),
      #legend.position = "bottom",
      #legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

college_court = ggplot() +
  geom_path(data = college_court_points,
            aes(x = x, y = y - 5.25, group = desc),
            color = court_lines_color) +
  coord_fixed(ylim = c(-5.3, 32), xlim = c(-25, 25)) +
  theme_court(base_size = 12)
