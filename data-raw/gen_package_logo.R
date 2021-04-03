# Code to create the package logo

library(hexSticker)
library(ggplot2)

data <- tibble::tibble(
  x = 1,
  y = 1
)

p <- ggplot(data) +
  theme_void() +
  theme_transparent()

sticker(p,
  package = "ocR Project", p_size = 20, p_x = 1, p_y = 1,
  l_x = 0.5, l_y = 1, dpi = 300,
  spotlight = TRUE,
  filename = "man/figures/logo.png",
  h_fill = "#002642", # Fill Color Hexagon
  h_color = "#0F5587", # Color Border
  p_color = "#FBFFF1", # Color Package Name
  white_around_sticker = TRUE,
  url = "https://github.com/Tim-Lukas-H/ocrProject",
  u_color = "#FBFFF1", # URL Color
  u_size = 2
)
