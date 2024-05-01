library(showtext)

font_add_google("Open Sans")
showtext_auto()

theme_reason <- function(base_line_size = 0.5,
                         base_family = "Open Sans",
                         base_size = 12) {
  theme_minimal(base_family = base_family,
                base_size = base_size,
                base_line_size = base_line_size) %+replace%
    theme(
      # line = element_line(linewidth = base_line_size),
      # axis.line.x = element_line(),
      axis.ticks = element_line(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "grey92"),
      legend.position = "top"
    )
}


ggplot(mtcars, aes(mpg, wt)) +
geom_point() +
theme_reason()
#   
# p

