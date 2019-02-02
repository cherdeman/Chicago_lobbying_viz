# Aesthetic options

theme_claire <- theme_minimal() + 
  theme(text = element_text(family = "RobotoCondensed-Regular"),
        plot.title = element_text(family = "Roboto-Regular", size = 16),
        plot.subtitle = element_text(family = "RobotoCondensed-Italic", size = 12),
        plot.caption = element_text(hjust = 1, size = 8),
        panel.grid = element_line(color = "#d9d9d9", size = .3),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  )

dark_colors = c(
  'yellow' = "#FAD510",
  'orange' = "#F98400", 
  'red' = "#CB2314", 
  'blue' = "#046C9A", 
  'purple' = "#273046",
  'green' = "#2F401F", # old green #354823
  'gray' = "#595959"
)



light_colors = c(
  'orange' = "#F2AD00", 
  'red' = "#F4948b",
  'blue' = "#5BBCD6",
  'purple' = "#576B9C",
  'green' = "#6EB643", # #658A43",
  'gray' = "#A6A6A6"
)

dark_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (dark_colors)
  
  dark_colors[cols]
}

light_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (light_colors)
  
  light_colors[cols]
}