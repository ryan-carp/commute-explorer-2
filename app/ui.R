################################################################################
# UI of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:34:50
################################################################################

library(bslib)

# Commute Explorer UI -----------------------------------------------------

commute_explorer_ui <- page_sidebar( 
  
  
  # Set the CSS theme
  theme = bs_theme(bg = "#101010", # black
                   fg = "#E69F00", #white 
                   primary = "#E69F00", # orange 
                   secondary = "#0072B2", # blue 
                   success = "#009E73"), #green 
  
  
  # Add title
  title = "Engagement Strategy Eficacy Dashboard", 
  
  
  # Add sidebar elements
  sidebar = sidebar(
    #class = "bg-secondary",
    sidebar_content
  ),
  
  # nav_item(
  #   input_dark_mode(id = "dark_mode", mode = "light")
  # ), 
  
  # Layout non-sidebar elements
  
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    height = "950px",
    card(full_screen = TRUE,card_header("Overall Effect of Engagement", class = "h6 bg-primary"),
         plotlyOutput("line")),
    card(full_screen = TRUE,card_header("Effect Size of Engagement on Screen Rate", class = "h6 bg-primary"),
         plotlyOutput("bar")),
    card(full_screen = TRUE,card_header("Barriers on Screen Rate", class = "h6 bg-primary"),
         plotlyOutput("plot")),
    card(full_screen = TRUE,card_header("Decaying Effect over Time", class = "h6 bg-primary"),
         plotlyOutput("last")))
  
)

# App template ------------------------------------------------------------

htmlTemplate(
  filename = "www/index.html",
  commute_explorer = commute_explorer_ui
)
