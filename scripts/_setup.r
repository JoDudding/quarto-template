#-------------------------------------------------------------------------------
# r options
#-------------------------------------------------------------------------------

options(
  dplyr.width = Inf,
  papersize = "a4",
  tab.width = 2,
  width = 80,
  max.print = 25,
  stringsAsFactors = FALSE,
  lubridate.week.start = 6,
  tibble.print_max = 25,
  tibble.print_min = 25,
  tibble.width = Inf,
  dplyr.summarise.inform = FALSE,
  tidyverse.quiet = TRUE
)

#-------------------------------------------------------------------------------
# packages
#-------------------------------------------------------------------------------

library(tidyverse)
library(scales)
library(patchwork)
library(ggtext)
library(glue)
library(gt)

#-------------------------------------------------------------------------------
# font
#-------------------------------------------------------------------------------

library(showtext)
font_add_google("Montserrat", "Montserrat")
showtext_auto()

#-------------------------------------------------------------------------------
# theme
#-------------------------------------------------------------------------------

jo_primary <- "#5F187F"
jo_secondary <- "#FF823E"
jo_body <- "#340048"
jo_rev <- "#FFFFE0"
jo_bg <- "transparent"
jo_grid <- "#B486D4"


jo_pal_grad <- colorRampPalette(c(jo_primary, jo_secondary))

scale_fill_jo_d <- function(n = 6, ...) {
  scale_fill_manual(values = jo_pal_grad(n), ...)
}

scale_colour_jo_d <- function(n = 6, ...) {
  scale_colour_manual(values = jo_pal_grad(n), ...)
}

scale_fill_jo_c <- function(...) {
  scale_fill_gradient(low = jo_primary, high = jo_secondary, ...)
}

scale_colour_jo_c <- function(...) {
  scale_colour_gradient(low = jo_primary, high = jo_secondary, ...)
}


theme_jo <- function(
    add_colour = jo_primary,
    base_colour = jo_body,
    base_bg = jo_bg,
    base_family = "Montserrat",
    base_size = 20,
    line_colour = jo_grid,
    ...
) {

  theme_bw(...) %+replace%
    theme(
      # default font
      text = element_text(family = base_family, size = base_size),
      # align title and caption to the plot not the panel
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      # change the title and caption to markdown and move them further from the plot
      plot.title = element_text( #element_markdown(
        size = rel(2),
        hjust = 0,
        margin = margin(c(0, 0, 10, 0)),
        colour = add_colour
      ),
      plot.subtitle = element_text( #element_markdown(
        size = rel(1.5),
        hjust = 0,
        margin = margin(c(0, 0, 15, 0))
      ),
      plot.caption = element_text( #element_markdown(
        hjust = 0,
        margin = margin(c(10, 0, 0, 0))
      ),
      # allow the axis values to the markdown as well
      #axis.text = element_markdown(),
      # remove the panel border
      panel.border = element_blank(),
      # set the background colour
      panel.background = element_rect(fill = base_bg, colour = NA),
      plot.background = element_rect(fill = base_bg, colour = NA),
      legend.background = element_rect(fill = base_bg, colour = NA),
      # put in the axis lines with a slightly thicker line than the gridlines
      axis.line = element_line(colour = line_colour, linewidth = 0.2),
      # make the tickmarks the same colour
      axis.ticks = element_line(colour = line_colour),
      # facet strip text left aligned with extra space above
      strip.text = element_markdown(
        hjust = 0, margin = margin(c(10, 0, 0, 0)), colour = add_colour
      ),
      # clear colour and fill for strip
      strip.background = element_rect(colour = NA, fill = NA),
      # dotted gridlines
      panel.grid = element_line(linetype = 'dotted'),
      # ability to use a different colour for the gridlines
      panel.grid.major.x = element_line(colour = line_colour, linetype = 1, linewidth = 0.15),
      panel.grid.major.y = element_line(colour = line_colour, linetype = 1, linewidth = 0.15),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

#-------------------------------------------------------------------------------
# set theme
#-------------------------------------------------------------------------------

theme_set(theme_jo())

#-------------------------------------------------------------------------------
# functions
#-------------------------------------------------------------------------------

#---- scale_x_pct --------------------------------------------------------------
#' formats the scale with percentages.

scale_x_pct <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_x_continuous(
    labels = scales::percent_format(accuracy = accuracy, big.mark = ","),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_y_pct <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_y_continuous(
    labels = scales::percent_format(accuracy = accuracy, big.mark = ","),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_fill_pct <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_fill_gradient(
    labels = scales::percent_format(accuracy = accuracy, big.mark = ","),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_colour_pct <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_colour_gradient(
    labels = scales::percent_format(accuracy = accuracy, big.mark = ","),
    breaks = breaks,
    expand = expand,
    ...
  )
}

#---- scale_*_comma ------------------------------------------------------------
#' formats the scale with commas.

scale_x_comma <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_x_continuous(
    labels = scales::comma_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_y_comma <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_y_continuous(
    labels = scales::comma_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_fill_comma <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_fill_gradient(
    labels = scales::comma_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_colour_comma <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_fill_gradient(
    labels = scales::comma_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

#---- scale_*_dollar -----------------------------------------------------------
#' formats the scale as currency.

scale_x_dollar <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_x_continuous(
    labels = scales::dollar_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_y_dollar <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_y_continuous(
    labels = scales::dollar_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_fill_dollar <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_fill_gradient(
    labels = scales::dollar_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}

scale_colour_dollar <- function(
    accuracy = 1L,
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, .05)),
    ...
) {
  scale_fill_gradient(
    labels = scales::dollar_format(accuracy = accuracy),
    breaks = breaks,
    expand = expand,
    ...
  )
}
