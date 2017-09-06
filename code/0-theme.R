#=========================================
# CREATE THEMES
# We'll create two themes:
#
# 1. theme.porttheme
#    - this will be a general theme that
#      we'll apply to most of our charts
#      to format the text, titles, etc
#
# 2. theme.smallmult
#    - we'll apply this exclusively to
#      "small multiple" charts
#      (AKA, trellis charts).  We need this
#      because the axis text needs to be
#      smaller for the small multiples
#=========================================


#----------------------------------------
# GENERAL THEME
# - we'll use this for most of our charts
#   and build on it when we need to
#----------------------------------------

theme.porttheme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank())



#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
# - there are several bar charts that
#   are very wide, and need some
#   special formatting
#------------------------------------

theme.widebar <-
  theme.porttheme +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(legend.title = element_blank(), legend.background = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = c(.9,.55)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))



#------------------------------------
# THEME FOR 'WIDE' BAR CHARTS
#  - we'll use this for small multiple
#    charts.  these also have some
#    special formatting requirements
#------------------------------------

theme.smallmult <-
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))



theme_plot <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),

      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.spacing = unit(c(-.1,0.2,0.02,0.2), "cm"),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "#a8a8a8", size = 0.2),
      panel.grid.major.x = element_blank(),

      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(family="Calibri", hjust = 0, color = "blue", size = 24),
      plot.subtitle = element_text(hjust = 0, color = "#4e4d47", size = 18, debug = F),
      plot.margin = unit(c(.5,.5,1,.5), "cm"),
      plot.caption = element_text(size = 14, hjust = 1, color = "#939184"),

      axis.text.y = element_text(size=14, color="#696969"),
      axis.text.x = element_text(size=14, color="#3f3f3f"),
      axis.line.y = element_line(color="#3f3f3f"),
      axis.line.x = element_line(color="transparent"),
      axis.ticks.x = element_line(colour = "white", size = 0.1),
      axis.ticks.y = element_line(colour = "#a8a8a8", size = 0.2),

      legend.title = element_text(size = 1, colour = "white"),

      ...
    )
}
