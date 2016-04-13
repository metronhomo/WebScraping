theme_MH <- function() {
  theme(
      axis.text.x=element_text(angle=0,size=15, colour = 'black'),
      axis.text.y=element_text(angle = 90, size=15, colour = 'black'),
      panel.background=element_rect(fill='grey95'),
      strip.background=element_rect(fill="#2c3e50"),
      panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
      strip.text.x = element_text(colour = 'white', size = 15),
      legend.text = element_text(size=15),
      legend.title = element_blank(),
      panel.grid.major = element_line(colour = 'grey70', linetype = 'dashed'),
      panel.grid.minor = element_line(colour = 'grey70', linetype = 'dashed'))
  }

  theme_MH2 <- function() {
  theme(
    axis.text.x=element_text(angle=90, colour = 'black', hjust = 1),
    axis.text.y=element_text(colour = 'black'),
    legend.title=element_blank(),
    panel.grid.major = element_line(colour = 'grey70', linetype = 'dashed'),
    panel.grid.minor = element_line(colour = 'grey70', linetype = 'dashed'))
}