# Code to create epicurve ggplot 

plot_epicurve <- ggplot(linelist) +
  geom_bar(aes(x = date_onset), 
           fill = "#0A2F41",
           color = "white") +
  labs(
    x = "Onset date",
    y = "Number of cases",
    fill = "Region"
  ) +
  scale_x_date(
    date_labels = "%b %d",     
    date_breaks = "1 week"     
  ) +
  theme_minimal(base_size = 25) +
  theme(
    axis.text.x = element_text(angle = 45, size = 13),
    panel.grid.major  = element_blank(),
    legend.position = "bottom")




