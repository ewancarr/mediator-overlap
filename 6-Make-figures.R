#  ┌─────────────────────────────────────────────────────────────────────────┐
#  │                                                                         │
#  │                FIGURE 1: Percent of overlapping loadings                │
#  │                                                                         │
#  └─────────────────────────────────────────────────────────────────────────┘

# Make plot for "Percent of overlapping loadings"

plot_data <- counts_by_type %>%
    extract(pair, c("mediator", "type", "outcome"), "(.*)_([SL])_(.*)") %>%
    filter(outcome == "CFQ") %>%
    mutate(mediator = relabel_mediators(mediator),
           Mediator = relabel_type(type),
           point_shape = if_else(type == "Long version", 0, 3))

# Theming
plot_theme <- list(scale_fill_brewer(type = "qual"),
                   theme(legend.position = "none",
                         axis.title.y = element_blank(),
                         strip.text = element_text(size = 12),
                         axis.text.y = element_text(size = 12),
                         axis.title.x = element_text(size = 12),
                         axis.text.x = element_text(size = 12)),
                   labs(y = "Percent"))

# Bar plot
option_1 <- ggplot(plot_data, aes(x = mediator, y = percent, fill = mediator)) +
    geom_col() +
    facet_wrap(~ Mediator) +
    coord_flip(ylim = 0:100) +
    plot_theme

# Dot plot
option_2 <- ggplot(plot_data, aes(x = mediator, y = percent,
                                  group = Mediator, color = Mediator)) +
    geom_point(aes(shape = Mediator), alpha = 0.5, size = 5) +
    coord_flip(ylim = 0:100) +
    plot_theme +
    theme(legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))

# Save

ggsave(option_1,
       filename = "option_1.png",
       width = 7,
       height = 4,
       dpi = 300,
       dev = "png")

ggsave(option_2,
       filename = "option_2.png",
       width = 7,
       height = 4,
       dpi = 300,
       dev = "png")
