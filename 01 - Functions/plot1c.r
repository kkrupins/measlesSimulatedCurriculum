plot1c <- function() {
  c <- dat %>%
    ggplot(aes(
      x = factor(type, levels = c("pre", "post")),
      y = q3
    )) +
    geom_boxplot(alpha = 0.2, fill = "grey") +
    stat_compare_means(comparisons = list(c("pre", "post")),
                       label = "p.signif", size = 8, vjust = 0.4, bracket.size = 1) +
    scale_y_continuous(limits = c(1, 5.5)) +
    scale_x_discrete(labels = c("Pre-test", "Post-test")) +
    labs(y = "Confidence Level", x = "") +
    theme_bw(base_size = 18) +
    theme(legend.position = "none",
          legend.position.inside = c(0, 1),
          text = element_text(face = "bold"))
  return(c)
}