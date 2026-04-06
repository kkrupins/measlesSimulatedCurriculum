plot1b <- function() {
  q2 <- dat %>%
    select(q2, type) %>%
    rename(answer = q2) %>%
    group_by(answer, type) %>%
    ungroup() %>% group_by(type) %>%
    group_by(type, answer) %>% count() %>%
    full_join(allOptions) %>%
    mutate(n = ifelse(is.na(n), 0, n))
  
  d <- q2 %>%
    ggplot(mapping = aes(
      x = factor(type, levels = c("post", "pre")),
      y = n,
      fill = factor(answer, levels = c("E", "D", "C", "B", "A"))
    )) +
    geom_col(col = "black") +
    labs(y = "Frequency") +
    scale_fill_manual(
      values = c(
        "A" = "#675478",
        "B" = "#9e6374",
        "C" = "#c67b6f",
        "D" = "#de9b71",
        "E" = "#efbc82"
      )
    ) +
    scale_alpha_manual(values = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_x_discrete(labels = c("Post-test", "Pre-test")) +
    scale_y_continuous(limits = c(0, 11), breaks = c(seq(1, 11, 2))) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 18) +
    coord_flip() +
    theme(legend.title = element_blank(), axis.title.y = element_blank())
  return(d)
}