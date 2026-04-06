figure3 <- function(dat, qualCode, themeClass) {
  dat <- left_join(dat, qualCode) %>%
    select(type, id, primary, secondary, tertiary) %>%
    gather(x, theme, -c(type, id)) %>%
    select(-x) %>%
    na.omit() %>%
    group_by(type, theme) %>%
    count()
  
  allOptions <- dat %>%
    ungroup() %>%
    select(theme) %>%
    unique()
  
  allOptions <- bind_rows(allOptions, allOptions) %>%
    mutate(type = c(rep("pre", 7), rep("post", 7)))
  
  dat <- dat %>%
    full_join(allOptions) %>%
    mutate(n = ifelse(is.na(n), 0, n))
  
  dat2 <- dat %>%
    mutate(n = ifelse(type == "pre", -n, n)) %>%
    full_join(themeClass) %>%
    mutate(theme = factor(
      theme,
      levels = c(
        "Surveillance",
        "Communicate scientific findings",
        "Decrease misinformation",
        "Transparency",
        "Public education",
        "Avoid future outbreaks",
        "Longterm improvement"
      )
    ),
    audience = factor(audience, levels = c("professional", "both", "public")))
  
  out <- dat2 %>%
    ggplot(aes(
      y = theme,
      x = n,
      alpha = type,
      fill = audience
    )) +
    geom_col(col = "black") +
    geom_vline(xintercept = 0) +
    scale_color_manual(
      values = c("#d2848d", "#d0e2af", "#7bbcd5"),
      labels = c("Professional", "Both", "Public")
    ) +
    scale_fill_manual(
      values = c("#d2848d", "#d0e2af", "#7bbcd5"),
      labels = c("Professional", "Both", "Public")
    ) +
    scale_x_continuous(
      limits = c(-8, 8),
      breaks = seq(-8, 8, 2),
      labels = c(seq(8, 0, -2), seq(2, 8, 2))
    ) +
    scale_alpha_manual(values = c(0.4, 1)) +
    labs(x = "Number of mentions",
         y = element_blank(),
         subtitle = "                                Pre-test                                                   Post-test") +
    guides(
      alpha = "none",
      fill = guide_legend(title = "Audience"),
      color = guide_legend(title = "Audience")
    ) +
    theme_bw(base_size = 18) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.9, 0.12),
      legend.background = element_rect(color = "black", linewidth = 0.5)
    )
  
  return(out)
}