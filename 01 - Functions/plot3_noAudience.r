plotQ5_duelBars_noAudience <- function(dat, qualCode, themeClass) {
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
    # mutate(n = ifelse(type == "pre", -n, n)) %>%
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
      fill = type,
      col = type
    )) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("#e69b99", "#24492e"),
                      labels = c("Post-test", "Pre-test")) +
    scale_color_manual(values = c("#e69b99", "#24492e")) +
    scale_x_continuous(
      limits = c(0, 14),
      breaks = seq(0, 14, 2)
    ) +
    labs(x = "Number of mentions",
         y = element_blank()) +
    guides(fill = guide_legend(title = element_blank()), color = "none") +
    theme_bw(base_size = 18) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.91, 0.075),
      legend.background = element_rect(color = "black", linewidth = 0.5),
      text = element_text(face = "bold")
    )
  
  return(out)
}