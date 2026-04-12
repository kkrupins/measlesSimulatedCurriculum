plotWrittenResponse_lolipop <- function() {
  dat2 <- dat %>%
    group_by(theme) %>%
    count() %>%
    arrange(-n) %>%
    mutate(theme = factor(
      theme,
      levels = c(
        "Worksheet inclusion",
        "Lecturer knowledge & enthusiasm",
        "Taking on the role of a professional",
        "Interactive componenets",
        "Breakout rooms/share-out time",
        "Real-world problem solving",
        "Lecture format/organization"
      )
    )) %>%
    mutate(percent = (n / 35) * 100)
  
  out <- dat2 %>%
    ggplot() +
    geom_segment(aes(
      y = theme,
      yend = theme,
      x = 0,
      xend = n
    ),
    linewidth = 1) +
    geom_point(aes(x = n, y = theme), size = 5, col = "#89589d") +
    scale_x_continuous(limits = c(0, 8)) +
    labs(x = "Number of mentions", y = element_blank()) +
    theme_bw(base_size = 18) +
    theme(text = element_text(face = "bold"))
  
  return(out)
}