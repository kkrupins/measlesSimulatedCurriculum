# ------------------------------------------------------------------------------
# Title: Figure 4 Plotting Functions
# Author: Katy Krupinsky
# Updated: 08/05/26
# Description: This code contains the functions to creates the plots seen within
# Figure 4 of Krupinsky et al. 2026 JMBE
# ------------------------------------------------------------------------------
plot4 <- function(dat) {
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
        "Interactive components",
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