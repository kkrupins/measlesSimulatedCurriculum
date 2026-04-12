plot1a <- function() {
  q1 <- dat %>%
    select(type, q1) %>%
    rename(answer = q1) %>%
    group_by(answer, type) %>%
    count() %>%
    full_join(allOptions) %>%
    arrange(answer) %>%
    mutate(answer_val = ifelse(answer == "E", "Correct", "Incorrect")) %>%
    mutate(fill_new = paste(type, answer_val, sep = ", "))
  
  a <- q1 %>%
    ggplot(mapping = aes(
      x = answer,
      y = n,
      fill = factor(
        fill_new,
        levels = c(
          "pre, Correct",
          "pre, Incorrect",
          "post, Correct",
          "post, Incorrect"
        )
      )
    )) +
    geom_col(position = position_dodge2(width = 0.5), col = "black") +
    labs(x = "Answer choice", y = "Frequency") +
    scale_y_continuous(limits = c(0, 11), breaks = seq(1, 11, 2)) +
    scale_fill_manual(
      values = c(
        "pre, Correct" = "#24492e",
        "post, Correct" = "#e69b99",
        "pre, Incorrect" = "#24492e60",
        "post, Incorrect" = "#e69b9960"
      ),
      labels = c(
        "Pre-test, Correct",
        "Pre-test, Incorrect",
        "Post-test, Correct",
        "Post-test, Incorrect"
      )
    ) +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw(base_size = 18) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.23, 0.78),
      legend.spacing.y = unit(-0.3, "cm"),
      text = element_text(face = "bold")
    )
  return(a)
}