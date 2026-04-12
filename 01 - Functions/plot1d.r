plot1d <- function() {
  q4 <- dat %>%
    select(q4, type) %>%
    rename(answer = q4) %>%
    mutate(answer = ifelse(answer == "B", "Correct", "Incorrect")) %>%
    group_by(answer, type) %>%
    count() %>%
    mutate(fill_new = paste(type, answer, sep = ", "))
  
  d <- q4 %>%
    ggplot(mapping = aes(
      x = factor(type, levels = c("pre", "post")),
      fill = factor(
        fill_new,
        levels = c(
          "pre, Correct",
          "pre, Incorrect",
          "post, Correct",
          "post, Incorrect"
        )
      ),
      y = n
    )) +
    geom_bar(stat = "identity", col = "black") +
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
    scale_alpha_manual(values = c(1, 0.2),
                       labels = c("Correct", "Incorrect")) +
    guides(fill = guide_legend(title = NULL), alpha = guide_legend(title = NULL)) +
    coord_polar("y", start = 0) +
    theme_void(base_size = 18) +
    theme(text = element_text(face = "bold"))
  
  return(d)
}