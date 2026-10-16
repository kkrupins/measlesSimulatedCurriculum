# ------------------------------------------------------------------------------
# Title: Figure 2 Plotting Functions
# Author: Katy Krupinsky
# Updated: 08/05/26
# Description: This code contains the functions to creates the plots seen within
# Figure 2 of Krupinsky et al. 2026 JMBE
# ------------------------------------------------------------------------------

plot2a <- function(dat) {
  q1 <- dat %>%
    select(type, q1) %>%
    rename(answer = q1) %>%
    group_by(answer, type) %>%
    count() %>%
    full_join(allOptions) %>%
    arrange(answer) %>%
    mutate(answer_val = ifelse(answer == "E", "Correct", "Incorrect")) %>%
    mutate(fill_new = paste(type, answer_val, sep = ", ")) %>% 
    mutate(type = factor(type, levels = c("pre", "post"), labels = c("Pre-test", "Post-test")))
  
  a <- q1 %>%
    ggplot(mapping = aes(
      x = answer,
      y = n,
      fill = answer_val)) +
    geom_col(position = position_dodge2(width = 0.5), col = "black") +
    facet_wrap(vars(type), nrow = 2) +
    labs(x = "Answer choice", y = "Frequency") +
    scale_y_continuous(limits = c(0, 11), breaks = seq(1, 11, 2)) +
    scale_fill_manual(
      values = c(
        "Correct" = "#CE4441FF",
        "Incorrect" = "#E3ABA7FF"
      )
    ) +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw(base_size = 18) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.14, 0.86),
      legend.spacing.y = unit(-0.3, "cm"),
      text = element_text(face = "bold")
    )
  return(a)
}
plot2b <- function(dat) {
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
        "A" = "#EFC86EFF",
        "B" = "#6F9969FF",
        "C" = "#AADCE0FF",
        "D" = "#808FE1FF",
        "E" = "#0A2E57FF")
    ) +
    scale_alpha_manual(values = c(0.2, 0.4, 0.6, 0.8, 1)) +
    scale_x_discrete(labels = c("Post-test", "Pre-test")) +
    scale_y_continuous(limits = c(0, 11), breaks = c(seq(1, 11, 2))) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 18) +
    coord_flip() +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      text = element_text(face = "bold")
    )
  return(d)
}
plot2c <- function(dat) {
  c <- dat %>%
    ggplot(aes(x = factor(type, levels = c("pre", "post")), y = q3)) +
    geom_boxplot(alpha = 0.2, fill = "grey") +
    scale_y_continuous(limits = c(1, 5.5)) +
    scale_x_discrete(labels = c("Pre-test", "Post-test")) +
    labs(y = "Confidence Level", x = "") +
    theme_bw(base_size = 18) +
    theme(
      legend.position = "none",
      legend.position.inside = c(0, 1),
      text = element_text(face = "bold")
    )
  return(c)
}
plot2d <- function(dat) {
  q4 <- dat %>%
    select(q4, type) %>%
    rename(answer = q4) %>%
    mutate(answer = ifelse(answer == "B", "Correct", "Incorrect")) %>%
    group_by(answer, type) %>%
    count() 
  
  d <- q4 %>%
    ggplot(mapping = aes(
      x = factor(type, levels = c("pre", "post")),
      fill = answer,
      y = n
    )) +
    geom_bar(stat = "identity", col = "black") +
    scale_fill_manual(
      values = c(
        "Correct" = "#CE4441FF",
        "Incorrect" = "#E3ABA7FF"
      )
    ) +
    scale_x_discrete(labels = c("Pre-test", "Post-test")) +
    scale_y_continuous(limits = c(0, 11), breaks = c(seq(1, 11, 2))) +
    labs(y = "Frequency", x = "") +
    guides(fill = guide_legend(title = NULL)) +
    theme_bw(base_size = 18) +
    theme(text = element_text(face = "bold"))
  
  return(d)
}