library(tidyverse)
library(readxl)

source("./01 - Functions/plotWrittenResponse_lolipop.r")

dat <- read_excel("./02 - Input/Written Feedback/codedResponses_2026.xlsx") %>%
  select(id, primary, secondary, tertiary, quatrinary, quinary) %>%
  gather(x, theme, -c(id)) %>%
  select(id, theme) %>%
  na.omit()

out <- plotWrittenResponse_lolipop()

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/fig4.png",
  width = 13,
  height = 8
)

rm(list = ls())




