# ------------------------------------------------------------------------------
# Title: Figure 4
# Author: Katy Krupinsky
# Updated: 08/05/26
# Description: This file contains the code to create Figure 4 of Krupinsky et al.
# 2026 JMBE
# ------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

source("./01 - Functions/plotFig4.R")

dat <- read_excel("./02 - Input/Written Feedback/codedResponses_2026.xlsx") %>%
  select(id, primary, secondary, tertiary, quatrinary, quinary) %>%
  gather(x, theme, -c(id)) %>%
  select(id, theme) %>%
  na.omit()

out <- plot4(dat)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/png/fig4.png",
  width = 13,
  height = 8
)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/tiff/fig4.tiff",
  width = 13,
  height = 8
)

rm(list = ls())




