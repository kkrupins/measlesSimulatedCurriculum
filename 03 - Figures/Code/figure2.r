library(tidyverse)
library(readxl)
# ------------------------------------------------------------------------------
# Title: Figure 2
# Author: Katy Krupinsky
# Updated: 08/05/26
# Description: This file contains the code to create Figure 2 of Krupinsky et al.
# 2026 JMBE
# ------------------------------------------------------------------------------

library(cowplot)
library(ggpubr)

source("./01 - Functions/plotFig2.r")

dat <- read_excel("./02 - Input/Survey Data/data_2026.xlsx") %>%
  filter(consent == 1 & age == 1)

allOptions <- tibble(answer = rep(c("A", "B", "C", "D", "E"), 2), type = c(rep("pre", 5), rep("post", 5)))

a <- plot2a(dat)
b <- plot2b(dat)
c <- plot2c(dat)
d <- plot2d(dat)

out <- plot_grid(plotlist = c(a, b, c, d), labels = "AUTO")

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/png/fig2.png",
  width = 13,
  height = 8
)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/tiff/fig2.tiff",
  width = 13,
  height = 8
)

rm(list = ls())
