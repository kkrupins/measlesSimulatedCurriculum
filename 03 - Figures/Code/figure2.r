library(tidyverse)
library(readxl)
library(cowplot)
library(ggpubr)

source("./01 - Functions/plot1a.r")
source("./01 - Functions/plot1b.r")
source("./01 - Functions/plot1c.r")
source("./01 - Functions/plot1d.r")

dat <- read_excel("./02 - Input/Survey Data/data_2026.xlsx") %>%
  filter(consent == 1 & age == 1)

allOptions <- tibble(answer = rep(c("A", "B", "C", "D", "E"), 2), type = c(rep("pre", 5), rep("post", 5)))

a <- plot1a()
b <- plot1b()
c <- plot1c()
d <- plot1d()

out <- plot_grid(plotlist = c(a, b, c, d), labels = "AUTO")

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/fig2.png",
  width = 13,
  height = 8
)

rm(list = ls())
