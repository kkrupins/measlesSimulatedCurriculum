library(tidyverse)
library(readxl)
library(cowplot)
library(ggpubr)

source("./01 - Functions/plot3.r")

qualCode <- read_excel("./02 - Input/Survey Data/data_qualitative responses_2026.xlsx", col_names = TRUE)

out <- plotFig3(qualCode)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/fig3.png",
  width = 13,
  height = 8
)

rm(list = ls())
