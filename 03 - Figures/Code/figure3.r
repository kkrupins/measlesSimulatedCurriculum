# ------------------------------------------------------------------------------
# Title: Figure 3
# Author: Katy Krupinsky
# Updated: 08/05/26
# Description: This file contains the code to create Figure 3 of Krupinsky et al.
# 2026 JMBE
# ------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(cowplot)
library(ggpubr)

source("./01 - Functions/plotFig3.r")

qualCode <- read_excel("./02 - Input/Survey Data/data_qualitative responses_2026.xlsx", col_names = TRUE)

out <- plot3(qualCode)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/fig3.png",
  width = 13,
  height = 8
)

rm(list = ls())
