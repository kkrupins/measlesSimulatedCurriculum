library(tidyverse)
library(readxl)
library(cowplot)
library(ggpubr)

source("./01 - Functions/plot3.r")
source("./01 - Functions/plot3_noAudience.r")

dat <- read_excel("./02 - Input/Survey Data/data_2026.xlsx") %>%
  filter(consent == 1 & age == 1)
qualCode <- read_excel("./02 - Input/Survey Data/data_qualitative responses_2026.xlsx")
themeClass <- read_excel("./02 - Input/Survey Data/themeClassifications.xlsx")

out <- plotQ5_duelBars_noAudience(dat, qualCode, themeClass)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/fig3_Audience_dodge.png",
  width = 13,
  height = 8
)

rm(list = ls())
