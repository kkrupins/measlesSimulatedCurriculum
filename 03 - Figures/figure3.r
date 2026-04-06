library(tidyverse)
library(readxl)
library(cowplot)
library(ggpubr)

source("./Functions/plot3.r")

dat <- read_excel("data_2026.xlsx") %>%
  filter(consent == 1 & age == 1)
qualCode <- read_excel("data_qualitative responses_2026.xlsx")
themeClass <- read_excel("themeClassifications.xlsx")

out <- figure3(dat, qualCode, themeClass)

ggsave(
  plot = out,
  bg = "white",
  filename = "fig3.png",
  width = 13,
  height = 8
)
