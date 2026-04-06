library(tidyverse)
library(readxl)
library(cowplot)
library(ggpubr)

source("./01 - Functions/plot3.r")

dat <- read_excel("./02 - Input/data_2026.xlsx") %>%
  filter(consent == 1 & age == 1)
qualCode <- read_excel("./02 - Input/data_qualitative responses_2026.xlsx")
themeClass <- read_excel("./02 - Input/themeClassifications.xlsx")

out <- figure3(dat, qualCode, themeClass)

ggsave(
  plot = out,
  bg = "white",
  filename = "./03 - Figures/Rendered/fig3.png",
  width = 13,
  height = 8
)
