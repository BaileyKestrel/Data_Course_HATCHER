library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)

chickadee_raw <- read_csv("../../MAPS_data_download/MAPS_BANDING_capture_data.csv")

chickadee_raw %>% select(-SPN, -NUMB, -TIME, -NET, -ANET, -PPC, -SSC, -PPF, -SSF,
                         -TT, -RR, -HD, -UNP, -BPL, -NF, -FP, -B, -A, -IP, -SP) %>% 
  View()


chickadee_raw %>% select(LOC, STA, STATION, C, BAND, SPEC, AGE, HA, WRP, SEX, HS,
                         SK, CP, BP, F, BM, FM, FW, JP, WNG, WEIGHT, STATUS, DATE,
                         DISP, N, BRSTAT) %>% View()

