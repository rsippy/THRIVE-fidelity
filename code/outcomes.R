# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data preparation
# Rachel Sippy
#
# 1: Restructuring
# 2: Missing observations & summarization
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(tidyverse)
library(extrafont)
library(RColorBrewer)

# import dataset
fidelity <- read.csv("data/fidelity.csv", na.strings = ".") 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1 Creating new variables & restructuring data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.1 Averages per site
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## using dplyr pipeline to average the scores for each rater, creating an average
## score for each variable. 
Fidelity1 <- fidelity %>% 
  rowwise() %>% ## averaging the raters to create an average rating per principle per site. 
  mutate(s11_BL_iTHR = mean(c(Anna_11_BL,
                                 Ollie_11_BL), na.rm = TRUE)) %>%
  mutate(s11_FU_iTHR = mean(c(Kate_11_FU,
                                 Ollie_Anna_11_FU), na.rm = TRUE)) %>%
  mutate(s12_BL_iTHR = mean(c(Lida_12_BL,
                                    Niran_12_BL))) %>%
  mutate(s12_FU_iTHR = mean(c(Lida_12_FU,
                                    Niran_12_FU))) %>%
  mutate(s13_BL_iTHR = mean(c(Anna_13_BL, Lida_13_BL, 
                                 Niran_13_BL, Ollie_13_BL))) %>%
  mutate(s13_FU_iTHR = mean(c(Lida_13_FU, 
                                 Niran_13_FU))) %>%
  mutate(s14_BL_iTHR = mean(c(Anna_14_BL, Niran_14_BL,
                                Lida_14_BL, Ollie_14_BL))) %>%
  mutate(s14_FU_iTHR = mean(c(Anna_14_FU,
                                Ollie_14_FU))) %>%
  mutate(s15_BL_iTHR = mean(c(Lida_15_BL, 
                                Niran_15_BL))) %>%
  mutate(s15_FU_iTHR = mean(c(Lida_15_FU, 
                                Niran_15_FU))) %>%
  mutate(s16_BL_iTHR = mean(c(Anna_16_BL, 
                                  Ollie_16_BL))) %>%
  mutate(s16_FU_iTHR = mean(c(Anna_16_FU,
                                  Ollie_16_FU))) %>%
  mutate(s17_BL_iTHR = mean(c(Anna_17_BL, 
                                    Ollie_17_BL))) %>%
  mutate(s17_FU_iTHR = mean(c(Anna_Kate_17_FU, 
                                    Ollie_17_FU))) %>%
  mutate(s18_BL_iTHR = mean(c(Lida_18_BL, 
                                   Niran_18_BL))) %>%
  mutate(s18_FU_iTHR = mean(c(Ollie_18_FU, 
                                   Niran_18_FU))) %>%
  mutate(s19_BL_iTHR = mean(c(Anna_19_BL, 
                                   Ollie_19_BL))) %>%
  mutate(s19_FU_iTHR = mean(c(Ollie_19_FU, 
                                   Kate_19_FU))) %>%
  mutate(s20_BL_iTHR = mean(c(Ollie_20_BL, 
                                     Niran_20_BL))) %>%
  mutate(s20_FU_iTHR = mean(c(Ollie_20_FU, 
                                     Niran_20_FU))) %>%
  mutate(s1_BL_Ctrl = mean(c(Anna_1_BL, Niran_1_BL,
                                   Lida_1_BL, Ollie_1_BL))) %>%
  mutate(s1_FU_Ctrl = mean(c(Ollie_1_FU,
                                   Niran_1_FU))) %>%
  mutate(s2_BL_Ctrl = mean(c(Niran_2_BL, 
                                   Lida_2_BL))) %>%
  mutate(s2_FU_Ctrl = mean(c(Lida_2_FU, 
                                   Niran_2_FU))) %>%
  mutate(s3_BL_Ctrl = mean(c(Niran_3_BL,
                                   Lida_3_BL))) %>%
  mutate(s3_FU_Ctrl = mean(c(Niran_3_FU,
                                   Lida_3_FU))) %>%
  mutate(s4_BL_Ctrl = mean(c(Anna_4_BL,
                                    Ollie_4_BL))) %>%
  mutate(s4_FU_Ctrl = mean(c(Anna_Kate_4_FU,
                                    Ollie_4_FU))) %>%
  mutate(s5_BL_Ctrl = mean(c(Lida_5_BL,
                                  Niran_5_BL))) %>%
  mutate(s5_FU_Ctrl = mean(c(Ollie_5_FU,
                                  Niran_5_FU), na.rm = TRUE)) %>%
  mutate(s6_BL_Ctrl = mean(c(Anna_6_BL, Lida_6_BL,
                                     Niran_6_BL, Ollie_6_BL))) %>%
  mutate(s6_FU_Ctrl = mean(c(Kate_6_FU, 
                                     Ollie_6_FU))) %>%
  mutate(s7_BL_Ctrl = mean(c(Anna_7_BL,
                                   Ollie_7_BL))) %>%
  mutate(s7_FU_Ctrl = mean(c(Ollie_7_FU, 
                                   Niran_7_FU))) %>%
  mutate(s8_BL_Ctrl = mean(c(Lida_8_BL,
                                Niran_8_BL), na.rm = TRUE)) %>%
  mutate(s8_FU_Ctrl = mean(c(Ollie_8_FU,
                                Niran_8_FU))) %>%
  mutate(s9_BL_Ctrl = mean(c(Anna_9_BL,
                                     Ollie_9_BL))) %>%
  mutate(s9_FU_Ctrl = mean(c(Ollie_9_FU,
                                     Niran_9_FU))) %>%
  mutate(s10_BL_Ctrl = mean(c(Anna_10_BL,
                                    Ollie_10_BL))) %>%
  mutate(s10_FU_Ctrl = mean(c(Ollie_10_FU,
                                    Niran_10_FU))) %>%
  dplyr::select(iTHRIVE_principle, Level, s11_BL_iTHR, s11_FU_iTHR, s12_BL_iTHR, 
                s12_FU_iTHR, s13_BL_iTHR, s13_FU_iTHR, s14_BL_iTHR, 
                s14_FU_iTHR, s15_BL_iTHR, s15_FU_iTHR,s16_BL_iTHR,
                s16_FU_iTHR, s17_BL_iTHR, s17_FU_iTHR, s18_BL_iTHR,
                s18_FU_iTHR, s19_BL_iTHR, s19_FU_iTHR, s20_BL_iTHR,
                s20_FU_iTHR, s1_BL_Ctrl, s1_FU_Ctrl, 
                s2_BL_Ctrl, s2_FU_Ctrl,
                s3_BL_Ctrl, s3_FU_Ctrl, s4_BL_Ctrl, 
                s4_FU_Ctrl, s5_BL_Ctrl, 
                s5_FU_Ctrl, s6_BL_Ctrl, s6_FU_Ctrl, 
                s7_BL_Ctrl, s7_FU_Ctrl,
                s8_BL_Ctrl, s8_FU_Ctrl, s9_BL_Ctrl, s9_FU_Ctrl, 
                s10_BL_Ctrl, s10_FU_Ctrl)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.2 Sites with missing scores
## site11 FU, site8 BL NAs
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

print(Fidelity1[,c("Level", "s11_FU_iTHR", "s8_BL_Ctrl")], n = 100, na.print = "NA")

## site11 FU all level 3, site8 BL all level 2

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.3 Wide to long
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

fidelity_by_site <- (
  Fidelity1 %>%
    mutate(
      level = case_when(
        Level == 1 ~ "micro",
        Level == 2 ~ "meso",
        Level == 3 ~ "macro"
      )
    ) %>%
    pivot_longer(
      cols = 3:ncol(Fidelity1),
      names_to = c("site", "time", "intervention"),
      names_pattern = "(.*)_(.*)_(.*)",
      values_to = "fidelity"
    ) %>%
    dplyr::select(
      -Level
    )
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.4 Groupings
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

table(fidelity_by_site$site[fidelity_by_site$time == "BL"], fidelity_by_site$level[fidelity_by_site$time == "BL"])
## macro: 21
## meso: 26
## micro: 28

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.1 Summed scores
## Some sites are missing scores for subsections/times
## These sites have deflated total scores
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

fidelity_by_site_total <- (
  fidelity_by_site %>%
    group_by(site, time, intervention) %>%
    summarize(total_fidelity = sum(fidelity, na.rm = TRUE), .groups = "drop")
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2 Corrected summed scores
## Some sites are missing scores for subsections/times
## Fill in missing with mean within level or 
## mean after taking mean difference from time pair
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

site11 <- Fidelity1[, c("Level", "s11_BL_iTHR", "s11_FU_iTHR")]
site11$diff <- site11$s11_FU_iTHR - site11$s11_BL_iTHR

mean(site11$diff, na.rm = TRUE)
# mean difference between FU and BL = 0.157

site11FU <- site11$s11_BL_iTHR + 0.157

site8 <- Fidelity1[, c("Level", "s8_BL_Ctrl", "s8_FU_Ctrl")]
site8$diff <- site8$s8_FU_Ctrl - site8$s8_BL_Ctrl

mean(site8$diff, na.rm = TRUE)
# mean difference between FU and BL = 0.112

site8BL <- site8$s8_FU_Ctrl - 0.112

# replace missing
fidelity_by_site_fill <- fidelity_by_site
fidelity_by_site_fill$fidelity[fidelity_by_site_fill$site == "11" & 
                                 fidelity_by_site_fill$time == "FU"] <- site11FU
fidelity_by_site_fill$fidelity[fidelity_by_site_fill$site == "8" & 
                                 fidelity_by_site_fill$time == "BL"] <- site8BL

# corrected summed scores
fidelity_by_site_fill_total <- (
  fidelity_by_site_fill %>%
    group_by(site, time, intervention) %>%
    summarize(total_fidelity = sum(fidelity, na.rm = TRUE), .groups = "drop")
)

save(fidelity_by_site_fill_total, file = "fidelity_by_site_fill_total.RData")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.3 Corrected summed scores by level
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

fidelity_by_site_fill_level <- (
  fidelity_by_site_fill %>%
    group_by(level, site, time, intervention) %>%
    summarize(total_fidelity = sum(fidelity, na.rm = TRUE), .groups = "drop")
)

save(fidelity_by_site_fill_level, file = "fidelity_by_site_fill_level.RData")
