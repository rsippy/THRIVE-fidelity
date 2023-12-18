# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data preparation
# Rachel Sippy
#
# 1: Restructuring
# 2: Missing observations & summarization
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(readxl)
library(tidyverse)
library(extrafont)
library(RColorBrewer)

# import dataset
fidelity <- read_excel("data/Recoded_fidelity_2022_v2.xlsx") 

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
  mutate(Bexley_BL_iTHR = mean(c(Anna_Bexley_BL,
                                 Ollie_Bexley_BL), na.rm = TRUE)) %>%
  mutate(Bexley_FU_iTHR = mean(c(Kate_Bexley_FU,
                                 Ollie_Anna_Bexley_FU), na.rm = TRUE)) %>%
  mutate(CambPeter_BL_iTHR = mean(c(Lida_CambPet_BL,
                                    Niran_CambPet_BL))) %>%
  mutate(CambPeter_FU_iTHR = mean(c(Lida_CambPet_FU,
                                    Niran_CambPet_FU))) %>%
  mutate(Camden_BL_iTHR = mean(c(Anna_Camden_BL, Lida_Camden_BL, 
                                 Niran_Camden_BL, Ollie_Camden_BL))) %>%
  mutate(Camden_FU_iTHR = mean(c(Lida_Camden_FU, 
                                 Niran_Camden_FU))) %>%
  mutate(Herts_BL_iTHR = mean(c(Anna_Herts_BL, Niran_Herts_BL,
                                Lida_Herts_BL, Ollie_Herts_BL))) %>%
  mutate(Herts_FU_iTHR = mean(c(Anna_Herts_FU,
                                Ollie_Herts_FU))) %>%
  mutate(Luton_BL_iTHR = mean(c(Lida_Luton_BL, 
                                Niran_Luton_BL))) %>%
  mutate(Luton_FU_iTHR = mean(c(Lida_Luton_FU, 
                                Niran_Luton_FU))) %>%
  mutate(MancSal_BL_iTHR = mean(c(Anna_MancSal_BL, 
                                  Ollie_MancSal_BL))) %>%
  mutate(MancSal_FU_iTHR = mean(c(Anna_MancSal_FU,
                                  Ollie_MancSal_FU))) %>%
  mutate(Stockport_BL_iTHR = mean(c(Anna_Stockport_BL, 
                                    Ollie_Stockport_BL))) %>%
  mutate(Stockport_FU_iTHR = mean(c(Anna_Kate_Stockport_FU, 
                                    Ollie_Stockport_FU))) %>%
  mutate(TowerHam_BL_iTHR = mean(c(Lida_TowerHamlets_BL, 
                                   Niran_TowerHamlets_BL))) %>%
  mutate(TowerHam_FU_iTHR = mean(c(Ollie_TowerHamlets_FU, 
                                   Niran_TowerHamlets_FU))) %>%
  mutate(WalthamF_BL_iTHR = mean(c(Anna_Waltham_BL, 
                                   Ollie_Waltham_BL))) %>%
  mutate(WalthamF_FU_iTHR = mean(c(Ollie_Waltham_FU, 
                                   Kate_Waltham_FU))) %>%
  mutate(Warrington_BL_iTHR = mean(c(Ollie_Warrington_BL, 
                                     Niran_Warrington_BL))) %>%
  mutate(Warrington_FU_iTHR = mean(c(Ollie_Warrington_FU, 
                                     Niran_Warrington_FU))) %>%
  mutate(Bradford_BL_Ctrl = mean(c(Anna_Bradford_BL, Niran_Bradford_BL,
                                   Lida_Bradford_BL, Ollie_Bradford_BL))) %>%
  mutate(Bradford_FU_Ctrl = mean(c(Ollie_Bradford_FU,
                                   Niran_Bradford_FU))) %>%
  mutate(EastSuff_BL_Ctrl = mean(c(Niran_EastSuffolk_BL, 
                                   Lida_EastSuffolk_BL))) %>%
  mutate(EastSuff_FU_Ctrl = mean(c(Lida_EastSuffolk_FU, 
                                   Niran_EastSuffolk_FU))) %>%
  mutate(Lewisham_BL_Ctrl = mean(c(Niran_Lewisham_BL,
                                   Lida_Lewisham_BL))) %>%
  mutate(Lewisham_FU_Ctrl = mean(c(Niran_Lewisham_FU,
                                   Lida_Lewisham_FU))) %>%
  mutate(NeneCorby_BL_Ctrl = mean(c(Anna_NeneCorby_BL,
                                    Ollie_NeneCorby_BL))) %>%
  mutate(NeneCorby_FU_Ctrl = mean(c(Anna_Kate_NeneCorby_FU,
                                    Ollie_NeneCorby_FU))) %>%
  mutate(Norfolk_BL_Ctrl = mean(c(Lida_Norfolk_BL,
                                  Niran_Norfolk_BL))) %>%
  mutate(Norfolk_FU_Ctrl = mean(c(Ollie_Norfolk_FU,
                                  Niran_Norfolk_FU), na.rm = TRUE)) %>%
  mutate(Portsmouth_BL_Ctrl = mean(c(Anna_Portsmouth_BL, Lida_Portsmouth_BL,
                                     Niran_Portsmouth_BL, Ollie_Portsmouth_BL))) %>%
  mutate(Portsmouth_FU_Ctrl = mean(c(Kate_Portsmouth_FU, 
                                     Ollie_Portsmouth_FU))) %>%
  mutate(SouthHam_BL_Ctrl = mean(c(Anna_Southampton_BL,
                                   Ollie_Southampton_BL))) %>%
  mutate(SouthHam_FU_Ctrl = mean(c(Ollie_Southampton_FU, 
                                   Niran_Southampton_FU))) %>%
  mutate(Stoke_BL_Ctrl = mean(c(Lida_Stoke_BL,
                                Niran_Stoke_BL), na.rm = TRUE)) %>%
  mutate(Stoke_FU_Ctrl = mean(c(Ollie_Stoke_FU,
                                Niran_Stoke_FU))) %>%
  mutate(Sunderland_BL_Ctrl = mean(c(Anna_Sunderland_BL,
                                     Ollie_Sunderland_BL))) %>%
  mutate(Sunderland_FU_Ctrl = mean(c(Ollie_Sunderland_FU,
                                     Niran_Sunderland_FU))) %>%
  mutate(Worcester_BL_Ctrl = mean(c(Anna_Worcester_BL,
                                    Ollie_Worcester_BL))) %>%
  mutate(Worcester_FU_Ctrl = mean(c(Ollie_Worcester_FU,
                                    Niran_Worcester_FU))) %>%
  dplyr::select(iTHRIVE_principle, Level, Bexley_BL_iTHR, Bexley_FU_iTHR, CambPeter_BL_iTHR, 
                CambPeter_FU_iTHR, Camden_BL_iTHR, Camden_FU_iTHR, Herts_BL_iTHR, 
                Herts_FU_iTHR, Luton_BL_iTHR, Luton_FU_iTHR,MancSal_BL_iTHR,
                MancSal_FU_iTHR, Stockport_BL_iTHR, Stockport_FU_iTHR, TowerHam_BL_iTHR,
                TowerHam_FU_iTHR, WalthamF_BL_iTHR, WalthamF_FU_iTHR, Warrington_BL_iTHR,
                Warrington_FU_iTHR, Bradford_BL_Ctrl, Bradford_FU_Ctrl, 
                EastSuff_BL_Ctrl, EastSuff_FU_Ctrl,
                Lewisham_BL_Ctrl, Lewisham_FU_Ctrl, NeneCorby_BL_Ctrl, 
                NeneCorby_FU_Ctrl, Norfolk_BL_Ctrl, 
                Norfolk_FU_Ctrl, Portsmouth_BL_Ctrl, Portsmouth_FU_Ctrl, 
                SouthHam_BL_Ctrl, SouthHam_FU_Ctrl,
                Stoke_BL_Ctrl, Stoke_FU_Ctrl, Sunderland_BL_Ctrl, Sunderland_FU_Ctrl, 
                Worcester_BL_Ctrl, Worcester_FU_Ctrl)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.2 Sites with missing scores
## Bexley FU, Stoke BL NAs
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

print(Fidelity1[,c("Level", "Bexley_FU_iTHR", "Norfolk_FU_Ctrl", "Stoke_BL_Ctrl")], n = 100, na.print = "NA")

## Bexley FU all level 3, Stoke BL all level 2

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

bexley <- Fidelity1[, c("Level", "Bexley_BL_iTHR", "Bexley_FU_iTHR")]
bexley$diff <- bexley$Bexley_FU_iTHR - bexley$Bexley_BL_iTHR

mean(bexley$diff, na.rm = TRUE)
# mean difference between FU and BL = 0.157

bexleyFU <- bexley$Bexley_BL_iTHR + 0.157

stoke <- Fidelity1[, c("Level", "Stoke_BL_Ctrl", "Stoke_FU_Ctrl")]
stoke$diff <- stoke$Stoke_FU_Ctrl - stoke$Stoke_BL_Ctrl

mean(stoke$diff, na.rm = TRUE)
# mean difference between FU and BL = 0.112

stokeBL <- stoke$Stoke_FU_Ctrl - 0.112

# replace missing
fidelity_by_site_fill <- fidelity_by_site
fidelity_by_site_fill$fidelity[fidelity_by_site_fill$site == "Bexley" & 
                                 fidelity_by_site_fill$time == "FU"] <- bexleyFU
fidelity_by_site_fill$fidelity[fidelity_by_site_fill$site == "Stoke" & 
                                 fidelity_by_site_fill$time == "BL"] <- stokeBL

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
