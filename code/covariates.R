# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add covariates
# Rachel Sippy
#
# 1: Prepare IMD data
# 2: Scale covariates
# 3: Merge covariates to fidelity data
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Create lookup table for LSOAs within CCGs
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

lfile <- list.files("data/", "Lower_Layer")

lsoas <- read.csv(paste0("data/",lfile), header = TRUE, stringsAsFactors = FALSE)

# add names of sites
lsoas$site <- NA
lsoas$site[grep("Bexley",lsoas$CCG16NM)] <- "Bexley"
lsoas$site[grep("Bradford",lsoas$CCG16NM)] <- "Bradford"
lsoas$site[grep("Airedale",lsoas$CCG16NM)] <- "Bradford"
lsoas$site[grep("Cambridge",lsoas$CCG16NM)] <- "CambPeter"
lsoas$site[grep("Camden",lsoas$CCG16NM)] <- "Camden"
lsoas$site[grep("Ipswich",lsoas$CCG16NM)] <- "EastSuff"
lsoas$site[grep("North Hertfordshire",lsoas$CCG16NM)] <- "Herts"
lsoas$site[grep("Herts Valley",lsoas$CCG16NM)] <- "Herts"
lsoas$site[grep("Lewisham",lsoas$CCG16NM)] <- "Lewisham"
lsoas$site[grep("Luton",lsoas$CCG16NM)] <- "Luton"
lsoas$site[grep("Manchester",lsoas$CCG16NM)] <- "MancSal"
lsoas$site[grep("Salford",lsoas$CCG16NM)] <- "MancSal"
lsoas$site[grep("Corby",lsoas$CCG16NM)] <- "NeneCorby"
lsoas$site[grep("Nene",lsoas$CCG16NM)] <- "NeneCorby"
lsoas$site[grep("Norfolk",lsoas$CCG16NM)] <- "Norfolk"
lsoas$site[grep("Norwich",lsoas$CCG16NM)] <- "Norfolk"
lsoas$site[grep("Yarmouth",lsoas$CCG16NM)] <- "Norfolk"
lsoas$site[grep("Portsmouth",lsoas$CCG16NM)] <- "Portsmouth"
lsoas$site[grep("Southampton",lsoas$CCG16NM)] <- "SouthHam"
lsoas$site[grep("Stockport",lsoas$CCG16NM)] <- "Stockport"
lsoas$site[grep("Stoke",lsoas$CCG16NM)] <- "Stoke"
lsoas$site[grep("Sunderland",lsoas$CCG16NM)] <- "Sunderland"
lsoas$site[grep("Tower Hamlets",lsoas$CCG16NM)] <- "TowerHam"
lsoas$site[grep("Waltham Forest",lsoas$CCG16NM)] <- "WalthamF"
lsoas$site[grep("Warrington",lsoas$CCG16NM)] <- "Warrington"
lsoas$site[grep("Worcestershire",lsoas$CCG16NM)] <- "Worcester"

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 IMD 2019
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

ifile <- list.files("data/", "Indices_of_Multi")

imd <- read.csv(paste0("data/",ifile), header = TRUE, stringsAsFactors = FALSE)

colnames(imd)[2] <- "LSOA11CD"

# merge ccg, site names, imd
imd_site <- merge(lsoas[,c(1,5,9)], imd, "LSOA11CD")

# apply population weights to IMDScore
imd_site$wtd_IMDScore <- imd_site$IMDScore * imd_site$TotPop

# site or CCG as area of interest 
imd_site$aoi <- imd_site$site
imd_site$aoi[is.na(imd_site$aoi)] <- imd_site$CCG16NM[is.na(imd_site$aoi)]

#sum scores and populations by site
site_level <- aggregate(imd_site[,c(62,69)], list(aoi = imd_site$aoi), sum)

# divide summed scores by summed population
site_level$IMDScore <- site_level$wtd_IMDScore/site_level$TotPop

# rank
site_level$rank <- rank(-site_level$IMDScore)

# subset to sites
snm <- unique(subset(lsoas$site, !is.na(lsoas$site)))
imd19 <- subset(site_level, site_level$aoi %in% snm)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 IMD 2015
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
ifile5 <- list.files("data/", "Deprivation.csv")

imd5 <- read.csv(paste0("data/",ifile5), header = TRUE, stringsAsFactors = FALSE)

colnames(imd5)[1] <- "LSOA11CD"

# LSOA population 2015
pfile <- list.files("data/", "denominators.csv")

pop5 <- read.csv(paste0("data/",pfile), header = TRUE, stringsAsFactors = FALSE)

colnames(pop5)[1] <- "LSOA11CD"

# merge pop and imd
imd_pop <- merge(pop5[,c(1,5)], imd5, "LSOA11CD")

imd_pop$Total.population..mid.2012..excluding.prisoners. <- gsub(",", "", imd_pop$Total.population..mid.2012..excluding.prisoners.)
imd_pop$TotPop <- as.numeric(imd_pop$Total.population..mid.2012..excluding.prisoners.)

# merge ccg, site names, imd
imd15 <- merge(lsoas[,c(1,5,9)], imd_pop[,c(1,6,22)], "LSOA11CD")

# apply population weights to IMDScore
imd15$wtd_IMDScore <- imd15$Index.of.Multiple.Deprivation..IMD..Score * imd15$TotPop

# site or CCG as area of interest 
imd15$aoi <- imd15$site
imd15$aoi[is.na(imd15$aoi)] <- imd15$CCG16NM[is.na(imd15$aoi)]

#sum scores and populations by site
imd_aoi <- aggregate(imd15[,5:6], list(aoi = imd15$aoi), sum)

# divide summed scores by summed population
imd_aoi$IMDScore <- imd_aoi$wtd_IMDScore/imd_aoi$TotPop

# rank
imd_aoi$rank <- rank(-imd_aoi$IMDScore)

# subset to sites
imd15 <- subset(imd_aoi, imd_aoi$aoi %in% snm)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Prepare covariate data
## Re-scale cyp, dep to /1000
## Re-scale funds to /100000
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

cov <- read.csv("data/covariates.csv", header = TRUE, stringsAsFactors = FALSE)

cov$pop15s <- cov$pop15/100
cov$funds16s <- cov$funds16/100000
cov$pop19s <- cov$pop19/100
cov$funds19s <- cov$funds19/100000
cov$area18s <- cov$area18/1000000 # sq km
cov$area15s <- cov$area15/1000000 # sq km

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Merge with IMD data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(imd15)[c(1,5)] <- c("site", "rank15")
cov15 <- merge(cov[,c(1,11,12,16,13:15,8:10)], imd15[,c(1,5)], "site")

colnames(imd19)[c(1,5)] <- c("site", "rank19")
cov1519 <- merge(cov15, imd19[,c(1,5)], "site")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3 New variables
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

cov1519$popd15 <- cov1519$pop15s/cov1519$area15s
cov1519$popd19 <- cov1519$pop19s/cov1519$area18s

cov1519$high <- 0
cov1519$high[cov1519$effectiveness >= 68] <- 1

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Restructure data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

covs <- cov1519[,c(1,8:10,15)]
covs2 <- rbind(covs, covs)
covs2$time <- c(rep("BL", 20), rep("FU", 20))
covs2$popd[covs2$time == "BL"] <- cov1519$popd15
covs2$popd[covs2$time == "FU"] <- cov1519$popd19
covs2$funds[covs2$time == "BL"] <- cov1519$funds16s
covs2$funds[covs2$time == "FU"] <- cov1519$funds19s
covs2$IMD[covs2$time == "BL"] <- cov1519$rank15
covs2$IMD[covs2$time == "FU"] <- cov1519$rank19

load("fidelity_by_site_fill_total.RData")
load("fidelity_by_site_fill_level.RData")

level_wide <- pivot_wider(fidelity_by_site_fill_level, 
                          id_cols = c(site, time), names_from = level,
                          values_from = total_fidelity)

fidelity_all <- merge(fidelity_by_site_fill_total, level_wide, by = c("site", "time"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Merge to fidelity data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

fidelity_covars <- merge(fidelity_all, covs2, by = c("site", "time"))

save(fidelity_covars, file = "fidelity_covars.RData")
