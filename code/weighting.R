# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Propensity score weights
# Rachel Sippy
#
# 1: Prepare data
# 2: Standardized means
# 3: Generate model & calculate weights
# 4: Check balance
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tableone)
library(nnet)
library(ggplot2)
library(reshape2)
# library(splines)
# library(rms)

load("fidelity_covars.RData")

# ++++++++++++++++++++++++++++++++++
# 1.1 Add four groups
# ++++++++++++++++++++++++++++++++++

fidelity_covars$group <- "G1"
fidelity_covars$group[fidelity_covars$intervention == "iTHR" & fidelity_covars$time == "FU"] <- "G2"
fidelity_covars$group[fidelity_covars$intervention == "Ctrl" & fidelity_covars$time == "BL"] <- "G3"
fidelity_covars$group[fidelity_covars$intervention == "Ctrl" & fidelity_covars$time == "FU"] <- "G4"

# ++++++++++++++++++++++++++++++++++
# 1.2 Standardized means table
# ++++++++++++++++++++++++++++++++++

vars <- c("popd", "funds", "IMD", "nccgs", "compliance")

dat12 <- subset(fidelity_covars, fidelity_covars$group == "G1" | fidelity_covars$group == "G2")
dat13 <- subset(fidelity_covars, fidelity_covars$group == "G1" | fidelity_covars$group == "G3")
dat14 <- subset(fidelity_covars, fidelity_covars$group == "G1" | fidelity_covars$group == "G4")
dat12$group <- factor(dat12$group, levels = c("G1", "G2"))
dat13$group <- factor(dat13$group, levels = c("G1", "G3"))
dat14$group <- factor(dat14$group, levels = c("G1", "G4"))

# Construct tables
tabUn12 <- CreateTableOne(vars = vars, strata = "group", data = dat12,
                          test = FALSE)
tabUn13 <- CreateTableOne(vars = vars, strata = "group", data = dat13,
                          test = FALSE)
tabUn14 <- CreateTableOne(vars = vars, strata = "group", data = dat14,
                          test = FALSE)
# Show table with SMD
print(tabUn12, smd = TRUE)
print(tabUn13, smd = TRUE)
print(tabUn14, smd = TRUE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.1 Generate model
# Linear version
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

ml2 <- multinom(group ~ popd + funds + IMD + nccgs + compliance, data = fidelity_covars) # best

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2 Generate flexible models
# Generate percentiles for k=3
# Versions of flexible models
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# percentile values for knot placement
# funk <- unname(quantile(fidelity_covars$funds, probs = c(0.1,0.5,0.9)))
# imdk <- unname(quantile(fidelity_covars$IMD, probs = c(0.1,0.5,0.9)))
# pdk <- unname(quantile(fidelity_covars$popd, probs = c(0.1,0.5,0.9)))

# per Harrell, n < 100 so replace first and last knot locations with
# 5th largest and 5th smallest values
# funk[c(1,3)] <- c(sort(fidelity_covars$funds, partial = 5)[5], sort(fidelity_covars$funds, partial = 35)[35])
# imdk[c(1,3)] <- c(sort(fidelity_covars$IMD, partial = 5)[5], sort(fidelity_covars$IMD, partial = 35)[35])
# pdk[c(1,3)] <- c(sort(fidelity_covars$popd, partial = 5)[5], sort(fidelity_covars$popd, partial = 35)[35])

# results in 0, 1 probability predictions
# fml <- multinom(group ~ rcs(funds, knots = funk) + rcs(IMD, knots = imdk) + 
#                  rcs(popd, knots = pdk) + rcs(cyp, knots = cypk), data = dat)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3 Generate predictions
## Predicted probability of being in each group
## Conditional on covariates
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

pp2 <- fitted(ml2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Add to data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

wts2 <- cbind(fidelity_covars, pp2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Examine area of support
## all obs must have 0<Pr(g|X)<1
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary(wts2$G1)
summary(wts2$G2)
summary(wts2$G3)
summary(wts2$G4)

ggplot(wts2, aes(x = G1, fill = group)) +
  geom_histogram( color="#bdbdbd", alpha=0.6, position = 'identity') +
  theme_classic() +
  labs(fill="Group", title = "Distribution of Probability Scores") +
  ylab("Frequency") +
  xlab("Probability") +
  facet_wrap(~group, nrow = 4)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 Calculate G1 propensity scores 
## w = e1(X)/eg(X) where e(X) is Pr(g|X)
## and g is true group for obs
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

wts2$g1w <- 1
wts2$g1w[wts2$group == "G2"] <- wts2$G1[wts2$group == "G2"]/wts2$G2[wts2$group == "G2"]  
wts2$g1w[wts2$group == "G3"] <- wts2$G1[wts2$group == "G3"]/wts2$G3[wts2$group == "G3"]  
wts2$g1w[wts2$group == "G4"] <- wts2$G1[wts2$group == "G4"]/wts2$G4[wts2$group == "G4"]  

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.4 Examine weights
## per Dr Stuart: only eliminate extreme weight outliers
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

ggplot(wts2, aes(x = g1w, fill = group)) +
  geom_histogram( color="#bdbdbd", alpha=0.6, position = 'identity') +
  theme_classic() +
  labs(fill="Group", title = "Distribution of Probability Scores") +
  ylab("Frequency") +
  xlab("Probability") +
  facet_wrap(~group, nrow = 4)

write.csv(wts2, file = "weights.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1 Assess covariate balance
## From Austin & Stuart 2015
## Calculate weighted value
## Calculate weighted mean by group
## Calculate weighted variance by group
## Calculate weighted difference in standardized means
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# weighted values (value * weight)
wc <- data.frame(group = wts2$group, apply(wts2[,c(8,9,12:14)], 2, function(v){
  mapply(function(x,y){
    w <- x*y
    w
  }, v, wts2$g1w)
}))

####################### weighted group means

## sum of weighted values/sum of weights
### sum of weighted means
wms <- aggregate(wc[,-1], list(group = wc$group), sum)

### sum of weights 
ws <- aggregate(wts2$g1w, list(group = wts2$group), sum)

## weighted group means
wm <- data.frame(group = c("G1", "G2", "G3", "G4"), apply(wms[,2:6], 2, function(x){
  x/ws$x
})) 

# per Stuart: use unweighted standard errors
## unweighted group variance
wsq <- data.frame(group = c("G1", "G2", "G3", "G4"), apply(wts2[,c(8,9,12:14)], 2, function(x){
  z <- aggregate(x, list(group = wts2$group), var)
  z[,-1]
})) 

# G2 v G1: post iTHR v pre iTHR
wg2g1 <- (wm[2,2:6] - wm[1,2:6])/sqrt((wsq[2,2:6] + wsq[1,2:6])/2)

# G3 v G1: pre Ctrl v pre iTHR
wg3g1 <- (wm[3,2:6] - wm[1,2:6])/sqrt((wsq[3,2:6] + wsq[1,2:6])/2)

# G4 v G1: post Ctrl v pre iTHR
wg4g1 <- (wm[4,2:6] - wm[1,2:6])/sqrt((wsq[4,2:6] + wsq[1,2:6])/2)

wg2g1
wg3g1
wg4g1
