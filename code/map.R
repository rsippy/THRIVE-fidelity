# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Map
# Rachel Sippy
#
# 1: Figure 2
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(sf)
library(tidyverse)
library(RColorBrewer)
library(ggspatial)
library(gridExtra)
library(ggthemes)
library(ggpubr)
library(cowplot)
library(extrafont)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Prepare shapefiles
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

ccg18 <- st_read("data/CCG_Apr_2018_UGCB_in_England.shp")

## add names of sites
ccg18$site <- NA
ccg18$site[grep("Bexley",ccg18$ccg18nm)] <- "Bexley"
ccg18$site[grep("Bradford",ccg18$ccg18nm)] <- "Bradford"
ccg18$site[grep("Airedale",ccg18$ccg18nm)] <- "Bradford"
ccg18$site[grep("Cambridge",ccg18$ccg18nm)] <- "CambPeter"
ccg18$site[grep("Camden",ccg18$ccg18nm)] <- "Camden"
ccg18$site[grep("Ipswich",ccg18$ccg18nm)] <- "EastSuff"
ccg18$site[grep("North Hertfordshire",ccg18$ccg18nm)] <- "Herts"
ccg18$site[grep("Herts Valley",ccg18$ccg18nm)] <- "Herts"
ccg18$site[grep("Lewisham",ccg18$ccg18nm)] <- "Lewisham"
ccg18$site[grep("Luton",ccg18$ccg18nm)] <- "Luton"
ccg18$site[grep("Manchester",ccg18$ccg18nm)] <- "MancSal"
ccg18$site[grep("Salford",ccg18$ccg18nm)] <- "MancSal"
ccg18$site[grep("Corby",ccg18$ccg18nm)] <- "NeneCorby"
ccg18$site[grep("Nene",ccg18$ccg18nm)] <- "NeneCorby"
ccg18$site[grep("Norfolk",ccg18$ccg18nm)] <- "Norfolk"
ccg18$site[grep("Norwich",ccg18$ccg18nm)] <- "Norfolk"
ccg18$site[grep("Yarmouth",ccg18$ccg18nm)] <- "Norfolk"
ccg18$site[grep("Portsmouth",ccg18$ccg18nm)] <- "Portsmouth"
ccg18$site[grep("Southampton",ccg18$ccg18nm)] <- "SouthHam"
ccg18$site[grep("Stockport",ccg18$ccg18nm)] <- "Stockport"
ccg18$site[grep("Stoke",ccg18$ccg18nm)] <- "Stoke"
ccg18$site[grep("Sunderland",ccg18$ccg18nm)] <- "Sunderland"
ccg18$site[grep("Tower Hamlets",ccg18$ccg18nm)] <- "TowerHam"
ccg18$site[grep("Waltham Forest",ccg18$ccg18nm)] <- "WalthamF"
ccg18$site[grep("Warrington",ccg18$ccg18nm)] <- "Warrington"
ccg18$site[grep("Worcestershire",ccg18$ccg18nm)] <- "Worcester"

# dummy var for sites
ccg18$sitei <- NA
ccg18$sitei[!is.na(ccg18$site)] <- 1

# number sites (28 sites)
ccg18$siten <- NA
ccg18$siten[is.na(ccg18$site)] <- 1:166
                                    #bradford                      #nene 
ccg18$siten[!is.na(ccg18$site)] <- c(167, 168, 167, 167, 169, 170, 171,
                                     172, 173, 172, 174, 175, 176, 171, 
                                    #hert #norf
                                     173, 177, 178, 179, 180, 181, 182,
                                               #manc
                                     183, 184, 185, 186, 173, 178, 173,
                                     173)
  
## merge ccgs to sites 186 features
sites18 <- ccg18 %>%
  group_by(siten, site) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

## site type
sites18$sitetype <- "Not enrolled"
sites18$sitetype[!is.na(sites18$site)] <- "Ctrl"
sites18$sitetype[sites18$site == "Bexley"] <- "iTHR"
sites18$sitetype[sites18$site == "CambPeter"] <- "iTHR"
sites18$sitetype[sites18$site == "Camden"] <- "iTHR"
sites18$sitetype[sites18$site == "Herts"] <- "iTHR"
sites18$sitetype[sites18$site == "MancSal"] <- "iTHR"
sites18$sitetype[sites18$site == "Stockport"] <- "iTHR"
sites18$sitetype[sites18$site == "TowerHam"] <- "iTHR"
sites18$sitetype[sites18$site == "WalthamF"] <- "iTHR"
sites18$sitetype[sites18$site == "Warrington"] <- "iTHR"
sites18$sitetype[sites18$site == "Luton"] <- "iTHR"

# test map
ggplot(sites18) + geom_sf(aes(fill = sitetype))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 Add outcome data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("fidelity_covars.RData")

fid <- fidelity_covars[fidelity_covars$time == "BL", c(1,4)]
colnames(fid)[2] <- "fid15"
fid$fid18 <- fidelity_covars[fidelity_covars$time == "FU", 4]
fid$diff <- fid$fid18 - fid$fid15

# merge to shapefiles
sites18b <- merge(sites18, fid, by = "site", all.x = TRUE)

# re-order for plotting
sites18b$sitetype <- factor(sites18b$sitetype, levels = c("Not enrolled", "Ctrl", "iTHR"))
sites18b <- sites18b[order(sites18b$sitetype),]

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 Border and boundary geometries
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# create line geometries for borders between iTHR and Ctrl
norf <- sites18b[sites18b$site == "Norfolk", ]
norf <- norf[!st_is_empty(norf), ]
camp <- sites18b[sites18b$site == "CambPeter", ]
camp <- camp[!st_is_empty(camp), ]
nene <- sites18b[sites18b$site == "NeneCorby", ]
nene <- nene[!st_is_empty(nene), ]

cnk <- st_intersection(camp, norf)
cne <- st_intersection(camp, nene)

# boxes of inset boundaries to add to main plot
bb1 = st_polygon(
  list(
    cbind(
      c(338000, 338000, 425000, 425000, 338000), 
      c(340000, 410000, 410000, 340000, 340000))
  )
)
bb1f <- st_sfc(bb1)
st_crs(bb1f) <- st_crs(sites18b)

bb2 = st_polygon(
  list(
    cbind(
      c(485000, 485000, 530000, 530000, 485000), 
      c(203000, 239300, 239300, 203000, 203000))
  )
)
bb2f <- st_sfc(bb2)
st_crs(bb2f) <- st_crs(sites18b)

bb3 = st_polygon(
  list(
    cbind(
      c(516000, 516000, 564000, 564000, 516000), 
      c(162000, 200000, 200000, 162000, 162000))
  )
)
bb3f <- st_sfc(bb3)
st_crs(bb3f) <- st_crs(sites18b)

bb4 = st_polygon(
  list(
    cbind(
      c(436000, 436000, 470000, 470000, 436000), 
      c(92300, 120000, 120000, 92300, 92300))
  )
)
bb4f <- st_sfc(bb4)
st_crs(bb4f) <- st_crs(sites18b)

# labels for boxes
lbls <- st_sfc(st_point(c(320000,375000)), st_point(c(467000, 221150)), 
               st_point(c(580000, 181000)), st_point(c(452500, 76000)))
st_crs(lbls) <- st_crs(sites18b) 
lblsf <- st_sf(lbls, txt = c("B", "C", "D", "E"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Inset maps
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# set palette
pal <- brewer.pal(11, "RdYlGn")[4:11]
pal2 <- pal[c(1,3:8)]

# inset B
i1 <- ggplot() + 
  geom_sf(data = sites18b, aes(fill = diff, color = sitetype, linewidth = sitetype), show.legend = FALSE) +
  coord_sf(xlim = c(338000, 425000), ylim = c(340000, 410000)) +
  scale_fill_gradientn(colors = pal2, na.value = "grey65") +
  scale_color_manual(values = c("grey90", "#BC2629", "#5968DB")) +
  scale_linewidth_manual(values = c(0.2, 0.8, 0.8)) +
  labs(title = "B. North West") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "plain", family = "Lucida Sans", vjust = -2),
        plot.title.position = "plot",
        plot.margin = unit(c(0,0,0.5,0), "line")) 

# inset C
i2 <- ggplot() + 
  geom_sf(data = sites18b, aes(fill = diff, color = sitetype, linewidth = sitetype), show.legend = FALSE) +
  coord_sf(xlim = c(485000, 530000), ylim = c(203000, 239300)) +
  scale_fill_gradientn(colors = pal2, na.value = "grey65") +
  scale_color_manual(values = c("grey90", "#BC2629", "#5968DB")) +
  scale_linewidth_manual(values = c(0.2, 0.8, 0.8)) +
  labs(title = "C. East of England") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "plain", family = "Lucida Sans", vjust = -2),
        plot.title.position = "plot",
        plot.margin = unit(c(0,0,0.5,0.5), "line")) 

# inset D
i3 <- ggplot() + 
  geom_sf(data = sites18b, aes(fill = diff, color = sitetype, linewidth = sitetype), show.legend = FALSE) +
  coord_sf(xlim = c(516000, 564000), ylim = c(162000, 200000)) +
  scale_fill_gradientn(colors = pal2, na.value = "grey65") +
  scale_color_manual(values = c("grey90", "#BC2629", "#5968DB")) +
  scale_linewidth_manual(values = c(0.2, 0.8, 0.8)) +
  labs(title = "D. London") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "plain", family = "Lucida Sans", vjust = -2),
        plot.title.position = "plot",
        plot.margin = unit(c(0,0,0.5,0), "line")) 

# inset E
i4 <- ggplot() + 
  geom_sf(data = sites18b, aes(fill = diff, color = sitetype, linewidth = sitetype), show.legend = FALSE) +
  coord_sf(xlim = c(436000, 470000), ylim = c(92300, 120000)) +
  scale_fill_gradientn(colors = pal2, na.value = "grey65") +
  scale_color_manual(values = c("grey90", "#BC2629", "#5968DB")) +
  scale_linewidth_manual(values = c(0.2, 0.8, 0.8)) +
  labs(title = "E. South East") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "plain", family = "Lucida Sans", vjust = -2),
        plot.title.position = "plot",
        plot.margin = unit(c(0,0,0.5,0.5), "line"))   

# check
grid.arrange(i1, i2, i3, i4, nrow = 2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.5 Main map
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

lgd <- ggplot() + 
  geom_sf(data = sites18b, aes(fill = diff, color = sitetype, linewidth = sitetype)) +
  scale_fill_gradientn(colors = pal2, na.value = "grey65") +
  scale_color_manual(name = "Site Type", values = c("grey90", "#BC2629", "#5968DB")) +
  scale_linewidth_manual(name = "Site Type", values = c(0.2, 0.8, 0.8)) +
  geom_sf(data = cnk, color = "#BC2629", linewidth = 0.8, show.legend = FALSE) +
  geom_sf(data = cne, color = "#BC2629", linewidth = 0.8, show.legend = FALSE) +
  geom_sf(data = cnk, color = "#5968DB", linewidth = 0.4, show.legend = FALSE) +
  geom_sf(data = cne, color = "#5968DB", linewidth = 0.4, show.legend = FALSE) +
  geom_sf(data = bb1f, fill = NA, color = "black", linewidth = 0.6) + 
  geom_sf(data = bb2f, fill = NA, color = "black", linewidth = 0.6) + 
  geom_sf(data = bb3f, fill = NA, color = "black", linewidth = 0.6) + 
  geom_sf(data = bb4f, fill = NA, color = "black", linewidth = 0.6) + 
  geom_sf_text(data = lblsf, aes(label = txt), family = "Lucida Sans") +
  labs(fill = "Change in Fidelity", color = "Site Type",
       title = "A. All Sites",
       x = "Longitude", y = "Latitude") +
  annotation_scale(location = "tl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  theme(text = element_text(family = "Lucida Sans"),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        plot.margin = unit(c(0,0,0,0), "line"), 
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),        
        legend.direction = "vertical",
        legend.box = "horizontal") 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.6 Combine main and insets
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# extract legend
legend <- get_legend(
  # create some space to the top of the legend
  lgd + theme(legend.box.margin = margin(0, 0, 0, 0))
)

# add legend to inset
ins <- plot_grid(i1, i2, i3, i4, nrow = 2)
insl <- plot_grid(ins, legend, nrow = 2, rel_heights = c(2, 1))

# main without legend
lgdl <- lgd + theme(legend.position = "none", 
                    plot.margin = unit(c(0,0.5,0,0), "line"))

# combine all
f2 <- plot_grid(lgdl, insl, ncol = 2, rel_widths = c(1.48,1))

ggsave("Figure2.png", f2, width = 170, height = 113, units = "mm", dpi = 300, bg = 'white')