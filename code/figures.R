# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figures
# Rachel Sippy

# 1: Figure 1
# 2: Supplemental Figure 1
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(gridExtra)
library(extrafont)
library(ggpattern)

load("fidelity_by_site_fill_level.RData")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1: Figure 1
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

fidelity_by_site_fill_level$period <- "Pre"
fidelity_by_site_fill_level$period[fidelity_by_site_fill_level$time == "FU"] <- "Post"
fidelity_by_site_fill_level$period <- factor(fidelity_by_site_fill_level$period, levels = c("Pre", "Post"))

fctrl <- subset(fidelity_by_site_fill_level, fidelity_by_site_fill_level$intervention == "Ctrl")
fthr <- subset(fidelity_by_site_fill_level, fidelity_by_site_fill_level$intervention == "iTHR")

cc <- ggplot(fctrl, aes(x = period, y = total_fidelity, fill = level)) +
  geom_bar(position = "stack", stat = "identity") + 
  facet_grid(cols = vars(site)) +
  scale_fill_manual(values = c("#6a1617","#BC2629", "#ea9597"), 
                    name = "Level", labels = c("Macro", "Meso", "Micro")) +
  ylim(0,200) +
  labs(title = "B. Comparison Sites") +
  xlab("Implementation Period") +
  ylab("Fidelity Score") +
  theme_minimal() +
  theme(text = element_text(family="Lucida Sans"),
        plot.caption = element_text(hjust = 0, size = 11), #Default is hjust=1
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(b = -1)),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9),
        strip.text.x = element_text(size = 9, margin = margin(t = 0)),
        legend.position = "top",
        legend.margin=margin(0),
        plot.margin=unit(c(0, 1, 0, 0.5), units="line"))

tt <- ggplot(fthr, aes(x = period, y = total_fidelity, fill = level)) +
  geom_bar(position = "stack", stat = "identity") + 
  facet_grid(cols = vars(site)) +
  scale_fill_manual(values = c("#17216c","#5968DB", "#99a3e9"), 
                    name = "Level", labels = c("Macro", "Meso", "Micro")) +
  ylim(0,200) +
  xlab("") +
  ylab("Fidelity Score") +
  labs(title = "A. Implementation Sites") +
  theme_minimal() +
  theme(text = element_text(family="Lucida Sans"),
        plot.title.position = "plot",
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9),
        strip.text.x = element_text(size = 9),
        legend.position = "bottom",
        legend.margin=margin(0),
        plot.margin=unit(c(0, 1, 0, 0.5), units="line"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(b = 0)))

tc <- grid.arrange(tt, cc, nrow = 2)

ggsave("Figure1.png", tc, width = 170, height = 100, units = "mm", dpi = 300)

# panel A
tt2 <- ggplot(fthr, aes(x = period, y = total_fidelity, pattern = level,
                        pattern_density = level)) +
  geom_bar(fill = "#5968DB", position = "stack", stat = "identity") + 
  geom_bar_pattern(fill = NA, color = "black",
                   pattern_angle = 45,
                   pattern_spacing = 0.08,
                   pattern_fill = "black",
                   pattern_color = "black",
                   pattern_shape = 21,
                   position = "stack", stat = "identity",
                   pattern_key_scale_factor = 0.3) + 
  facet_grid(cols = vars(site)) +
  scale_pattern_manual(name = "Level", values = c("stripe", "pch", "none"),
                       labels = c("Macro", "Meso", "Micro"), guide = 'none') +
  scale_pattern_density_manual(name = "Level", values = c(0.2, 0.25, 0),
                               labels = c("Macro", "Meso", "Micro"), guide = 'none') +
  ylim(0,200) +
  labs(title = "A. Implementation Sites") +
  xlab("") +
  ylab("Fidelity Score") +
  theme_minimal() +
  theme(text = element_text(family="Lucida Sans", color = "black"),
        plot.caption = element_text(hjust = 0, size = 10), #Default is hjust=1
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(b = -1)),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        plot.margin=unit(c(0, 0.5, 0, 0.5), units="line"),
        panel.spacing = unit(0,'lines'),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(b = 0)))

# panel B
cc2 <- ggplot(fctrl, aes(x = period, y = total_fidelity, pattern = level,
                         pattern_density = level)) +
  geom_bar(fill = "#BC2629", position = "stack", stat = "identity") + 
  geom_bar_pattern(fill = NA, color = "black",
                   pattern_angle = 45,
                   pattern_spacing = 0.08,
                   pattern_fill = "black",
                   pattern_color = "black",
                   pattern_shape = 21,
                   position = "stack", stat = "identity",
                   pattern_key_scale_factor = 0.3) + 
  facet_grid(cols = vars(site)) +
  scale_pattern_manual(name = "Level", values = c("stripe", "pch", "none"),
                     labels = c("Macro", "Meso", "Micro")) +
  scale_pattern_density_manual(name = "Level", values = c(0.2, 0.25, 0),
                               labels = c("Macro", "Meso", "Micro")) +
  ylim(0,200) +
  labs(title = "B. Comparison Sites") +
  xlab("Implementation Period") +
  ylab("Fidelity Score") +
  theme_minimal() +
  theme(text = element_text(family="Lucida Sans", color = "black"),
        plot.caption = element_text(hjust = 0, size = 10), #Default is hjust=1
        plot.title.position = "plot",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        legend.direction = "horizontal",
        legend.position = c(0.8, 1.3),
        legend.margin=margin(0),
        legend.key.size = unit(1, 'line'),
        plot.margin=unit(c(0.5, 0.5, 0, 0.5), units="line"),
        panel.spacing = unit(0,'lines'))

tc2 <- grid.arrange(tt2, cc2, nrow = 2, heights = c(1, 1.1))

ggsave("Figure1.png", tc2, width = 170, height = 113, units = "mm", dpi = 300)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1: Supplemental Figure 1
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++

surv <- data.frame(prop = c(0.875, 0.125, 1, 0, 
                            0.429, 0.286, 0.286, 
                            0.429, 0.429, 0.143), 
                  resp = c("Yes", "No", "Yes", "No", 
                           "Agree", "Neutral", "Disagree", "Agree", "Neutral", "Disagree"),
                  ques = c(rep(c("tools", "COP"), each = 2), rep(c("plan", "comm"), each = 3)))

fullq <- c("Use of tools and resources developed for the implementation of THRIVE",
           "Attendance at the i-THRIVE Community of Practice days",
           "i-THRIVE implementation has been carried out according to plan",
           "Frequent and good communication about THRIVE implementation within my organisation")
names(fullq) <- c("tools", "COP", "plan", "comm")
fullq2 <- data.frame(full = fullq, ques = names(fullq))

sur2 <- merge(surv, fullq2, by = "ques", all.x = TRUE)
sur2$resp <- factor(sur2$resp, levels = c("No", "Yes", "Disagree", "Neutral", "Agree"))
sur2$full2 <- str_wrap(sur2$full, width = 26)
sur2$full2 <- factor(sur2$full2, levels = c("Frequent and good\ncommunication about THRIVE\nimplementation within my\norganisation",
                                          "i-THRIVE implementation\nhas been carried out\naccording to plan",
                                          "Use of tools and resources\ndeveloped for the\nimplementation of THRIVE",
                                          "Attendance at the i-THRIVE\nCommunity of Practice days"))

bc <- ggplot(data = sur2) +
  geom_bar(aes(x = prop, y = full2, fill = resp), 
           stat = "identity", position = "stack") +
  scale_fill_manual(name = "Response", 
                    values = c("#7570b3", "#d95f02", "#4dac26", "#e6ab02", "#d01c8b"),
                    breaks = c("Yes", "No", "Agree", "Neutral", "Disagree")) +
  ylab("") +
  xlab("Proportion") +
  theme_bw() +
  theme(legend.position = "right",
        text = element_text(family = "Lucida Sans"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "line"))

ggsave("SFig1.png", bc, width = 170, height = 90, unit = "mm")
