library(here)
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(ggpubr)


lcmClimate <- read.csv("LCM_Climate_by_Spp.csv", stringsAsFactors=FALSE)
# get a report of the counts in various taxonomic groups
table(lcmClimate$Category)

lcmClimate$catGpAq <- as.character(lcmClimate$Category.Aquatic)
lcmClimate[lcmClimate$catGpAq %in% c("Aquatic Invertebrates", "Aquatic Vertebrates"), "catGpAq"] <- "Freshwater Animals"
lcmClimate[lcmClimate$catGpAq %in% c("Pollinating Invertebrates"), "catGpAq"] <- "Pollinators"
lcmClimate[lcmClimate$catGpAq %in% c("Vascular Plants"), "catGpAq"] <- "Plants"
lcmClimate$catGpAq <- as.factor(lcmClimate$catGpAq)
# check to see how it changed
table(lcmClimate$catGpAq)

# lcmClimate$AquaticSpp <- as.factor(lcmClimate$AquaticSpp)
# lcmClimate$Category <- as.factor(lcmClimate$Category)
# lcmClimate$Category <- factor(lcmClimate$Category, levels=c("Vascular Plants", "Pollinating Invertebrates", "Vertebrates", "Aquatic Invertebrates"))
# lcmClimate$Category.Aquatic <- as.factor(lcmClimate$Category.Aquatic)


####################################################################################################################################################
# BoxPlots
g <- rasterGrob(c("cyan4","cyan4","wheat3","goldenrod4"), y=c(0, 0.25, 0.36, 0.5), width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE) # height=unit(1,"npc"), 

##############################
# lcm boxplots
lcm_all <- ggplot(lcmClimate, aes(x=catGpAq, y=LCM_all)) + #, fill=AquaticSpp
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=100) + 
  geom_boxplot() +
  ggtitle("Landscape Condition - All") +
  xlab("Category") + ylab("Landscape Condition") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# split by aquatics  
lcm_all_Aq <- ggplot(lcmClimate, aes(x=catGpAq, y=LCM_all, fill=AquaticSpp)) + #, fill=AquaticSpp
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=100) + 
  geom_boxplot() +
  scale_fill_manual(values=c("gray100", "gray67"), labels=c("Terrestrial", "Aquatic")) +
  ggtitle("Landscape Condition - All") +
  xlab("Category") + ylab("Landscape Condition") +
  theme_bw() +
  theme(legend.position = "top") +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.justification=c(1,0.9), legend.position=c(0.99,0.91),
        axis.title.x = element_blank(),
        legend.title=element_blank())

#LCM no 12
lcm_no12 <- ggplot(lcmClimate, aes(x=catGpAq, y=LCM_no12)) + #, fill=AquaticSpp
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=100) + 
  geom_boxplot() +
  ggtitle("Landscape Condition - no Gap 1/2") +
  xlab("Category") + ylab("Landscape Condition") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


# split by aquatics  
lcm_no12_Aq <- ggplot(lcmClimate, aes(x=catGpAq, y=LCM_no12, fill=AquaticSpp)) + #, fill=AquaticSpp
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=100) + 
  geom_boxplot() +
  scale_fill_manual(values=c("gray100", "gray67"), labels=c("Terrestrial", "Aquatic")) +
  ggtitle("Landscape Condition - no Gap 1/2") +
  xlab("Category") + ylab("Landscape Condition") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        legend.justification=c(1,0.9), legend.position=c(0.99,0.91),
        axis.title.x = element_blank()) 


png("lcm.png", width = 960, height = 480)
grid.arrange(lcm_all, lcm_no12, ncol=2, widths=c(1,1))
dev.off()


png("lcm_Aq.png", width = 960, height = 480)
grid.arrange(lcm_all_Aq, lcm_no12_Aq, ncol=2, widths=c(1,1))
dev.off()

#############################################
# Climate

h <- rasterGrob(c("cyan4","cyan4","wheat3","goldenrod4"), y=c(0, 0.25, 0.36, 0.5), width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE) # height=unit(1,"npc"), 

ct_all <- ggplot(lcmClimate, aes(x=catGpAq, y=CT_all)) + #, fill=AquaticSpp
  #annotation_custom(h, xmin=-Inf, xmax=Inf, ymin=0, ymax=1) +
  geom_boxplot() +
  ggtitle("Climate Typicality - All") +
  xlab("Category") + ylab("Typicality") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


# split by aquatics  
ct_all_Aq <- ggplot(lcmClimate, aes(x=catGpAq, y=CT_all, fill=AquaticSpp)) + #, fill=AquaticSpp
  #annotation_custom(h, xmin=-Inf, xmax=Inf, ymin=0, ymax=1) +
  geom_boxplot() +
  scale_fill_manual(values=c("gray100", "gray67"), labels=c("Terrestrial", "Aquatic")) +
  ggtitle("Typicality - All") +
  xlab("Category") + ylab("Climate Typicality") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.justification=c(1,0.9), legend.position=c(0.99,0.91),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


#LCM no 12
ct_no12 <- ggplot(lcmClimate, aes(x=catGpAq, y=CT_no12)) + #, fill=AquaticSpp
  #annotation_custom(h, xmin=-Inf, xmax=Inf, ymin=0, ymax=1) +
  geom_boxplot() +
  ggtitle("Climate Typicality - no Gap 1/2") +
  xlab("Category") + ylab("Climate Typicality") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


# split by aquatics  
ct_no12_Aq <- ggplot(lcmClimate, aes(x=catGpAq, y=CT_no12, fill=AquaticSpp)) + #, fill=AquaticSpp
  #annotation_custom(h, xmin=-Inf, xmax=Inf, ymin=0, ymax=1) +
  geom_boxplot() +
  scale_fill_manual(values=c("gray100", "gray67"), labels=c("Terrestrial", "Aquatic")) +
  ggtitle("Climate Typicality - no Gap 1/2") +
  xlab("Category") + ylab("Climate Typicality") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=11),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.justification=c(1,0.9), legend.position=c(0.99,0.91),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


png("ct.png", width = 960, height = 480)
grid.arrange(ct_all, ct_no12, ncol=2, widths=c(1,1))
dev.off()


png("ct_Aq.png", width = 960, height = 480)
grid.arrange(ct_all_Aq, ct_no12_Aq, ncol=2, widths=c(1,1))
dev.off()

############################################################################################################################################
# Scatterplot

shapes <- data.frame(levels(lcmClimate$catGpAq), c(18, 15, 17, 16))
shapes <- data.frame(levels(lcmClimate$catGpAq), c(21, 22, 23, 24))
names(shapes) <- c("catGpAq","shaper")

lcmClimate <- merge(lcmClimate, shapes, by="catGpAq")

lcmClimate1 <- lcmClimate %>% mutate(name=factor(catGpAq, levels=c("Vascular Plants","Aquatic Animals","Pollinators","Terrestrial Vertebrates"  ) ))

# add in protected land
proland <- read.csv(here::here("mobi_protectedland.csv"), stringsAsFactors = FALSE)
names(proland) <- c("cutecode","sciName","ProtectedGap12","ProtectedGap13")
proland$ProtectedGap13 <- gsub("%", "", proland$ProtectedGap13)
proland$ProtectedGap13 <- as.numeric(proland$ProtectedGap13)

proland$gt50 <- ifelse(proland$ProtectedGap13>=50, "yes", NA) 

lcmClimate1 <- merge(lcmClimate1, proland[c("cutecode", "gt50")], by="cutecode")
lcmClimate1$gt50 <- as.factor(lcmClimate1$gt50)


####

df <- data.frame(
  x = c(1, 1, 100, 100),
  y = c(0, 1, 0, 1),
  text = c("Poor condition\nNovel climate", "Poor condition\nTypical climate", "Good condition\nNovel climate", "Good condition\nTypical climate"))

p <- ggplot(lcmClimate1, aes(LCM_all, CT_all, color=catGpAq, shape=catGpAq)) + #, fill=gt50
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 1),clip = 'off') +
  #scale_y_continuous(limits=c(0,1)) +
  geom_point(alpha=0.6, size=3) +
  scale_color_manual(values=c("#1f78b4", "#ff7f00", "#e31a1c", "#33a02c")) + #"#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"
  scale_shape_manual(values=c(18, 15, 17, 1)) +
  #scale_shape_manual(values=c(21, 22, 23, 24)) +
  #scale_alpha_manual(values = c(0,1)) +
  ylab("Climate Typicality") +
  xlab("Landscape Condition") +
  # ylab("Increasing Climate Typicality") +
  # xlab("Increasing Landscape Condition") +
  #annotate("segment", x=47, xend=97, y=-0.11, yend=-0.11, size=2.53, arrow=arrow(), linejoin='mitre') +
  #annotate("segment", x=-13, xend=-13, y=0.44, yend=0.97, size=2.53, arrow=arrow(), linejoin='mitre') + #length=unit(0.5, "cm")
  theme_classic() +
   #theme(axis.title.x=element_text(margin=margin(0,-100,0,0))) +
   theme(legend.position="bottom") +
   theme(axis.title=element_text()) +
   font("xylab", size=16, family="sans") +
   font("axis.text", size=12, family="sans") +
   font("legend.text", size=12, family="sans") +
   theme(legend.title=element_blank(), legend.spacing.x=unit(.1,'cm'), axis.line=element_line(colour='black', size = 1)) +
   geom_text(data=df, aes(x, y, label=text, hjust="inward", vjust="inward"),size=4, family="sans", fontface="italic", inherit.aes=FALSE, show.legend=FALSE) +
   geom_vline(xintercept=mean(lcmClimate$LCM_all), color="gray32", linetype="twodash", size=0.9) +
   geom_hline(yintercept=mean(lcmClimate$CT_all, na.rm=TRUE), color="gray32", linetype="twodash", size=0.9) +
  #theme(plot.margin = margin(2, 4, 2, 2, "cm")) +
  theme(aspect.ratio=1) 
p

q <- ggExtra::ggMarginal(p, type="density", fill="gray82", groupColour=TRUE) #
q

###ggsave("lcmClimate.tiff", plot=q, width=8, height=8, units="in", dpi=300)

dev.off()



# figure 6 less cool alternate

library(extrafont)
font_import()

plot_lcm <- ggplot(lcmClimate1, aes(LCM_all, color=catGpAq, fill=catGpAq)) +
  geom_density(alpha=0.2, size=1) +
  scale_fill_manual(values=c("#1f78b4", "#ff7f00", "#e31a1c", "#33a02c")) +
  scale_color_manual(values=c("#1f78b4", "#ff7f00", "#e31a1c", "#33a02c")) +
  xlim(0, 100) +
  xlab("Landscape Condition") +
  ylab("Density") +
  annotate("text", x=20, y=0.033, label="*", fontface='bold', size=10) +
  theme_classic() +
  theme(
    legend.position="bottom",
    legend.title=element_blank(),
    aspect.ratio=1,
    axis.title=element_text()
  ) +
  font("xylab", size=14, family="sans") +
  font("axis.text", size=12, family="sans") +
  font("legend.text", size=14, family="sans")

plot_ct <- ggplot(lcmClimate1, aes(CT_all, color=catGpAq, fill=catGpAq)) +
  geom_density(alpha=0.2, size=1) +
  scale_fill_manual(values=c("#1f78b4", "#ff7f00", "#e31a1c", "#33a02c")) +
  scale_color_manual(values=c("#1f78b4", "#ff7f00", "#e31a1c", "#33a02c")) +
  xlim(0, 1) +
  xlab("Climate Typicality") +
  ylab("Density") +
  annotate("text", x=0.8, y=5.6, label="*", fontface='bold', size=10) +
  theme_classic() +
  theme(
    legend.position="bottom",
    legend.title=element_blank(),
    aspect.ratio=1,
    axis.title=element_text()
  ) +
  font("xylab", size=14, family="sans") +
  font("axis.text", size=12, family="sans") +
  font("legend.text", size=14, family="sans")


z <- ggarrange(plot_lcm, plot_ct, # list of plots
          labels="auto", # labels
          common.legend=TRUE, # COMMON LEGEND
          legend="bottom", # legend position
          align="hv", # Align them both, horizontal and vertical
          ncol=2)  # number of rows

ggsave("fig6.tiff",  plot=z, width=8, antialias="default", units="in", dpi=300)

###ggsave("lcmClimate.tiff", plot=q, width=8, height=8, units="in", dpi=300)
