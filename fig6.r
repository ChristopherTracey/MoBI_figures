library(tidyverse)
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
