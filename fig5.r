library(tidyverse)

options(scipen = 999) # don't use scientific notation

overlap <- read.csv("AUBIdatForChris_20210412.csv", stringsAsFactors=FALSE)
overlap <- overlap[which(overlap$thresh>=0.000001),] 

overlap[which(overlap$mobi_gp=="Crocodilians, Reptiles, and Turtles"), "mobi_gp"] <- "Reptiles"
overlap[which(overlap$mobi_gp=="Pollinating Invertebrates"), "mobi_gp"] <- "Pollinators"

unique(overlap$mobi_gp)

overlap$mobi_gp <- factor(overlap$mobi_gp, levels=c("Amphibians, Birds, Mammals","Reptiles","Freshwater Invertebrates","Pollinators","Freshwater and Anadromous Fishes","Plants"))


x <- ggplot(data=overlap, aes(x=thresh+0.0000001, y=mn_prp, group=mobi_gp)) +
  #geom_ribbon(aes(fill=mobi_gp, ymin=mn_prp-sd_prp, ymax=mn_prp+sd_prp), alpha=0.3) +
  geom_point(aes(color=mobi_gp), size=1.5) +
  geom_line(aes(color=mobi_gp), size=1.25) +
  geom_vline(xintercept = 0.0005, linetype="twodash", color="darkgrey", size=0.5) +
  geom_errorbar(aes(color=mobi_gp, ymin=mn_prp-sd_prp, ymax=mn_prp+sd_prp), width=0.05) + #, position="dodge"
    annotate("text", x=0.0005, y=0.02, label="0.0005", hjust=-0.05, color="black", size=4) +
  scale_x_log10(
    breaks=c(0.000001,0.00001,0.0001,0.001,0.01),
    labels=c(0.000001,0.00001,0.0001,0.001,0.01)
  ) + 
  xlab("Threshold Used to Define Bird/Mammal/Amphibian AUBIs") +
  ylab("Proportion of species with distributions intersecting AUBIs") +
  theme_classic() +
  theme(
    legend.position=c(0.20,0.2),
    legend.title=element_blank(),
    legend.text=element_text(size=10),
    axis.title=element_text(size=14, face="plain", color="black"),
    aspect.ratio=0.6:1,
    axis.text=element_text(size=12)
  ) 


ggsave("fig5.tiff",  plot=x, width=8, height=6, antialias="default", units="in", dpi=300)

