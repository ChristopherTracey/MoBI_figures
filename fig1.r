
library(tidyverse)
library(rstatix)
library(ggpubr)

#protectgrp <- read.csv("DataFig.csv", stringsAsFactors=FALSE)
#protectgrp <- pivot_longer(protectgrp, everything())

protectgrp <- read.csv("DataFig1_202104.csv", stringsAsFactors=FALSE)

protectgrp <- protectgrp[which(protectgrp$GAP12_Protected!=""),] 
names(protectgrp) <- c("group", "percentprotected")
protectgrp[which(protectgrp$group=="Vascular Plants"),"group"] <- "Plants"
protectgrp[which(protectgrp$group=="Freshwater Intertebrates"),"group"] <- "Freshwater Invertebrates"
#protectgrp$percentprotected <- as.numeric(gsub("[\\%,]", "", protectgrp$percentprotected))
protectgrp$percentprotected <- protectgrp$percentprotected*100
#protectgrp[which(protectgrp$group=="Aquatic.Inverts"),]$group <- "Freshwater Invertebrates"


protectgrp <- protectgrp %>% mutate(group=factor(group, levels=c("Plants","Vertebrates","Freshwater Invertebrates","Pollinators" ) ))
levels(protectgrp$group)

kruskal.test(percentprotected~group, data=protectgrp)
pairwise.wilcox.test(protectgrp$percentprotected, protectgrp$group, p.adjust.method = "BH")


res.kruskal <- protectgrp %>% kruskal_test(percentprotected~group)
res.kruskal
protectgrp %>% kruskal_effsize(percentprotected~group)

pwc <- protectgrp %>%  dunn_test(percentprotected~group, p.adjust.method = "bonferroni") 
pwc
pwc2 <- protectgrp %>% wilcox_test(percentprotected~group, p.adjust.method = "bonferroni")
pwc2

# function to get the count
stat_box_data <- function(x, upper_limit = max(diamonds$price) * 1.15) {
  return( 
    data.frame(
      y = min(protectgrp$percentprotected) - 2.5,
      label = paste('n =', format(length(x), big.mark=",", decimal.mark=".", scientific=FALSE))
    )
  )
}


x <- ggplot(protectgrp, aes(x=group, y=percentprotected)) + #, fill=group
  geom_boxplot(fill="#1f78b4") +
  labs(x="Taxonomic Group", y = "Percent of Suitable Habitat Overlapping Protected Areas", aes(size=10)) + #title="Plot of length  per dose",
  stat_summary(fun.data=stat_box_data, geom="text", hjust = 0.5, vjust = 0.9, size=4) +
  annotate("text", x = 1, y = 105, label = "a", size=5) + 
  annotate("text", x = 2, y = 105, label = "a", size=5) + 
  annotate("text", x = 3, y = 105, label = "b", size=5) + 
  annotate("text", x = 4, y = 105, label = "a", size=5) + 
  theme_classic() +
  theme(
    legend.position="none",
    axis.title=element_text(size=14, face="plain"),
    aspect.ratio = 0.6:1,
    axis.text=element_text(size=12)
  ) 
  
ggsave("fig1.tiff",  plot=x, width=8, height=6, antialias="default", units="in", dpi=300)


