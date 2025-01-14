#This script analyzes alpha and beta diversity, and plots an ordination with environmental variables overlaid
  #Hannah Rodgers, Feb 2024

#packages
require(ggrepel)
require(phyloseq)
library(mgcv)
library(DHARMa)
require(vegan)
require(patchwork)
require(ggpubr)
require(tidyverse)

#load in data
load("../Kernza_ps_object.RData")


#set which data you're working with as ps
ps <- ps_bac_noNA

#basic things
sort(sample_sums(ps_AMF_noNA_pruned))

taxa <- data.frame(tax_table(ps))
unique(taxa$PHYLUM)
ntaxa(ps_AMF_noNA)

sum(sample_sums(ps_AMF_noNA)) /sum(sample_sums(ps_AMF))

#### ALPHA DIVERSITY ####

#rarefy and check read depth
sort(colSums(otu_table(ps)))

ps_rarefy <- rarefy_even_depth(ps, rngseed = 1992)
sort(colSums(otu_table(ps_rarefy)))

# Calculate diversity metrics and then add in metadata
metadata <- data.frame(sample_data(ps))

Richness <- estimate_richness(ps_rarefy, split = TRUE, measures = NULL)
Richness$Crop <- metadata$Crop
Richness$Year <- as.factor(metadata$Year)

# Plot all alpha diversity measures
plot_richness(ps_rarefy, "Crop") + geom_boxplot() + geom_point()

#Run an ANOVA
mod <- aov(Chao1 ~ Crop * Year, data = Richness)
summary(mod)

#test for normality
hist(resid(mod))
shapiro.test(resid(mod))

#if not normal, log transform (must use aov to get boxcox)
mod2 <- lm((var+1) ~ Cover_Crops * Compost_Rate_Year * Sampling_Year, 
           data=OREI)
MASS::boxcox(mod2)

#posthoc tests for letters
mod <- aov(Chao1 ~ Crop, data = Richness)
TukeyHSD(mod)

#### Beta Diversity (Permanova and Ordination) ####
ps1 <- subset_samples(ps, Year == "2023")

otu <- data.frame(otu_table(ps1))
metadata <- data.frame(sample_data(ps1))

# Calculate Bray Curtis distance matrix
adonis_dist <- vegdist(data.matrix(t(otu)), method="bray")

#calculate PERMANOVA
adonis2(adonis_dist ~ Crop, data = metadata, permutations = 50000)

pairwiseAdonis::pairwise.adonis2(adonis_dist ~ Crop, data = metadata, permutations = 50000)

#calculate and visualize dispersal
mod <- betadisper(adonis_dist, metadata$Crop, 
                  type = "centroid")
anova(mod)

### Plot Ordination with Environmental Variables Correlated ####

#perform NMDS. 
# if you don't set seed, randomization will yield slightly different results every time

set.seed(05261940)
nmds <- metaMDS(adonis_dist, distance = "bray") 
nmds # good stress value is below 0.2

# save NMDS scores
data.scores <- as.data.frame(scores(nmds$points))
  
#merge metadata with data.scores
metadata$Sample.ID <- rownames(metadata)
data.scores$Sample.ID <- metadata$Sample.ID

  data.scores <- data.scores %>% 
  dplyr::left_join(metadata, by = 'Sample.ID')

# PLOT ORDINATION
data.scores$Year <- as.factor(data.scores$Year)

(bac_ord2 <- ggplot(data.scores, aes(x = MDS1, y = MDS2, 
                                     color = Crop)) +
    
    geom_point(aes(fill = Crop), 
               colour = "black", pch = 21, size = 5) +
  
  stat_ellipse(lwd=1) +
  
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("cadetblue3", "darkseagreen", "tan3")) +
    scale_fill_manual(values = c("cadetblue3", "darkseagreen", "tan3")) +
    
  theme(panel.background = element_rect(fill = 'gray98')) +
    
    #theme(legend.position = "none") +
  facet_wrap(~ "Year 2") +
    labs(title = "Bacteria"))

#patchwork ordination and scatterplot
png(file = "DNA_ordination.png", width = 9, height = 8, units = "in", res = 900)

(bac_ord1 + bac_ord2) / (AMF_ord1 + AMF_ord2) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")

dev.off()
