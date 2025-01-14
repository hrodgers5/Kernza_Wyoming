library(readxl)
library(patchwork)
library(ggrepel)
library(vegan)
library(pairwiseAdonis)
library(tidyverse)

#bulk soil data
enzymes_bulk <- read_excel("../Bulk_Soil_Data/Kernza_BulkSoil_Enzymes.xlsx", sheet = "Chugwater")
plfa_groups_bulk <- read_excel("../Bulk_Soil_Data/Kernza_BulkSoil_PLFA.xlsx", sheet = "named_peaks_chugwater")
plfa_raw_bulk <- read_excel("../Bulk_Soil_Data/Kernza_BulkSoil_PLFA.xlsx", sheet = "raw_peaks_chugwater")

#rhizo soil data
enzymes_rhizo <- read_excel("../Rhizosphere_Data/Rhizo_Enzymes.xlsx")
plfa_groups_rhizo <- read_excel("../Rhizosphere_Data/Rhizo_PLFA.xlsx", sheet = "named_peaks_Wyoming")
plfa_raw_rhizo <- read_excel("../Rhizosphere_Data/Rhizo_PLFA.xlsx", sheet = "raw_peaks_Wyoming")

#### PERMANOVA ####

#use PERMANOVA (adonis testing) to test for sig differences between groups. separate by year if there's a sig interaction
plfa1 <- filter(plfa_raw_rhizo, Year == "Year 1")
plfa2 <- filter(plfa_raw_rhizo, Year == "Year 2")

adonis2 (plfa2[,c(5:67)] ~ Crop, data = plfa2, by = "terms")

pairwise.adonis2(plfa2[,c(5:67)] ~ Crop, data = plfa2)

#test for sig differences in dispersion (distance from centroid) between groups
disp2 <- betadisper(dist(enzymes2[,c(5:12)]),
                    #grouping variable
                    enzymes2$Crop, 
                    type = 'centroid')

anova(disp2)
boxplot(disp2)

#### PLOTTING ORDINATION ####
#set colors
my_colors <- c("CRP"= "cadetblue3", "CRP IWG"= "cadetblue3", 
               "Kernza"="darkseagreen", "Wheat" = "tan3", "Fallow" = "burlywood4")

plfa <-        filter(plfa_raw_rhizo, Year == "Year 2")
plfa_groups <- filter(plfa_groups_rhizo, Year == "Year 2")

#calculate PCA
plfa = plfa[ ,colSums(plfa != 0) > 0]

pca <- plfa[,5:61] %>% 
  prcomp(center = TRUE, scale. = TRUE)

#put PC1 and PC2 into dataframe
plfa[, c('PC1', 'PC2')] <- pca$x[, 1:2]

#use envfit to correlate other enzymes to ordination

 #vars <- enzymes[,5:12]
 vars <- select(plfa_groups, AMF, Bacteria, Fungi)

(en <- vegan::envfit(pca, vars, permutations = 10000, na.rm = TRUE, strata = NULL))

# use results from envfit to create arrows (continuous variables)
arrows = as.data.frame(scores(en, "vectors"))

#rescale arrows to fit nicely within scatterplot of our data
arrows$var = rownames(arrows)

mult = max(abs(plfa[, c('PC1', 'PC2')])) / max(abs(arrows[, 1:2])) / 2
arrows[, 1:2] = arrows[, 1:2] * mult

summary (pca)
#### PLOT IT! Use enzymes for scatterplot, and arrows for arrows ####
(plfa_rhizo_2 <- plfa %>% 
   
   ggplot(aes(x = PC1, y = PC2, 
              color = Crop)) +
   
   geom_point(aes(fill = Crop), 
              colour = "black", pch = 21, size = 5) +
   
   scale_fill_manual(values = my_colors) +
   scale_color_manual(values = my_colors) +
   
   stat_ellipse(aes(color = Crop), linewidth = 1) +
   geom_segment(data = arrows, 
                aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                inherit.aes = FALSE,
                arrow = arrow(length = unit(0.02, "npc"))) +
   
   geom_text_repel(data = arrows, 
                   #nudge_x = -0.5,
                    aes(PC1*1.2, PC2*1.2, label = var),
                    inherit.aes = FALSE) +
   
   theme_bw(base_size = 12) +
   theme(panel.background = element_rect(fill = 'gray98'), 
         plot.title = element_text(hjust = 0.5)) +
   
    facet_wrap( ~ Year) +
   
   #theme(legend.position = "none") +
   labs(x = "PC1 (55%)", y = "PC2 (11%)", title = "Rhizosphere Soil"))

#variance explained on x, y axes:
    #ENZYMES:               PLFAs:
    #rhizo Y1: 69, 20       63, 6
    #rhizo Y2: 60, 26       55,11
    #bulk Y1: 53, 30        57, 12
    #bulk Y2: 57, 26        43,10

#patchwork plot
png(file = "ordinations_plfa.png", width = 9, height = 8, units = "in", res = 1000)

(plfa_bulk_1 + plfa_bulk_2) / (plfa_rhizo_1 + plfa_rhizo_2) + plot_annotation(tag_levels = "a")

dev.off()
