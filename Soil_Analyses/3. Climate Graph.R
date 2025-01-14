library(tidyverse)
library(readxl)
library(patchwork)

climate <- read_xlsx("../../Chugwater Climate.xlsx", sheet = "Graphing")

#precipitation
(precip <- ggplot(climate, aes(x = Date, y = Precip_mm)) +
  geom_line(color = "blue", group = 1, size = 1) +
  geom_line(aes(x = Date, y = Mean_Precip), 
            group = 1, size = 1, color = "black") +
    
  theme_bw(base_size = 14) +
  theme(panel.background = element_rect(fill = '#F8F8F8'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  labs(y= "Precipitation (mm)", color = "",) +
  annotate("segment", x = c(2,7,15,18,27,30), 
           y = 180, yend = 130,
           colour = "red", size = 1,
           arrow = arrow(length=unit(0.3, "cm"))) +
  ylim(0,199) +
  annotate("label", x = c(1.5,7,15,18,27,30), 
           y = 180, 
           label = c("Kernza\nplanted", 
                     "Wheat\nPlanted",
                     "Year 1\nSoil Sampled", 
                     "Year 1\nHarvest",
                     "Year 2\nSoil Sampled",
                     "Year 2\nHarvest")))

#temperature
(temp <- ggplot(climate, aes(x = Date, y = Temp_C)) +
    geom_line(color = "green", group = 1, size = 1) +
   
    geom_line(aes(x = Date, y = Mean_Temp, color = "30-Year Average"), 
              group = 1, size = 1) +

    scale_color_manual(values = c("black")) +
    
    scale_x_discrete(labels= climate$Month) +
    
    theme_bw(base_size = 14) +
    theme(panel.background = element_rect(fill = '#F8F8F8'), legend.position = "bottom") +
    
    labs(y = "Temperature (\u00B0C)", 
         color = "",
         x = 
           "2021                                            2022                                            2023")) 

#save figure
png(file = "Climate_Graph.png", width = 10.5, height = 6, units = "in", res = 1000)

precip / temp + plot_layout(heights = c(2,1)) + plot_annotation(tag_levels = "a")

dev.off()

#Apr 2021: Kernza planted
#May 2021, 2022, 2023: soil sampling
#Sep 2021: Wheat planted
#Aug 2022 & 2023: Kernza harvest/ biomass sampling
