library(readxl)
library(ggpubr)
library(patchwork)
require(lme4)
library(tidyverse)
library(car)
library(multcompView)

## Import Data
biomass <- read_excel("../Kernza_Biomass.xlsx", sheet = "Chugwater") %>% 
                      filter(Year != "Year 0")

roots <- read_excel("../Kernza_Biomass.xlsx", sheet = "Roots")

other_biomass <- read_excel("../Kernza_Biomass.xlsx", sheet = "Other")

#### Calculate means and SD and run ANOVAs ####
other_biomass %>% 
  group_by (Farm, Field, Year) %>% 
  ggpubr::get_summary_stats(Total_Biomass, type = "mean_sd")

#overall anova
mod <- aov(GrainYield_g.m2 ~ Field + Farm, data = other_biomass)
summary(mod)

#test for normality
hist(resid(mod))
shapiro.test(resid(mod))

#if not normal, log transform (must use aov to get boxcox)
mod2 <- lm((var+1) ~ Cover_Crops * Compost_Rate_Year * Sampling_Year, 
           data=OREI)
MASS::boxcox(mod2)

#posthoc tests for letters
TukeyHSD(aov(TotalBiomass_NoWeeds ~ Field, data = data))

#### BOXPLOTS with letters for differences ####

#set data and value
data <- filter(roots, Year == "Year 1")
data$value <- roots$TotalRootBiomass_NoWeeds

#create df with letters for sig dif
mod <- aov (value ~ Field, data = data)
letters.df <- data.frame(multcompLetters(TukeyHSD(aov(value ~ Field, data = data))$Field[,4])$Letters)

colnames(letters.df)[1] <- "Letter"
letters.df$Field <- rownames(letters.df)

placement <- data %>%
  group_by(Field) %>%
  summarise(quantile(value)[4])

colnames(placement)[2] <- "placement"

letters1 <- left_join(letters.df, placement)
letters1$Year <- "Year 1"

letters <- bind_rows(letters1, letters2) #%>% 
 # filter(Field != "CRP")

#GRAIN YIELD AND BIOMASS
(biomass_plot_chugwater <- 
      ggplot(biomass, aes(x= Field, y= TotalBiomass_NoWeeds, fill = Field)) +
    
    geom_boxplot() +
    theme_bw(base_size = 14) +
    
    theme(panel.background = element_rect(fill = '#F8F8F8'),
          legend.position = "none", axis.title.x = element_blank()) +
    
    labs(y = bquote("Aboveground Biomass (g /"~m^2~")")) +
    facet_wrap(~ Year) +
    
    #for Chugwater plot only:
      scale_fill_manual(values = c("cadetblue3", "darkseagreen", "tan3")) +
      geom_text(data = letters, aes(x = Field, y = placement + max(placement) * 0.2, label= Letter)) +
      labs (title = "Dryland")
    
    #for SAREC plot only:
      #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.8)) +
      #scale_fill_manual(values = c("springgreen4", "darkseagreen2")) +
      #labs(title = "Irrigated") +
      #scale_x_discrete(labels= c("Kernza\n(full irrigation)", "Kernza\n(partial irrigation)"))
    )

(root_plot <- 
    ggplot(roots, aes(x= Field, y= TotalRootBiomass_NoWeeds, fill = Field)) +
    
    geom_boxplot() +
    theme_bw(base_size = 14) +
    theme(panel.background = element_rect(fill = '#F8F8F8'),
          legend.position = "none", axis.title.x = element_blank()) +
    scale_fill_manual(values = c("cadetblue3", "darkseagreen", "tan3")) +
    geom_text(data = letters1, aes(x = Field, y = placement + max(placement) * 0.2, label= Letter)) +

  labs(y = bquote("Living Root Biomass (g /"~m^2~")")) +
  facet_wrap(~ "Year 2"))


(grain_plot_sarec <- 
    filter(other_biomass, Field != "CRP") %>% 
    
  ggplot(aes(x= Field, y= GrainYield_g.m2, fill = Field)) +
  geom_boxplot() +
  theme_bw(base_size = 14) +
  theme(panel.background = element_rect(fill = '#F8F8F8'),
        legend.position = "none", axis.title.x = element_blank()) + 
  #scale_fill_manual(values = c("darkseagreen", "tan3")) +
    
    #for SAREC plot only:
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.8)) +
    scale_fill_manual(values = c("springgreen4", "darkseagreen2")) +
    scale_x_discrete(labels= c("Kernza\n(full irrigation)", "Kernza\n(partial irrigation)")) +
  
    #geom_text(data = letters, aes(x = Field, y = placement + max(placement) * 0.2, label= Letter)) +
    labs(y = bquote("Grain Yield (g /"~m^2~")")) +

  facet_wrap(~ Year))

#patchwork

layout <- "
AABBC
AABBC
DDEEC
DDEE#
"
png(file = "biomass_plot.png", width = 11, height = 9, units = "in", res = 1000)

biomass_plot_chugwater + grain_plot + root_plot + 
  biomass_plot_sarec + grain_plot_sarec  + 
  plot_layout(design = layout) + plot_annotation(tag_levels = "a")

dev.off()

