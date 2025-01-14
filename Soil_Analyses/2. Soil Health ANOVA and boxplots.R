library(readxl)
library(patchwork)
library(tidyverse)
library(multcompView)

## Import Data
soil_bulk <- read_excel("../Bulk_Soil_Data/Kernza_BulkSoil_Soil.xlsx", sheet = "Chugwater")
plfa_groups_bulk <- read_excel("../Bulk_Soil_Data/Kernza_BulkSoil_PLFA.xlsx", sheet = "named_peaks_chugwater")

soil_rhizo <- read_excel("../Rhizosphere_Data/Rhizo_Soil.xlsx")
plfa_groups_rhizo <- read_excel("../Rhizosphere_Data/Rhizo_PLFA.xlsx", sheet = "named_peaks_Wyoming")

#### ANOVA TESTING FOR DIFFERENCES IN MEANS ####
data <- plfa_groups_rhizo

#Calculate means and SD. separate by year if there's a sig interaction

plfa_groups_bulk %>% 
  group_by (Crop, Year) %>% 
  ggpubr::get_summary_stats(AMF_percent, type = "mean_sd")

#create model
data$var <- data$AMF_percent

mod <- aov(var ~ Crop * Year, data = data)
summary(mod)

#test for normality
hist(resid(mod))
shapiro.test(resid(mod))

#if not normal, log transform (above)
MASS::boxcox(mod)

#posthoc tests for letters. separate by year if needed
data_1<- filter(data, Year == "Year 1")
data_2<- filter(data, Year == "Year 2")

mod1 <- aov(var ~ Crop, data = data_1)
mod2 <- aov(var ~ Crop, data = data_2)

summary(mod1)
summary(mod2)

TukeyHSD(mod1)
TukeyHSD(mod2)

#### BOXPLOTS with letters for differences ####
#prep for graph
my_colors <- c("CRP"= "cadetblue3", "CRP IWG"= "cadetblue3", 
               "Kernza"="darkseagreen", "Wheat" = "tan3", "Fallow" = "burlywood4")

#set data and value
data <- filter(soil_rhizo, Year == "Year 1")
data <- mutate(data, Crop = fct_relevel(Crop, "CRP IWG", "Kernza", "Wheat"))

data$value <- data$POXC

#create df with letters for sig dif
mod <- aov (value ~ Crop, data = data)
letters.df <- data.frame(multcompLetters(TukeyHSD(aov(value ~ Crop, data = data))$Crop[,4])$Letters)

colnames(letters.df)[1] <- "Letter"
letters.df$Crop <- rownames(letters.df)

placement <- data %>%
  group_by(Crop) %>%
  summarise(quantile(value, na.rm = TRUE)[4])

colnames(placement)[2] <- "placement"

letters <- left_join(letters.df, placement)

#if no sig difs, just tab out letters later
(poxc_1 <- 
   ggplot(data, aes(x= Crop, y= value, fill = Crop)) +
   geom_boxplot() +
   theme_bw(base_size = 10) +
   theme(panel.background = element_rect(fill = '#F8F8F8'),
         legend.position = "none",
  axis.title.x = element_blank()) + 
  scale_fill_manual(values = my_colors) +
   
   geom_text(data = letters, aes(x = Crop, y = placement + placement * 0.25, label= Letter)) +
   
   # ylim(1.5,5.1) + #protein
    ylim(150,750) + #poxc
   # ylim(30,140) + #micro
    labs(y = "Potentially Mineralizable C (mg/kg)", title = "Year 1"))

#patchwork
png(file = "soil health_plot_rhizo.png", width = 6.5, height = 5.5, units = "in", res = 800)


poxc_1 + protein_1 + micro_1 +
poxc_2 + protein_2 + micro_2 +
  plot_annotation(tag_levels = "a") + plot_layout(nrow = 2)

dev.off()
