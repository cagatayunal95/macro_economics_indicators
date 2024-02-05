# This work focuses on the various macroeconomis models for econometric sakes. 


# Here is 13 variables:NV.IND., TOTL.KD.ZG	, NE.IMP.GNFS.KD.ZG	, FM.AST.DOMO.ZG.M3	
# FM.AST.PRVT.ZG.M3	 , FM.AST.CGOV.ZG.M3 , NY.GDP.PCAP.KD.ZG , NV.IND.MANF.KD.ZG
# NV.SRV.TOTL.KD.ZG , 	BN.KLT.DINV.CD , 	BX.KLT.DINV.CD.WD , 	FM.AST.NFRG.CN , BM.KLT.DINV.CD.WD	

# All of the explanation of the variables is defined in an extra sheet. 
# Those terms named from World Bank. 
# This data lie down from 2013 to 2022 



library(ggplot2)
library(dplyr)
library(showtext)
library(dplyr)
library(stargazer)

options(scipen = 999) # getting rid of the scientific numeration. 


font_add_google("Special Elite", family = "special")
font_add_google("Roboto", "roboto")
font_add_google("Oswald", "oswald")
font_add_google("Pacifico", "pacifico")



showtext_auto()

### At first glance, most important relationship (according to literature) is 
### GDP growth and Foreign Direct Investment. 
### For consistency sakes, Foreign Direct Investment has been scaled. 
### Scaling formula is [Scaling factor =  max(FDI)/max(GDP growth)]


p <- ggplot() +
  geom_col(data = wb_data, aes(x = year, y = gdp_pc_growth), fill = "cornflowerblue") + 
  geom_line(data = data_with_line, aes(x = year, y = foreign_di_inflow_scaled), color = "coral3", group = 1, size = 2) +
  geom_text(data = wb_data, aes(x = year, y = gdp_pc_growth, label = round(gdp_pc_growth, 1)), vjust = 1, color = "blue", size = 5) +
  geom_text(data = data_with_line, aes(x = year, y = foreign_di_inflow_scaled, label = round(foreign_di_inflow_scaled, 1)), vjust = -2, color = "coral3", size = 5) +
  labs(x = "Year", y = "GDP per Capita", title = "GDP per Capita Growth and FDI Inflow Over Time") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Foreign Direct Investment Inflow")) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", color = "black", size = 12, family = "oswald"),
    axis.text.y = element_text(face = "plain", color = "black", size = 12, family = "oswald"),
    plot.title = element_text(face = "bold", size = 14, family = "oswald"),
    axis.title.x = element_text(face = "bold", size = 12, family = "oswald"),
    axis.title.y = element_text(face = "bold", size = 12, family = "oswald"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA)
  ) +
    annotate("text", x = Inf, y = Inf, label = "Scaling: FDI Inflow scaled to match GDP Growth", 
           hjust = 1.1, vjust = 1.1, size = 3.5, color = "coral3", fontface = "italic", family = "oswald")


print(p)


# 

# Now comparing manufacturing, service, and industry sector growths.

wb_data_long <- wb_data %>%
  gather(key = "sector", value = "growth_pct", services_growth, industry_growth, manufacturing_growth, factor_key=TRUE) %>%
  mutate(sector = factor(sector, labels = c("Services Growth", "Industry Growth", "Manufacturing Growth"))) # Rename sectors


ggplot(wb_data_long, aes(x = as.factor(year), y = growth_pct, color = sector)) +
  geom_line(aes(group = sector), size = 1.5) +  
  geom_point(size = 2) +  
  scale_color_manual(values = c("Services Growth" = "blue3", "Industry Growth" = "chartreuse2", "Manufacturing Growth" = "brown2")) +
  labs(title = "Growth Percentages: Services, Industry, and Manufacturing", family = "oswald",
       x = "Year", family = "oswald",
       y = "Growth Percentage (%)", family = "oswald",
       color = "Sector", family = "oswald") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, face = "plain", family = "oswald", size = 12),
    axis.text.y = element_text(size= 12, family = "oswald"),
    plot.title = element_text(size = 14, face = "bold", family = "oswald"),
    legend.title = element_text(size = 12, family = "oswald"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))



##

# Comparing the PSG, Imports growth, and GDP per capita growth. 

ggplot(wb_data, aes(x = as.factor(year))) +
  geom_col(aes(y = gdp_pc_growth), fill = "cornflowerblue") +  
  geom_line(aes(y = imports_growth, color = "Imports Growth", group = 1), size = 1.5) + 
  geom_point(aes(y = imports_growth, color = "Imports Growth"), size = 2) +
  geom_line(aes(y = private_sector_growth, color = "Private Sector Growth", group = 1), size = 1.5) + 
  geom_point(aes(y = private_sector_growth, color = "Private Sector Growth"), size = 2) +
  labs(
    title = "Economic Indicators Over Time", family = "oswald",
    x = "Year", family = "oswald",
    y = "Growth Percentage (%)", family = "oswald",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Imports Growth" = "red", "Private Sector Growth" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, face = "plain", family = "oswald", size = 12),
    axis.text.y = element_text(size= 12, family = "oswald"),
    plot.title = element_text(size = 14, face = "bold", family = "oswald"),
    legend.title = element_text(size = 12, family = "oswald"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )


###


# Now doing some math and statistics 




# Model 1: GDP per capita growth on foreign direct investment
model1 <- lm(gdp_pc_growth ~ foreign_di_inflow, data = wb_data)

# Model 2: Foreign DI inflow on services, industry, and manufacturing growth
model2 <- lm(foreign_di_inflow ~ services_growth + industry_growth + manufacturing_growth, data = wb_data)

# Model 3: Private sector growth on imports growth
model3 <- lm(private_sector_growth ~ imports_growth, data = wb_data)



stargazer(model1, model2, model3, type = "text",
          title = "Regression Models",
          header = FALSE,
          model.names = FALSE,
          column.labels = c("Model 1", "Model 2", "Model 3"),
          covariate.labels = c("Foreign DI", "Services Growth", "Industry Growth", "Manufacturing Growth", "Imports Growth"),
          omit.stat = c("f", "ser"),
          digits = 2,
          out = "file/path/regression_table.text")


## ### #### 

