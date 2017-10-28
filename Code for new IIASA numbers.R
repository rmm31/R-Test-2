library(tidyverse)
New_IIASA_Price_data <- as.tibble(New_IIASA_Price_data)
New_IIASA_Price_data <- New_IIASA_Price_data %>% gather(`2020`,`2030`, `2040`, `2050`, `2060`, `2070`, `2080`, `2090`, `2100`, key = "year", value ="price")
New_IIASA_Price_data$year <- as.numeric(New_IIASA_Price_data$year)

#transform US 2005 to AUD 2016 using world bank data from World Development Indicators http://databank.worldbank.org/data/
# USD2005 * AUD/USD market exchange rate 2005 * AUS GDP Deflator 2016 / AUS GDP Deflator 2005

New_IIASA_Price_data_AUS <- mutate(New_IIASA_Price_data, priceAUS = price*1.309473333*99.58627679/75.09789593)

#Minimum price of zero is a problem; transformed by adding small number to all prices

New_IIASA_Price_data_AUS <- mutate(New_IIASA_Price_data_AUS, priceAUS_add = priceAUS + 1 )

#Transform price data to log2 for chart

New_IIASA_Price_data_AUS <- mutate(New_IIASA_Price_data_AUS, priceAUSlog2add = log2(priceAUS_add) )

#Removew Baseline scenarios, which have carbon price of zero

New_IIASA_Price_data_AUS_no_baseline <- filter(New_IIASA_Price_data_AUS, !(Scenario == "SSP1-Baseline" | Scenario == "SSP2-Baseline" | Scenario == "SSP3-Baseline" | Scenario == "SSP4-Baseline" | Scenario == "SSP5-Baseline"))

#Look only at OECD data

New_IIASA_Price_data_AUS_no_baseline_OECD <- filter(New_IIASA_Price_data_AUS_no_baseline, Region == "R5.2OECD")

#Look only at SSP1 (sustainable pathway) data grepl('subset of text value', variable) there's also a !grepl function

New_IIASA_Price_data_AUS_no_baseline_OECD_SSP1 <- filter(New_IIASA_Price_data_AUS_no_baseline, grepl('SSP1', Scenario))
New_IIASA_Price_data_AUS_no_baseline_OECD_SSP2 <- filter(New_IIASA_Price_data_AUS_no_baseline, grepl('SSP2', Scenario))
New_IIASA_Price_data_AUS_no_baseline_OECD_SSP3 <- filter(New_IIASA_Price_data_AUS_no_baseline, grepl('SSP3', Scenario))
New_IIASA_Price_data_AUS_no_baseline_OECD_SSP4 <- filter(New_IIASA_Price_data_AUS_no_baseline, grepl('SSP4', Scenario))

#example for above filter(mtcars, grepl('Toyota|Mazda', type))


New_IIASA_Price_data_tot_AUD_no_baseline_OECD_SSP4_tab <- New_IIASA_Price_data_AUS_no_baseline_OECD_SSP4 %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    min = min(priceAUS, na.rm = TRUE),
    Q1 = quantile(priceAUS, c(0.25), na.rm = TRUE),
    median = median(priceAUS, na.rm = TRUE),
    mean = mean(priceAUS, na.rm = TRUE),
    IQmean = mean(priceAUS, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(priceAUS, c(0.75), na.rm = TRUE),
    max = max(priceAUS, na.rm = TRUE)
  )

New_IIASA_box <- ggplot(New_IIASA_Price_data_AUS_no_baseline_OECD, aes(x = factor(year), y = priceAUS)) + 
  #geom_violin() + 
  geom_boxplot(width = 0.3) + # coef = 0, outlier.shape = NA) + Setting outlier shape to NA removes outliers
  scale_y_continuous(limits = quantile(New_IIASA_Price_data_AUS$priceAUS, c(0.1, 0.75))) + #setting limit removes effect of outliers
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

New_IIASA_box 
