#Reshape AR5 data

library(tidyverse)

AR5_Subset_less_than_1000_for_Stephanie_20171204 <- as.tibble(AR5_Subset_less_than_1000_for_Stephanie_20171204)

AR5_Subset_less_than_1000_for_Stephanie_20171204 <- AR5_Subset_less_than_1000_for_Stephanie_20171204 %>% gather(`2020`,`2030`,`2040`, `2050`, key = "year", value ="price")

AR5_Subset_less_than_1000_for_Stephanie_20171204$year <- as.numeric(AR5_Subset_less_than_1000_for_Stephanie_20171204$year)

#Reshape all AR5 data

All_AR5_runs_at_450 <- as.tibble(All_AR5_runs_at_450)
All_AR5_runs_at_450 <- All_AR5_runs_at_450 %>% gather(`2020`,`2030`,`2040`, `2050`, key = "year", value ="price")
All_AR5_runs_at_450$year <- as.numeric(All_AR5_runs_at_450$year)

#Subset and reshape and convert to AUD2016 AR5 database you MUST use library stringr
library(stringr)
#Subset

ar5_public_version102_compare_compare_20150629_130000 <- as.tibble(ar5_public_version102_compare_compare_20150629_130000)

ar5_public_version102_carbon_price <- filter(ar5_public_version102_compare_compare_20150629_130000, VARIABLE == "Price|Carbon")

ar5_public_version102_carbon_price_450 <- filter(ar5_public_version102_carbon_price, str_detect(SCENARIO, ".450."))

ar5_public_version102_carbon_price_450_2020_2050 <- select(ar5_public_version102_carbon_price_450, MODEL, SCENARIO, REGION, VARIABLE, UNIT, `2020`, `2030`, `2040`, `2050`)

#Reshape

ar5_public_version102_carbon_price_450_2020_2050 <- ar5_public_version102_carbon_price_450_2020_2050 %>% gather(`2020`,`2030`,`2040`, `2050`, key = "year", value ="price")

#Convert



#Plot all model runs on one line plot

AR5a <-qplot(year, price, data=AR5_Subset_less_than_1000_for_Stephanie_20171204, geom = "line", color = scenario) + 
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(breaks = seq(0,1600, by = 100)) +
 theme(legend.title=element_blank()) +
 theme(legend.position="bottom") +
  theme(legend.text = element_text(size=7)) +
theme(axis.text = element_text(size=7))

#  IQR ribbon with median/trimmed mean line (note year must be numeric)

iqr <- function(x, ...) {qs <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = T)
names(qs) <- c("ymin", "ymax")
qs
}

Q1 <- function(x, ...) {md2 <- quantile(as.numeric(x), c(0.25), na.rm = T)
md2
}

Q3 <- function(x, ...) {md3 <- quantile(as.numeric(x), c(0.75), na.rm = T)
md3
}

Q2 <- function(x, ...) {md4 <- quantile(as.numeric(x), c(0.5), na.rm = T)
md4
}

#0.25 trim to establish the interquartile mean (used by the London Interbank Offered Rate - see Wikipedia)

IQmean <- function(x, ...) {md1 <- mean(as.numeric(x), trim = 0.25, na.rm = TRUE)
md1
} 

#Code for IQR ribbon with IQmean
AR5rib_steph_20171204 <- ggplot(AR5_Subset_less_than_1000_for_Stephanie_20171204, aes(year, price)) +
  stat_summary(fun.data = "iqr", geom = "ribbon", fill = alpha("blue", 1/5)) +
  stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "black") +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
 scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
 scale_y_continuous(breaks = seq(0,1200, by = 50)) +
theme(axis.text = element_text(size=7)) #+
# theme(legend.title=element_blank()) +
# theme(legend.position="bottom")

#Code for Q1, Q2 with IQmean - completed to send to Steph on 20171205

  ggplot(AR5_Subset_less_than_1000_for_Stephanie_20171204, aes(year, price)) +
  stat_summary(aes(year), fun.y = Q1, geom = "line", color = "blue", size=1) +
  # stat_summary(aes(year), fun.y = Q2, geom = "line", color = "red") +
  stat_summary(aes(year), fun.y = Q3, geom = "line", color = "green", size = 1) +
  stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "black", size = 1) +
  labs(x = "Year", y = bquote(~AUD[2016]~'/t'~CO[2]~' ')) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  scale_y_continuous(breaks = seq(0,1200, by = 50)) +
  theme(axis.text = element_text(size=7)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  ggtitle("Subset of AR5 modelled carbon prices 2020—2050") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

#Code for Q1, Q2 with IQmean for all AR5
ggplot(All_AR5_runs_at_450, aes(year, price)) +
  stat_summary(aes(year), fun.y = Q1, geom = "line", color = "blue", size=1) +
  # stat_summary(aes(year), fun.y = Q2, geom = "line", color = "red") +
  stat_summary(aes(year), fun.y = Q3, geom = "line", color = "green", size = 1) +
  stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "black", size = 1) +
  labs(x = "Year", y = bquote(~AUD[2016]~'/t'~CO[2]~' ')) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  scale_y_continuous(breaks = seq(0,1200, by = 50)) +
  theme(axis.text = element_text(size=7)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  ggtitle("Subset of AR5 modelled carbon prices 2020—2050") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

 
#Code for summary with boxplot

ggplot(AR5_Subset_less_than_1000_for_Stephanie_20171204, aes(x = factor(year), y = price)) + 
  #geom_violin() + 
  geom_boxplot(width = 0.3) +
  scale_y_continuous(breaks = seq(0,1000, by = 50)) +
  labs(x = "Year", y = bquote(~AUD[2016]~'/t'~CO[2]~'')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  ggtitle("Subset of AR5 modelled carbon prices 2020—2050") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

  


#Code for faceted selection of charts one for each price series
AR5facet <- qplot(year, price, data = fin_AR5_subset_market, geom = "line", group = model_scenario) + facet_wrap(~model_scenario) +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
  scale_x_continuous(breaks = pretty(cpricefin$year, n=5)) +
   theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
  theme(axis.text = element_text(size=7)) 

#Code to put faceted charts and ribbon on a single page.
multiplot(AR5a, AR5facet, AR5rib, cols=1)

#Create table of values to parallel chart

AR5_for_steph_tab_20171204 <- AR5_Subset_less_than_1000_for_Stephanie_20171204 %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    min = min(price, na.rm = TRUE),
    Q1 = quantile(price, c(0.25), na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    mean = mean(price, na.rm = TRUE),
    IQmean = mean(price, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(price, c(0.75), na.rm = TRUE),
    max = max(price, na.rm = TRUE)
  )

#Create table of values for all 450 scenarios
AR5_for_steph_tab_all_ar5_20171204 <- ar5_public_version102_carbon_price_450_2020_2050 %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    min = min(price, na.rm = TRUE),
    Q1 = quantile(price, c(0.25), na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    mean = mean(price, na.rm = TRUE),
    IQmean = mean(price, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(price, c(0.75), na.rm = TRUE),
    max = max(price, na.rm = TRUE)
  )
