#Code for summary statistics for all modelled/expert elicitation numbers excluding US EPA SCC

numbers_report_fin_Market_GDP <- as.tibble(numbers_report_fin_Market_GDP)
numbers_report_fin_Market_GDP <- numbers_report_fin_Market_GDP %>% gather(`2020`,`2030`, `2040`, `2050`, key = "year", value ="price")
numbers_report_fin_Market_GDP$year <- as.numeric(numbers_report_fin_Market_GDP$year)

numbers_report_fin_Market_GDP_1_tab <- numbers_report_fin_Market_GDP_1 %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    min = min(price, na.rm = TRUE),
    Q1 = quantile(price, c(0.25), na.rm = TRUE),
    median = median(price, na.rm = TRUE),
    mean = mean(price, na.rm = TRUE),
   # IQmean = mean(price, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(price, c(0.75), na.rm = TRUE),
    max = max(price, na.rm = TRUE)
  )

#Original boxplot for modelled values

ggplot(numbers_report_fin_Market_GDP_1, aes(x = factor(year), y = price)) + 
  #geom_violin() + 
  geom_boxplot(width = 0.3) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

#Code for all numbers from World Bank existing carbon price

qplot(y=existingcp, x= 1, geom = "boxplot") +
  scale_y_continuous(breaks = seq(0,200, by = 10)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
  labs(y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#data transformations for new charts requested 01/11/2017

#adding new column classifying modelled prices

numbers_report_fin_Market_GDP_1_with_existing <- numbers_report_fin_Market_GDP_1
numbers_report_fin_Market_GDP_1_with_existing$model_existing <- c("modelled")

#Existing nubmers imported from Excel as numbers_report_carbon_pricing_init_no_pilots_r

numbers_report_fin_Market_GDP_1_with_existing <- rbind(numbers_report_carbon_pricing_init_no_pilots_r, numbers_report_fin_Market_GDP_1_with_existing)

#create chart with existing carbon prices and modelled carbon prices

ggplot(numbers_report_fin_Market_GDP_1_with_existing, aes(x = factor(year), y = price, color = model_existing)) + 
  geom_boxplot(width = 0.3) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  scale_color_manual(name = "Carbon value source", labels = c("Existing carbon prices", "Modelled carbon values"), values = c("blue", "red")) +
  ggtitle("Existing carbon prices and modelled values") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

#Add US EPA SCC values to dataset
numbers_report_USEPA_SCC <- as.tibble(numbers_report_USEPA_SCC)
numbers_report_USEPA_SCC <- numbers_report_USEPA_SCC %>% gather(`2020`,`2030`, `2040`, `2050`, key = "year", value ="price")
numbers_report_USEPA_SCC$year <- as.numeric(numbers_report_USEPA_SCC$year)

#Add z to beginning of numbers so that they appear in order in plot

library(stringr)
str_sub(numbers_report_USEPA_SCC$scenario, 1,0) <- "z "

#Add geom_point plot with USEPA SCC values to boxplots of existing carbon prices and modelled carbon prices, note that generating two legends
#requires separating the grouping aesthetics so that the boxplots group on colour and the points group on fill

ggplot(numbers_report_fin_Market_GDP_1_with_existing, aes(x = factor(year), y = price)) + 
  geom_boxplot(fill = NA, size = 0.75, width = 0.3, aes(color = model_existing)) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  scale_color_manual(name = "Carbon value source", labels = c("Existing carbon prices", "Modelled carbon values \n(excluding US EPA SCC)"), values = c("blue", "red")) +
  geom_point(data = numbers_report_USEPA_SCC, aes(x = factor(year), y = price, fill = scenario), shape = 24, size = 3) +
  scale_fill_manual(name = "US EPA SCC values", labels = c("2.5% DR", "3% DR", "5% DR", "95th pct 3% DR"), values = c("green", "purple", "brown", NA)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), legend.key = element_blank()) +
  ggtitle("Existing carbon prices and modelled values") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))
  
#Create boxplots as facet grid

ggplot(numbers_report_fin_Market_GDP_1_with_existing, aes(x = factor(year), y = price)) + 
  geom_boxplot(fill = NA, size = 0.75, width = 0.3) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  facet_wrap(~ model_existing, nrow = 1, labeller = as_labeller(facet_names_fin_report)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), legend.key = element_blank()) +
  theme(strip.text.x = element_text(size = 14)) +
  ggtitle("Existing carbon prices and modelled values") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

facet_names_fin_report <- c(
  `Existing carbon price` = "Existing carbon pricing initiatives", 
  `modelled` = "Modelled carbon values")


#Create points alone for US EPA SCC
ggplot(numbers_report_fin_Market_GDP_1_with_existing, aes(x = factor(year), y = price)) +
geom_point(data = numbers_report_USEPA_SCC, aes(x = factor(year), y = price, fill = scenario), shape = 24, size = 3) +
  scale_y_continuous(breaks = seq(0,400, by = 25)) +
  labs(x = "Year", y = bquote(~AUD[2016]~'/t'~CO[2]~' ')) +
  scale_fill_manual(name = "US EPA SCC values", labels = c("2.5% DR", "3% DR", "5% DR", "95th pct 3% DR"), values = c("green", "purple", "brown", NA)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), legend.key = element_blank()) +
  ggtitle("US EPA Social Cost of Carbon Values") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

#Create points alone for US EPA SCC - coerced scale
ggplot(numbers_report_fin_Market_GDP_1_with_existing, aes(x = factor(year), y = price)) +
  geom_point(data = numbers_report_USEPA_SCC, aes(x = factor(year), y = price, fill = scenario), shape = 24, size = 3) +
  scale_y_continuous(limits = c(0,625), breaks = seq(0,625, by = 25)) +
  labs(x = "Year", y = bquote(~AUD[2016]~'/t'~CO[2]~' ')) +
  scale_fill_manual(name = "US EPA SCC values", labels = c("2.5% DR", "3% DR", "5% DR", "95th pct 3% DR"), values = c("green", "purple", "brown", NA)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), legend.key = element_blank()) +
  ggtitle("US EPA Social Cost of Carbon Values") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5))

#Create separate boxplots

#Boxplot for modelled values (same code as original report)

ggplot(numbers_report_fin_Market_GDP_1, aes(x = factor(year), y = price)) + 
  geom_boxplot(width = 0.3) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  labs(title = "Modelled Carbon Values", subtitle = "(excluding US EPA SCC values)") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5)) +
  theme(plot.subtitle = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust=0.5)) 

#Code for all numbers from World Bank existing carbon price (new boxplot) (coerced scale on request of DELWP)

ggplot(numbers_report_carbon_pricing_init_no_pilots_r, aes(x = factor(year), y = price)) + 
  geom_boxplot(width = 0.1) +
  scale_y_continuous(limits = c(0,625), breaks = seq(0,625, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  labs(title = "Existing carbon pricing initiatives", subtitle = "(World Bank data)") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5)) +
  theme(plot.subtitle = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust=0.5)) 

#Code for all numbers from World Bank existing carbon price (new boxplot) (coerced scale on request of DELWP)

ggplot(numbers_report_carbon_pricing_init_no_pilots_r, aes(x = factor(year), y = price)) + 
  geom_boxplot(width = 0.1) +
  scale_y_continuous(breaks = seq(0,250, by = 25)) +
  labs(x = "Year", y = bquote('AUD/t'~CO[2]~'or /t'~CO[2]~'-e')) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank() ) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +
  labs(title = "Existing carbon pricing initiatives", subtitle = "(World Bank data)") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0.5)) +
  theme(plot.subtitle = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=16, hjust=0.5)) 
  