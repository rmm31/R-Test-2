#Code for summary statistics for all modelled/expert elicitation numbers excluding US EPA SCC

numbers_report_fin_Market_GDP <- as.tibble(numbers_report_fin_Market_GDP)
numbers_report_fin_Market_GDP <- numbers_report_fin_Market_GDP %>% gather(`2020`,`2030`, `2040`, `2050`, key = "year", value ="price")
numbers_report_fin_Market_GDP$year <- as.numeric(numbers_report_fin_Market_GDP$year)

numbers_report_fin_Market_GDP_tab <- numbers_report_fin_Market_GDP %>% 
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
 