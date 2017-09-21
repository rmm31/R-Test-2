#Reshape AUSPRICER data

AR5 <- as.tibble(AR5R)

AR5 <- AR5 %>% gather(`2020`,`2030`,`2040`, `2050`, key = "year", value ="price")

AR5$year <- as.numeric(AR5$year)


AR5a <-qplot(year, price, data=AR5, geom = "line", color = ModelScenario) + 
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

#0.25 trim to establish the interquartile mean (used by the London Interbank Offered Rate - see Wikipedia)

IQmean <- function(x, ...) {md1 <- mean(as.numeric(x), trim = 0.25, na.rm = TRUE)
md1
} 

#Code for IQR ribbon with IQmean
AR5rib <- ggplot(AR5, aes(year, price)) +
  stat_summary(fun.data = "iqr", geom = "ribbon", fill = alpha("blue", 1/5)) +
  stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "black") +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
 scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
 scale_y_continuous(breaks = seq(0,1200, by = 50)) +
theme(axis.text = element_text(size=7)) #+
# theme(legend.title=element_blank()) +
# theme(legend.position="bottom")

#Code for faceted selection of charts one for each price series
AR5facet <- qplot(year, price, data = AR5, geom = "line", group = ModelScenario) + facet_wrap(~ModelScenario) +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
  scale_x_continuous(breaks = pretty(cpricefin$year, n=5)) +
   theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
  theme(axis.text = element_text(size=7)) 

#Code to put faceted charts and ribbon on a single page.
multiplot(AR5a, AR5facet, AR5rib, cols=1)

#Create table of values to parallel chart

sumdatallfin <- cpricefin %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    Q1 = quantile(priceAUD2016, c(0.25), na.rm = TRUE),
    IQmean = mean(priceAUD2016, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(priceAUD2016, c(0.75), na.rm = TRUE)
  )