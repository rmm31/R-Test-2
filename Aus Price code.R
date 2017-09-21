#Reshape AUSPRICER data

ALPR_price <- as.tibble(LPF_ausprice)

ALPR_price <- ALPR_price %>% gather("CPRS_5", "CPRS_15", "Garnaut_10", "Garnaut_25", key = "source", value ="priceAUD2016")

#Combine AUSPRICE data with SGLP 2010 modelling

auspriceall <- rbind(ALPR_price, SGLP_price1)

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
allribbon <- ggplot(auspriceall, aes(year, priceAUD2016)) +
  stat_summary(fun.data = "median_hilow", geom = "ribbon", fill = alpha("green", 1/5)) +
  stat_summary(fun.data = "iqr", geom = "ribbon", fill = alpha("blue", 1/5)) +
  stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "black") +
  stat_summary(aes(year), fun.y = median, geom = "line", color = "red") +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'e or t'~CO[2])) +
  scale_x_continuous(breaks = seq(2010, 2050, by = 5)) +
  scale_y_continuous(breaks = seq(0,800, by = 25)) +
  theme(axis.text = element_text(size=7)) #+
# theme(legend.title=element_blank()) +
# theme(legend.position="bottom")

#Code for faceted selection of charts one for each price series
ausallfacet <- qplot(year, priceAUD2016, data = auspriceall, geom = "line", group = source) + facet_wrap(~source, labeller = as_labeller(facet_names)) +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'e or t'~CO[2])) +
  scale_x_continuous(breaks = pretty(cpricefin$year, n=5)) +
  theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
  theme(axis.text = element_text(size=7)) +
  theme(strip.text.x = element_text(size = 7))
#Code to change names for facets
facet_names <- c(
  `CPRS_15` = "CPRS -15 Aust Emissions Price",
  `CPRS_5` = "CPRS -5 Aust Emissions Price",
  `Garnaut_25` = "Garnaut -25 Aust Emissions Price",
  `Garnaut_10` = "Garnaut -10 Aust Emissions Price",
  `SGLP_core` = "Strong Growth, Low Pollution Core",
  `Clean_Energy_Future` = "Strong Growth, Low Pollution Clean Energy Future",
  `Government_policy` = "Strong Growth, Low Pollution Government Policy",
  `High_price_scenario` = "Strong Growth, Low Pollution High Price"
)
#Code to put faceted charts and ribbon on a single page.
multiplot(Govmod, allfacet, allribbon,cols=1)

#Create table of values to parallel chart

sumdatallausfin <- auspriceall %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    Min = quantile(priceAUD2016, c(0.0), na.rm = TRUE),
    Quan05 = quantile(priceAUD2016, c(0.05), na.rm = TRUE),
    Quan10 = quantile(priceAUD2016, c(0.1), na.rm = TRUE),
    Quan15 = quantile(priceAUD2016, c(0.15), na.rm = TRUE),
    Q1 = quantile(priceAUD2016, c(0.25), na.rm = TRUE),
    IQmean = mean(priceAUD2016, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(priceAUD2016, c(0.75), na.rm = TRUE),
    Quan85 = quantile(priceAUD2016, c(0.85), na.rm = TRUE),
    Quan90 = quantile(priceAUD2016, c(0.90), na.rm = TRUE),
    Quan95 = quantile(priceAUD2016, c(0.95), na.rm = TRUE),
    Max = quantile(priceAUD2016, c(1.0), na.rm = TRUE)
  )


# Code for line chart

auspriceline <-qplot(year, priceAUD2016, data=auspriceall, geom = "line", color = source) + 
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'e')) +
  scale_x_continuous(breaks = seq(2010, 2050, by = 5)) +
  scale_y_continuous(breaks = seq(0,800, by = 50), sec.axis = dup_axis()) +
  theme_bw () +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text = element_text(size=7)) +
  theme(axis.text = element_text(size=7)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") 