#Combine international datasets

intprice <- rbind(UKDEBIS, WEO2016R, PET2017R)

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
allribbon <- ggplot(intprice, aes(year, priceAUD2016)) +
  stat_summary(fun.data = "median_hilow", geom = "ribbon", fill = alpha("green", 1/5)) +
  stat_summary(fun.data = "iqr", geom = "ribbon", fill = alpha("blue", 1/5)) +
  stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "black") +
  stat_summary(aes(year), fun.y = median, geom = "line", color = "red") +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'e or t'~CO[2])) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(breaks = seq(0,800, by = 25)) +
  theme(axis.text = element_text(size=7)) #+
# theme(legend.title=element_blank()) +
# theme(legend.position="bottom")

#Code for faceted selection of charts one for each price series
allfacet <- qplot(year, priceAUD2016, data = intprice, geom = "line", group = source) + facet_wrap(~source, labeller = as_labeller(facet_names_int)) +
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'e or t'~CO[2])) +
  scale_x_continuous(breaks = pretty(intprice$year, n=5)) +
  theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
  theme(axis.text = element_text(size=7)) +
  theme(strip.text.x = element_text(size = 7))
#Code to change names for facets
facet_names_int <- c(
  `DEBIStrhigh` = "DEBIS Traded High",
  `DEBIStrtrcent` = "DEBIS Traded Central",
  `DEBIStrlow` = "DEBIS Traded Low",
  `DEBISuntrhigh` = "DEBIS Untraded High",
  `DEBISuntrcent` = "DEBIS Untraded Central",
  `DEBISuntrlow` = "DEBIS Untraded Low",
  `PET2017R` = "Pathways 2017",
  `WEO2016R` = "WEO 2016"
)
#Code to put faceted charts and ribbon on a single page.
multiplot(Govmod, allfacet, allribbon,cols=1)

#Create table of values to parallel chart

sumdatallfin <- intprice %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    Quan10 = quantile(priceAUD2016, c(0.1), na.rm = TRUE),
    Quan15 = quantile(priceAUD2016, c(0.15), na.rm = TRUE),
    Q1 = quantile(priceAUD2016, c(0.25), na.rm = TRUE),
    IQmean = mean(priceAUD2016, trim = 0.25, na.rm = TRUE),
    Q3 = quantile(priceAUD2016, c(0.75), na.rm = TRUE)
  )


# Code for line chart

intpriceline <-qplot(year, priceAUD2016, data=intprice, geom = "line", color = source) + 
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'e or t'~CO[2])) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(breaks = seq(0,800, by = 50)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text = element_text(size=7)) +
  theme(axis.text = element_text(size=7)) 