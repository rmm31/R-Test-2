library(ggplot2)
library(scales)
theme_set(theme_classic())

#Prepare data: group mean city mileage by manufacturer.
wbpriceorder <- arrange(worldbankcarbonprice, price2017) #order by price
wbpriceorder$name <- factor(wbpriceorder$name, levels = wbpriceorder$name) #retain order in plot


# Plot

ggplot(wbpriceorder, aes(x=name, y=price2017, color = instrument)) + 
  geom_point(size=3) +   # Draw points
  geom_segment(aes(x=name, 
                   xend=name, 
                   y=min(price2017), 
                   yend=max(price2017)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Prices in implemented carbon pricing initiatives (World Bank data)",
       caption="Note: Nominal prices on April, 01 2017 
       Prices are not necessarily comparable between carbon pricing initiatives because of differences 
       in the number of sectors covered and allocation methods applied, specific exemptions, and different 
       compensation methods.", y = bquote('2017 USD/t'~CO[2]~'e'), x = "Price Initiative") +  
  scale_y_continuous(breaks = seq(0,150, by = 10)) +
  coord_flip() 

