Testdist <- as.tibble(Testdist1)

Testdist <- Testdist %>% gather(`low`,`medium`,`high`, key = "level", value ="price")

 qplot(year, price, data=Testdist, geom = "line", color = level) +
   labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
   scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
   scale_y_continuous(breaks = seq(0,1600, by = 100), sec.axis = dup_axis()) +
   theme(legend.title=element_blank()) +
   theme(legend.position="bottom") +
   theme_bw() +
   theme(legend.text = element_text(size=7)) +
   theme(axis.text = element_text(size=7)) +
   theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
   theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) +
   theme(legend.position = c(0.1, 0.8))
   
 