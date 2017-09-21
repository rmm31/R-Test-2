#Code for charts for paper - i.e. missing prices etc.

  qplot(year, price, data=AR5, geom = "line", color = ModelScenario) + 
  labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(breaks = seq(0,1600, by = 100)) +
  theme(legend.title=element_blank()) +
    theme_bw () +
    theme(legend.title=element_blank()) +
    theme(legend.position="bottom") +
    theme(legend.text = element_text(size=7)) +
    theme(axis.text = element_text(size=7)) +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) +
    theme(legend.title=element_blank())
  
  ggplot(AR5, aes(year, price)) +
    stat_summary(fun.data = "iqr", geom = "ribbon", fill = alpha("blue", 1/5)) +
    stat_summary(aes(year), fun.y = IQmean, geom = "line", color = "blue") +
    stat_summary(aes(year), fun.y = low25, geom = "line", color = "green") +
    stat_summary(aes(year), fun.y = high65, geom = "line", color = "red") +
    labs(x = "Year", y = bquote('2016 AUD/t'~CO[2]~'-e')) +
    scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
    theme_bw () +
  #  theme(legend.position="bottom") +
  #  theme(legend.text = element_text(size=7)) +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
    theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y=element_blank()) +
    theme(axis.text.x = element_text(size = 10))
    
  
  #function for low and high lines
  
  low35 <- fIQmean <- function(x, ...) {md1 <- quantile(as.numeric(x), c(0.35), na.rm = TRUE)
  md1
  } 
  
  high65 <- fIQmean <- function(x, ...) {md1 <- quantile(as.numeric(x), c(0.65), na.rm = TRUE)
  md1
  } 
