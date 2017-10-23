qplot(y=existingcp, x= 1, geom = "violin") +
  scale_y_continuous(breaks = seq(0,200, by = 10)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) +
  theme(axis.title.x = element_blank())
 
#following code needs year changed to factor using as.factor, just putting factor(year) alters the original data
#this doesn't happen with ggplot code below
qplot(year, price, data = fin_all_summ_subset2, geom = "boxplot") +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) 
 # theme(axis.text.x = element_text(colour = 'red', angle = 90)

ggplot(fin_all_summ_subset2, aes(x = factor(year), y = price)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) 

ggplot(fin_all_summ_subset2, aes(x = factor(year), y = price)) + 
  geom_violin() + 
  geom_boxplot(width = 0.1) +
  scale_y_continuous(breaks = seq(0,900, by = 25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y =  element_blank()) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y =  element_blank()) 
