library(tidyverse)

fin_AR5_lower_PPP <- as.tibble(fin_AR5_lower_PPP)
fin_AR5_lower_PPP <- fin_AR5_lower_PPP %>% gather(`2020`,`2030`, `2040`, `2050`, key = "year", value ="price")
fin_AR5_lower_PPP$year <- as.numeric(fin_AR5_lower_PPP$year)

fin_AR5_subset_ppp$year<-as.numeric(fin_AR5_subset_ppp$year)

fin_all_summ <- bind_rows(fin_corridors_ppp, fin_CPLF_ppp, fin_DEBIS_2017_PPP, fin_IEA_2016_ppp, fin_IEA_IRENA_2017_PPP, fin_LPF_CPI, fin_quinet_PPP, fin_SGLP_CPI, fin_US_EPA_PPP)

fin_all_summ[4:5]<- NULL

fin_all_summ_subset <- filter(fin_all_summ, year %in% c(2010, 2020, 2030, 2040, 2050))

all_AR5 <- all_AR5 %>% gather(`2020`,`2030`, `2040`, `2050`, key = "year", value ="price")

all_AR5_2016 <- mutate(all_AR5, price = price * 1.309473333 * 99.58627679/75.09789593)

numbers_report_fin_Market_GDP <- as.tibble(numbers_report_fin_Market_GDP)
numbers_report_fin_Market_GDP <- numbers_report_fin_Market_GDP %>% gather(`2020`,`2030`, `2040`, `2050`, key = "year", value ="price")
numbers_report_fin_Market_GDP$year <- as.numeric(numbers_report_fin_Market_GDP$year)