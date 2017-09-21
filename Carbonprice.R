#All data - however, DEBIS numbers are so much higher than they others that it's worth trying without

cpriceh <- plyr::rbind.fill(GarnautAUSR, GarnautINTR, PET2017R, WEO2016R, UKDEBIS)

#

cpricehlow <- plyr::rbind.fill(GarnautAUSR, GarnautINTR, PET2017R, WEO2016R)