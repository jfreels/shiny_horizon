### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

# Load libraries
libs<-c("lubridate","plyr","reshape2","ggplot2","xts","PerformanceAnalytics","quantmod","RColorBrewer","devtools")
lapply(libs,require,character.only=TRUE)
install_github(repo="r_jfreels",username="jfreels")
library(jfreels)