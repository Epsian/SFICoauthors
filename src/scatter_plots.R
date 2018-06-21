
#### Data Load ####

library(ggplot2)
library(plotly)
net1 = read.csv("")
net2 = read.csv("")

gstats = gstats[,-1]

#### Create Network Plots ####
# 

netplot = plot_ly(gstats, x = colnames(gstats), y = 0, type = 'bar', name = 'Newman Model') %>%
  add_trace(y = as.numeric(gstats[iter, ]), name = 'Model 1') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

#### Create Vector Plots ####
# 








