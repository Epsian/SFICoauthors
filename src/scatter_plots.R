
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

# Add newman model referance !!!
top_talent = plot_ly(y = c(2, 8, 15), type = "box", color = c("newman", "Model 1")) %>%
  add_trace(y = c(min(author_att$talent), median(author_att$talent), max(author_att$talent)))

#### Create Vector Plots ####
# 








