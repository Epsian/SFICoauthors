
#### Data Load ####

library(plotly)
model_metrics = readRDS("data/model_metrics.rds")

#### Create Network Plots ####
# Line graph x = decay, y = betweenness, by min, median, max betweenness centrality

decay_par = seq(0, .2, by = .01)
min_bet = as.numeric(lapply(model_metrics, FUN = function(x){
    att = x$author_att
    out = att$nbet[which(att$talent == min(att$talent))]
}))
med_bet = as.numeric(lapply(model_metrics, FUN = function(x){
    att = x$author_att
    out = att$nbet[which(att$talent == median(att$talent))]
}))
max_bet = as.numeric(lapply(model_metrics, FUN = function(x){
    att = x$author_att
    out = att$nbet[which(att$talent == max(att$talent))]
}))

vline <- function(x = 0, color = "red") {
    list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
    )
}

decay_line <- plot_ly(x = decay_par, y = min_bet, type = 'scatter', mode = 'lines', name = "Minimum") %>%
    add_trace(y = med_bet, name = "Median", mode = "lines") %>%
    add_trace(y = max_bet, name = "Max", mode = "lines") %>%
    layout(title = "Lowering Your Standards", xaxis = list(title = "Decay Parameter", yaxis = list(title = "Betweenness Centrality")), shapes = list(vline(.09)))


















# 

#netplot = plot_ly(gstats, x = colnames(gstats), y = 0, type = 'bar', name = 'Newman Model') %>%
  #add_trace(y = as.numeric(gstats[iter, ]), name = 'Model 1') %>%
  #layout(yaxis = list(title = 'Count'), barmode = 'group')

# Add newman model referance !!!
#top_talent = plot_ly(y = c(2, 8, 15), type = "box", color = c("newman", "Model 1")) %>%
  #add_trace(y = c(min(author_att$talent), median(author_att$talent), max(author_att$talent)))

#### Create Vector Plots ####
# 








