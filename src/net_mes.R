
#### Data Load ####

library(keyplayer)
model_list = readRDS("data/decay_lists.rds")

#### Setup ####

# What model do you want stats on?
.modelnum = 21

net_list = model_list[[.modelnum]]$net_list
dynet = model_list[[.modelnum]]$dynet
last_net = net_list[[length(net_list)]]
.author_out = paste0("data/model_metrics/", .modelnum, "_author_att.csv")
.gstats_out = paste0("data/model_metrics/", .modelnum, "_gstats_out.csv")

#### Remove Potential Ties From Dynet ####

net_list_matrix_c = lapply(model_list[[.modelnum]]$net_list_matrix, FUN = function(x){
  x[x == 1] = 0
  x[x == 2] = 0
  x[x == -1] = 0
  x[x == 3] = 1
  return(x)
})

num_authors = dim(net_list_matrix_c[[1]])[1]

#### Graph Level ####

gstats = data.frame(time = 1:(length(net_list_matrix_c) + 1),
                    trans = tSnaStats(dynet, snafun='gtrans'),
                    density = tSnaStats(dynet, snafun = "gden"),
                    avg_degree = apply(tSnaStats(dynet, snafun = "degree", gmode = "graph"), 1, mean),
                    avg_nbet = apply(tSnaStats(dynet, snafun='betweenness'), 1, FUN = function(x) mean(x/((num_authors-1)*(num_authors-2)/2))),
                    avg_evc = apply(tSnaStats(dynet, snafun = "evcent", gmode = "graph", rescale = FALSE), 1, mean)
)

gstats = gstats[1:length(net_list_matrix_c), ]

colnames(gstats) = c("time", "trans", "density", "avg_degree", "avg_nbet", "avg_evc")

#### Vertex Level ####

author_att = model_list[[.modelnum]]$author_att

author_att$talent = model_list[[.modelnum]]$author_weighted_ents
author_att$talent_bin = NA
author_att$degree = degree(last_net, gmode="graph")
author_att$kcore = kcores(last_net, mode="graph", diag= FALSE)
author_att$avg_geo = (colSums(geodist(last_net)$gdist))/(nrow(as.sociomatrix(last_net)) - 1)
author_att$nbet = (betweenness(last_net, gmode="graph", cmode="undirected")/((num_authors-1)*(num_authors-2)/2))
author_att$evc = evcent(last_net, gmode = "graph", rescale = FALSE)
author_att$frag = fragment(as.matrix.network.adjacency(last_net))

#### Save ####

write.csv(author_att, .author_out, row.names = FALSE)
write.csv(gstats, .gstats_out, row.names = FALSE)





