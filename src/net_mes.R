
#### Data Load ####

library(keyplayer)
source("src/jj_model_dynamic.r")

#### Setup ####

last_net = net_list[[length(net_list)]]
.out_dir = "data/"

#### Remove Potential Ties From Dynet ####

net_list_matrix_c = lappy(net_list_matrix, FUN = function(x){
  x[x == 1] = 0
  x[x == 2] = 0
  x[x == -1] = 0
  x[x == 3] = 1
  return(x)
})

#### Graph Level ####

gstats = data.frame(time = 1:length(net_list + 1),
                    trans = tSnaStats(net_list_matrix_c, snafun='gtrans'),
                    avg_degree = tSnaStats()
                    )

test = apply(tSnaStats(dynet, snafun='betweenness')[1:length(net_list_matrix_c),], 1, mean)

/((num_authors-1)*(num_authors-2)/2)

#### Vertex Level ####

author_att$talent = 
author_att$degree = degree(last_net, gmode="graph")
author_att$kcore = kcores(last_net, mode="graph", diag= FALSE)
author_att$avg_geo = (colSums(geodist(last_net)$gdist))/(nrow(as.sociomatrix(last_net)) - 1)
author_att$nbet = (betweenness(last_net, gmode="graph", cmode="undirected")/((num_authors-1)*(num_authors-2)/2))
author_att$evc = evcent(last_net, gmode = "graph", rescale = FALSE)
author_att$frag = fragment(as.matrix.network.adjacency(last_net))

#### Save ####







