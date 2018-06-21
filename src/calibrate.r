# Loads the empirical coauthorship network from Newman (2004) and then
# finds optimal parameter settings to make our network as close as possible
# to this one. Note: we're using the Physics coauthorship network

library(igraph)
# Load Newman data
#astro_graph <- read_graph("data/astro-ph.gml", format="gml")

# Average degree
#astro_ddist <- degree_distribution(astro_graph)
#max_degrees <- length(astro_ddist) - 1
#astro_avgdeg <- weighted.mean(0:max_degrees, astro_ddist)
#print(astro_avgdeg)
astro_avgdeg <- 14.51586

# Clustering coefficient
#astro_clustcoef <- transitivity(astro_graph)
#print(astro_clustcoef)
astro_clustcoef <- 0.4258896

# Assortativity
#astro_assort <- assortativity.degree(astro_graph)
#print(astro_assort)

# Average distance
#astro_avgdist <- mean_distance(astro_graph)
#print(astro_avgdist)
astro_avgdist <- 4.79796



# This is for full physics dataset :(
#emp_num_authors <- 52909
#emp_avg_coauthors <- 9.7
#emp_largest_component <- 0.85
#emp_avg_dist <- 5.9
#emp_largest_dist <- 20
#emp_clustering_coeff <- 0.43
#emp_assortativity <- 0.36

emp_sim <- function(adj_mat){
    # Computes a similarity score between the empirical stats and the
    # generated network
    # 1. compute avg_coauthors from adj_mat
    
    return(NULL)
}

source('src/jj_model_dynamic.r')

# Let's get the decay rate that gets us closest to these stats
#decay_rates <- seq(from=0, to=1, by=0.1)
#print(decay_rates)

decay_rates <- 0.5

for (cur_decay in decay_rates){
    result <- run_model(thresh_decay = cur_decay)
    # Now compute the similarity of the final network with the empirical stats
    all_nets <- result$net_list_matrix
    print(all_nets)
    final_net <- all_nets[-1]
    print(final_net)
    #print(emp_sim(result$net_list))
}

