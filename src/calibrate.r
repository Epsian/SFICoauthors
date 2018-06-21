# Loads the empirical coauthorship network from Newman (2004) and then
# finds optimal parameter settings to make our network as close as possible
# to this one. Note: we're using the Physics coauthorship network
source('src/jj_model_dynamic.r')

start_time <- Sys.time()
run_model(num_authors = 1000)
end_time <- Sys.time()
print(end_time - start_time)
stop()

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

compute_avg_degree <- function(graph){
    graph_ddist <- degree_distribution(graph)
    max_degrees <- length(graph_ddist) - 1
    graph_avg_deg <- weighted.mean(0:max_degrees, graph_ddist)
    return(graph_avg_deg)
}

emp_diff <- function(adj_mat){
    # Computes a difference score between the empirical stats and the
    # generated network (we'll want to minimize this)
    # Transform the 1s->0s then 3s->1s and -1s->0s
    adj_mat[adj_mat==1] <- 0
    adj_mat[adj_mat==3] <- 1
    adj_mat[adj_mat==-1] <- 0
    ig <- graph_from_adjacency_matrix(adj_mat, mode="undirected")
    avg_degree <- compute_avg_degree(ig)
    clust_coef <- transitivity(ig)
    avg_dist <- mean_distance(ig)
    
    # Compute differences
    avgdeg_diff <- abs(avg_degree - astro_avgdeg)
    clust_diff <- abs(clust_coef - astro_clustcoef)
    avgdist_diff <- abs(avg_dist - astro_avgdist)
    
    total_diff <- avgdeg_diff + 3*avgdist_diff + 35*clust_diff
    
    return(total_diff)
}

#result <- run_model()
#result_mats <- result$net_list_matrix
#final_mat <- result_mats[[result$iter]]
##print(final_mat)
#print(emp_sim(final_mat))

# Let's get the decay rate that gets us closest to these stats
decay_rates <- seq(from=0, to=0.2, by=0.01)
print(decay_rates)

diff_scores <- c()
for (cur_decay in decay_rates){
    result <- run_model(thresh_decay = cur_decay, gen_visuals=FALSE)
    # Now compute the similarity of the final network with the empirical stats
    result_mats <- result$net_list_matrix
    final_mat <- result_mats[[result$iter]]
    #print(final_net)
    #print(emp_sim(result$net_list))
    diff_score <- emp_diff(final_mat)
    diff_scores <- c(diff_scores, diff_score)
    print(paste0("cur_decay = ",cur_decay,", diff_score=",diff_score))
}
print(diff_scores)
argmin <- which.min(diff_scores)
print(argmin)
print(diff_scores[argmin])


