KL_Divergence <- function(p,q){
    # Computes the Kullback-Leibler divergence between p and q
    div <- p*log2(p/q)
    div[is.nan(div)] <- 0
    return(sum(div))
}

JS_Distance <- function(p,q){
    # Computes the Jensen-Shannon distance between p and q
    # Normalize the frequencies to create a probability distribution
    p_norm <- p/sum(p)
    q_norm <- q/sum(q)
    m <- 0.5*(p_norm + q_norm)
    divergence <- 0.5*KL_Divergence(p_norm, m) + 0.5*KL_Divergence(q_norm, m)
    return(sqrt(divergence))
}

JS_Vector <- function(p,q_mat){
    # Computes a vector of Jensen-Shannon distances between p
    # and the rows of q_mat
    js_vec <- apply(q_mat, 1, function(z) JS_Distance(p,z))
    return(js_vec)
}

JS_Sim_Vector <- function(p,q_mat){
    js_sim_vec <- apply(q_mat, 1, function(z) 1-JS_Distance(p,z))
    return(js_sim_vec)
}

choose_partner <- function(node_num, candidates, talent_matrix){
    # Choose a node from among the candidates with probability
    # proportional to the Jensen-Shannon distance to that node
    # Step 1: JS distances
    cur_talents <- talent_matrix[node_num,]
    candidate_talents <- talent_matrix[candidates,]
    distances <- JSVector(cur_talents, candidate_talents)
    # Step 2: P(choose as partner) inversely proportional to distance
    sims <- 1 - distances
    sims_norm <- sims/sum(sims)
    #print(sims_norm)
    chosen <- sample(candidates, 1, prob=sims_norm)
    #print(chosen)
    return(chosen)
}

# Testing
test_distances <- function(){
    talents <- matrix(c(10,1,0,0,0,11,0,1,11,0,1,0), nrow=4, ncol=3, byrow=TRUE)
    node_num <- 1
    candidates <- c(2,3,4)
    choose_partner(node_num, candidates, talents)
    JSDistance(c(10,1,0),c(0,0,11))
}