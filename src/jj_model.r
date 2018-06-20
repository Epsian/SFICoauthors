# Modeling co-authorship!

# Initialize the network, no connections yet
num_authors <- 3
academia <- network.initialize(num_authors, directed = FALSE)
network.vertex.names(academia) <- paste0("n",network.vertex.names(academia))
print(network.vertex.names(academia))

for (rownum in 1:num_authors){
    academia[rownum,rownum] <- -1
}

# Time loop
for (t in 1:10) {
    # Loop over agents
    # For sanity, I'm using the author's index as the unique id throughout
    author_names <- academia%v%"vertex.names"
    # Shuffle the order (to avoid agent being "stuck" if
    # odd number of agents)
    # According to https://stackoverflow.com/questions/13765972/how-to-randomize-a-vector
    # it's just sample(X) to shuffle X
    shuffled_authors <- sample(author_names)
    #print(all_author_indices)
    for (cur_auth in shuffled_authors){
        print(paste0("Processing author ", cur_auth))
        ### 1. look at all the *other* agents that cur_author
        # is *not* connected to already
        candidates <- author_names
        # Remove cur_author from candidates (no linking to self)
        candidates <- candidates[-which(candidates==cur_auth)]
        # Find candidates author is not already connected to
        print(which(academia[cur_auth,candidates] == 1))
        cur_auth_edges <- academia[cur_auth_index,]
        connected_indices <- which(cur_auth_edges > 0)
        print(connected_indices)
        candidates <- candidates[!candidates %in% connected_indices]
        print(candidates)
        print(length(candidates))
        # Randomly choose from the remaining candidates, if any
        if (length(candidates) > 0) {
            chosen_cand <- sample(candidates, 1)
            print(paste0("Author ", cur_auth_index, " linking with ", chosen_cand))
            add.edge(academia, cur_auth_index, chosen_cand, names.eval="weight", )
        }
        # Now set their edge to have weight 1 (corresponding to "discussing")
        ### 2. if there's an edge with weight 1, either "confirm" or "fail" that edge
        #if(any(academia))
        
    }
}
#add.edge()