num_iterations <- 20

adj_lists <- list()

num_authors <- 10
adj_list <- matrix(0L, nrow=num_authors, ncol=num_authors)
diag(adj_list) <- -1
auth_types <- rbinom(n=num_authors,size=1,prob=0.5)

cols_without_1 <- function(alist){
    num_cols <- dim(alist)[2]
    # List of cols
    to_return <- vector('integer')
    for (cur_col in 1:num_cols){
        col_vec <- alist[,cur_col]
        if (all(col_vec != 1)) {
            to_return <- c(to_return, cur_col)
        }
    }
    return(to_return)
}

# Time loop
for (t in 1:num_iterations){
    print(paste0("****** Time t=",t))
    # Loop over (shuffled) authors
    shuffled_authors <- sample(1:num_authors)
    for (cur_auth in shuffled_authors){
        print(paste0("*** Resolving 1s for author #", cur_auth))
        # First resolve any 1s
        unresolved <- which(adj_list[cur_auth,] == 1)
        if (length(unresolved) > 0){
            for (unresolved_ind in unresolved){
                # Check the types
                if (auth_types[cur_auth] == auth_types[unresolved_ind]){
                    print(paste0("Author ",cur_auth," and author ",unresolved_ind," SUCCESSFULLY coauthor!"))
                    adj_list[cur_auth, unresolved_ind] <- 3
                    adj_list[unresolved_ind, cur_auth] <- 3
                } else {
                    print(paste0("Author ", cur_auth," and author ", unresolved_ind," FAIL to coauthor"))
                    adj_list[cur_auth, unresolved_ind] <- 2
                    adj_list[unresolved_ind, cur_auth] <- 2
                }
            }
        }
    }
    for (cur_auth in shuffled_authors){
        # Then form new links, IF this author hasn't been chosen yet
        if (any(adj_list[cur_auth,] == 1)) {
            print(paste0("*** Author ",cur_auth," already chosen. No new links."))
        } else {
            print(paste0("*** Forming new links for author ",cur_auth))
            # Candidates must have 0 with cur_auth AND not have a 1 with anyone else!
            havent_talked <- which(adj_list[cur_auth,] == 0)
            not_talking <- cols_without_1(adj_list)
            candidates <- intersect(havent_talked, not_talking)
            print("Candidates:")
            print(candidates)
            # Randomly choose one of the candidates, if any
            if (length(candidates) > 0){
                if (length(candidates) == 1){
                    chosen_cand <- candidates[1]
                } else {
                    chosen_cand <- sample(candidates, 1)
                }
                print(paste0("Author #", cur_auth, " forming link with ",chosen_cand))
                adj_list[cur_auth, chosen_cand] <- 1
                adj_list[chosen_cand, cur_auth] <- 1
            }
        }
    }
    # End of time step. Add to list
    adj_lists[[t]] <- adj_list
}

# NOW do network stuff
library(network)

ac_net <- as.network(adj_list, matrix.type="adjacency", directed=FALSE,ignore.eval=FALSE,names.eval="weight")

plot(ac_net, vertex.col=auth_types, edge.col=adj_list)

