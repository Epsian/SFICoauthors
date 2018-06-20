library(ndtv)
library(tsna)
library(htmlwidgets)

adj_lists_filename <- "adj_lists.txt"
# Clear the file if it already exists
file.remove(adj_lists_filename)
num_authors <- 9
iter = 20
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

net_list = list()

# Time loop
for (t in 1:iter){
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
    net_list[[t]] = print(adj_list)
    write(paste0("\nt = ",t,"\n"), file=adj_lists_filename, append=TRUE)
    write.table(adj_list, file=adj_lists_filename, append=TRUE, row.names=TRUE, col.names=FALSE)
}

# NOW do network stuff
library(network)

net_list = lapply(net_list, FUN = function(x) as.network(x, matrix.type="adjacency", directed=FALSE,ignore.eval=FALSE, names.eval="weight"))

test = networkDynamic(network.list = net_list, create.TEAs = TRUE)

render.d3movie(test,
               edge.lwd = function(slice){
                 lwd = slice%e%"weight"
                 lwd[lwd == 2] = 0
                 return(lwd * 2)
               },
               edge.col = function(slice){
                 col = slice%e%"weight"
                 col[col == 2] = 0
                 return(col)
                 },
               vertex.col = auth_types,
               output.mode = 'htmlWidget')


#ac_net <- as.network(adj_list, matrix.type="adjacency", directed=FALSE,ignore.eval=FALSE,names.eval="weight")

#plot(ac_net, vertex.col=auth_types, edge.col=adj_list, network.layout = "circle")
