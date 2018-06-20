
#### Dependencies ####
library(ndtv)
library(tsna)
library(htmlwidgets)
library(statnet)

#### Setup ####

# How many authors should there be?
num_authors <- 25
# How many iterations will be run?
iter = 20
# 
adj_list <- matrix(0L, nrow=num_authors, ncol=num_authors)
diag(adj_list) <- -1
auth_types <- rbinom(n=num_authors,size=1,prob=0.5)

# How rare should high proficiency in a subtopic be?
.sub_curve = .4

#### Create Node Attributes ####

author_att = data.frame(author_id = 1:num_authors,
                        sub_a = floor(rexp(num_authors, .sub_curve)),
                        sub_b = floor(rexp(num_authors, .sub_curve)),
                        sub_c = floor(rexp(num_authors, .sub_curve)),
                        sub_d = floor(rexp(num_authors, .sub_curve)),
                        sub_e = floor(rexp(num_authors, .sub_curve)))

#### Create Network ####

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
}

#### Convert Networks to Dynamic Networks ####

# Convert matrix to network objects
net_list = lapply(net_list, FUN = function(x) as.network(x, matrix.type="adjacency", directed=FALSE, ignore.eval=FALSE, names.eval="weight"))

# Add vertex attributes
net_list = lapply(net_list, FUN = function(x){
  set.network.attribute(x, "author_id", author_att$author_id)
  set.network.attribute(x, "sub_a", author_att$sub_a)
  set.network.attribute(x, "sub_b", author_att$sub_b)
  set.network.attribute(x, "sub_c", author_att$sub_c)
  set.network.attribute(x, "sub_d", author_att$sub_d)
  set.network.attribute(x, "sub_e", author_att$sub_e)
  })

# Convert to dynamic network
dynet = networkDynamic(network.list = net_list, create.TEAs = TRUE)

#### Render HTML ####

render.d3movie(dynet,
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

#### Static Networks ####

ac_net <- as.network(adj_list, matrix.type="adjacency", directed=FALSE,ignore.eval=FALSE,names.eval="weight")

plot(ac_net, vertex.col=auth_types, edge.col=adj_list, network.layout = "circle")
