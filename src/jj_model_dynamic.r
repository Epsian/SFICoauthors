
#### Dependencies ####
library(ndtv)
library(tsna)
library(htmlwidgets)
library(statnet)

source('src/accept_reject.r')
source('src/entropy_calc.r')
source('src/choose_partner.r')

run_model <- function(num_authors=9, iter=50, init_threshold=0.75, thresh_decay=0.08, max_coauthors=3, max_rejections=3, sub_curve=0.4, output_log=FALSE, return_sim_mat=FALSE, gen_visuals=TRUE, matrix_only=FALSE){

#### Setup ####
set.seed(43) # generates 3 triangles corresponding to 3 separate
             # interest cliques (with default settings)

# num_authors: How many authors should there be?
# iter: How many iterations will be run?
# init_threshold: What similarity threshold should each author start with?
# thresh_decay: How much should authors "lower their expectations" when they
#               fail to match?
# max_coauthors: Authors stop accepting new links after they have formed this many
# max_rejections: Authors stop "talking" to other authors after this many match failures
# sub_curve: How rare should high proficiency in a subtopic be?

# Where to output the time-sliced adjacency matrices
adj_mats_filename <- "adj_mats.txt"

# Delete if it already exists
if (output_log){
    if (file.exists(adj_mats_filename)){
        file.remove(adj_mats_filename)
    }
}

# Initialize the adjacency matrix
adj_mat <- matrix(0L, nrow=num_authors, ncol=num_authors)

# Sets all diagonal elements to -1 so no agent ever matches
# with themselves
diag(adj_mat) <- -1

# Initialize the types of the authors (0 or 1)
#auth_types <- rbinom(n=num_authors,size=1,prob=0.5)

# Initialize the authors' thresholds (all start at 0.5)
thresholds <- rep(init_threshold, num_authors)

# Matrix where M_{i,j} = number of times j rejected i
rejection_mat <- matrix(0, nrow=num_authors, ncol=num_authors)

#### Create Node Interests ####

author_att = data.frame(author_id = 1:num_authors,
                        sub_a = floor(rexp(num_authors, sub_curve)),
                        sub_b = floor(rexp(num_authors, sub_curve)),
                        sub_c = floor(rexp(num_authors, sub_curve)),
                        sub_d = floor(rexp(num_authors, sub_curve)),
                        sub_e = floor(rexp(num_authors, sub_curve)))
# Convert to matrix, rows are authors columns are topics
interest_mat <- data.matrix(author_att)
# And delete the author_num column
interest_mat <- interest_mat[,-1]

# Now we can compute (a) entropies, (b) talent levels, and
# (c) talent-weighted entropies for each author
author_ents <- apply(interest_mat, 1, entropy)
author_talent <- apply(interest_mat, 1, sum)
author_weighted_ents <- author_ents * author_talent

if (return_sim_mat){
    # And a pairwise similarity matrix
    #print("Computing similarities")
    first_sims <- JS_Sim_Vector(interest_mat[1,],interest_mat[,])
    sim_matrix <- matrix(0, nrow=num_authors, ncol=num_authors)
    for (cur_author in 1:num_authors){
        sim_matrix[cur_author,] <- JS_Sim_Vector(interest_mat[cur_author,],interest_mat[,])
    }
    #sim_mat <- apply(interest_mat, 1, function(z) JSVector(z))
}

#### Create Network ####

cols_without_1 <- function(amat){
    # Takes in an adjacency matrix and returns the indices
    # of all columns without any 1s (meaning, all agents
    # who are not already talking to someone)
    num_cols <- dim(amat)[2]
    # List of cols
    to_return <- vector('integer')
    for (cur_col in 1:num_cols){
        col_vec <- amat[,cur_col]
        if (all(col_vec != 1)) {
            to_return <- c(to_return, cur_col)
        }
    }
    return(to_return)
}

set_value <- function(mat, rownum, colnum, value){
    # Since all the matrices we're working with are symmetric (besides the
    # interest matrix which is just appended vectors), this ensures that
    # the matrix never becomes asymmetric...
    mat[rownum,colnum] <- value
    mat[colnum,rownum] <- value
    return(mat)
}

increment_value <- function(mat, rownum, colnum){
    mat[rownum,colnum] <- mat[rownum,colnum] + 1
    mat[colnum,rownum] <- mat[colnum,rownum] + 1
    return(mat)
}

deterministic_match <- function(cur_auth, other_auth, auth_types, adj_mat){
    # Check the types
    if (auth_types[cur_auth] == auth_types[other_auth]){
        #print(paste0("Author ",cur_auth," and author ",other_auth," SUCCESSFULLY coauthor!"))
        return(TRUE)
        
    } else {
        #print(paste0("Author ", cur_auth," and author ", other_auth," FAIL to coauthor"))
        return(FALSE)
    }
}

threshold_match <- function(cur_auth, other_auth, interest_mat, thresholds){
    # Get interest vectors
    #print("interest_mat")
    #print(interest_mat)
    cur_interest <- interest_mat[cur_auth,]
    other_interest <- interest_mat[other_auth,]
    # Get threshold
    cur_threshold <- thresholds[cur_auth]
    return(accept_link(cur_interest, other_interest, cur_threshold))
}

net_list = list()

# Time loop
for (t in 1:iter){
    #print(paste0("****** Time t=",t))
    # Loop over (shuffled) authors
    shuffled_authors <- sample(1:num_authors)
    for (cur_auth in shuffled_authors){
        #print(paste0("*** Resolving 1s for author #", cur_auth))
        # First resolve any 1s
        unresolved <- which(adj_mat[cur_auth,] == 1)
        if (length(unresolved) > 0){
            for (unresolved_ind in unresolved){
                # Here we can use different matching rules
                #matched <- deterministic_match(cur_auth, unresolved_ind, auth_types, adj_mat)
                matched <- threshold_match(cur_auth, unresolved_ind, interest_mat, thresholds)
                if (matched){
                    adj_mat <- set_value(adj_mat, cur_auth, unresolved_ind, 3)
                    # set_value does the same thing as these two lines:
                    #adj_mat[cur_auth, unresolved_ind] <- 3
                    #adj_mat[unresolved_ind, cur_auth] <- 3
                } else {
                    # New 2018-06-20: this just resets back to 0, so that
                    # authors can match again later if they have "lowered
                    # their standards"
                    adj_mat <- set_value(adj_mat, cur_auth, unresolved_ind, 0)
                    # But update the rejection matrix, so that we don't get
                    # infinite rejections
                    rejection_mat <- increment_value(rejection_mat, cur_auth, unresolved_ind)
                    thresholds[cur_auth] <- thresholds[cur_auth]*(1-thresh_decay)
                }
            }
        }
    }
    for (cur_auth in shuffled_authors){
        # Then form new links, IF this author hasn't been chosen yet
        if (any(adj_mat[cur_auth,] == 1)) {
            #print(paste0("*** Author ",cur_auth," already chosen. No new links."))
            1+1
        } else {
            #print(paste0("*** Forming new links for author ",cur_auth))
            # Candidates must have 0 with cur_auth AND not have a 1 with anyone else!
            havent_talked <- which(adj_mat[cur_auth,] == 0)
            not_talking <- cols_without_1(adj_mat)
            few_rejections <- which(rejection_mat[cur_auth,] < max_rejections)
            candidates <- intersect(intersect(havent_talked, not_talking), few_rejections)
            #print("Candidates:")
            #print(candidates)
            # Randomly choose one of the candidates, if any
            if (length(candidates) > 0){
                if (length(candidates) == 1){
                    chosen_cand <- candidates[1]
                } else {
                    chosen_cand <- sample(candidates, 1)
                }
                #print(paste0("Author #", cur_auth, " forming link with ",chosen_cand))
                adj_mat <- set_value(adj_mat, cur_auth, chosen_cand, 1)
            } 
            #else {
                # No candidates this round. They get impatient and lower their threshold
                #thresholds[cur_auth] <- thresholds[cur_auth] * thresh_decay
            #}
        }
    }
    net_list[[t]] = adj_mat
    if (output_log){
        write(paste0("\nt = ",t,"\n"), file=adj_mats_filename, append=TRUE)
        write.table(adj_mat, file=adj_mats_filename, append=TRUE, row.names=TRUE, col.names=FALSE)
    }
} # end time loop

#### Convert Networks to Dynamic Networks ####

# Convert matrix to network objects
net_list_matrix = net_list
if (!matrix_only){
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
} else {
    net_list <- NULL
    dynet <- NULL
}

if (gen_visuals){
    wid = render.d3movie(dynet,
               vertex.cex = 3,
               edge.lwd = 3,
               edge.col = function(slice){
                   col = slice%e%"weight"
                   col[col == 3] = "green"
                   col[col == 1] = "grey"
                   return(col)
               },
               vertex.col = "blue",
               vertex.tooltip = paste('Subtopic A:', author_att$sub_a, '<br>',
                                      "Subtopic B:", author_att$sub_b, "<br>",
                                      "Subtopic C:", author_att$sub_c, "<br>",
                                      "Subtopic D:", author_att$sub_d, "<br>",
                                      "Subtopic E:", author_att$sub_e, "<br>",
                                      "Talent Weighted Diversity:", round(author_weighted_ents, digits = 2), "<br>"),
               output.mode = 'htmlWidget')
} else {
    wid <- NULL
}

#### Static Networks ####

#ac_net <- as.network(adj_list, matrix.type="adjacency", directed=FALSE,ignore.eval=FALSE,names.eval="weight")

#plot(ac_net, vertex.col=auth_types, edge.col=adj_list, network.layout = "circle")

return_list = list("net_list_matrix"=net_list_matrix, "net_list"=net_list, "dynet"=dynet, "wid"=wid, "author_weighted_ents"=author_weighted_ents, "author_att"=author_att, "iter"=iter)
if (return_sim_mat){
    return_list["sim_mat"] <- sim_matrix
}
return(return_list)

} # end run_model