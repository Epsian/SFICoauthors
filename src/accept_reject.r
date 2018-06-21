source('choose_partner.r')

accept_link <- function(my_interest, other_interest, my_threshold){
    interest_dist <- JSDistance(my_interest, other_interest)
    return(interest_dist >= my_threshold)
}