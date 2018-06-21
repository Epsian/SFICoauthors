source('src/choose_partner.r')

accept_link <- function(my_interest, other_interest, my_threshold){
    interest_sim <- JS_Sim(my_interest, other_interest)
    return(interest_sim >= my_threshold)
}