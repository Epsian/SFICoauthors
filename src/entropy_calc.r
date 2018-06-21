entropy <- function(p){
    p_norm <- p/sum(p)
    expected_vals <- p_norm*log2(p_norm)
    expected_vals[is.na(expected_vals)] <- 0
    return(-sum(expected_vals))
}

# Testing
test_entropy <- function(){
    p = c(0.2,0.2,0.2,0.2,0.2)
    print(entropy(p))
    q = c(1,0,0,0,1)
    print(entropy(q))
}