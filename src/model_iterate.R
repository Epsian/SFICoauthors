
#### Data Load ####
source("src/jj_model_dynamic.r")

decay_rates = seq(0, .2, by = .01)

# num_authors: How many authors should there be?
# iter: How many iterations will be run?
# init_threshold: What similarity threshold should each author start with?
# thresh_decay: How much should authors "lower their expectations" when they
#               fail to match?
# max_coauthors: Authors stop accepting new links after they have formed this many
# max_rejections: Authors stop "talking" to other authors after this many match failures
# sub_curve: How rare should high proficiency in a subtopic be?

model_list = list()

for(i in 1:length(decay_rates)){
  model_list[[i]] = run_model(thresh_decay = decay_rates[i])
  
}

saveRDS(model_list, "data/decay_list.rds")






