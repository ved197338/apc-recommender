# Experiment — Baseline algorithm comparison (APC vs CF/MF/Popularity)

###############################################################################

# set working directory to project root
# (you might need to adjust this depending on how you run the script)
setwd(dirname(dirname(sys.frame(1)$ofile)))

source("R/utils.R")
source("R/codes.R")
source("R/preference_state.R")
source("R/interaction_weights.R")
source("R/decay.R")
source("R/momentum.R")
source("R/compound_codes.R")
source("R/hierarchy.R")
source("R/scoring.R")
source("R/exploration.R")
source("R/simulator.R")
source("R/evaluation.R")

# EXPERIMENT SETUP

set_experiment_seed(42, "baseline comparison experiment")

cat("\n")
cat("  APC Baseline Comparison Experiment\n")
cat("\n\n")

# Step 1: generate synthetic catalogue
cat("generating synthetic catalogue...\n")
catalogue <- generate_synthetic_catalogue(n_items = 500, seed = 42)
cat(sprintf("  catalogue: %d items, %d dimensions\n", 
            catalogue$n_items, length(catalogue$all_dimensions)))

# Step 2: create simulated users
cat("\ncreating simulated user population...\n")
sim_users <- create_sim_user_population(n_users = 30, seed = 42)
cat(sprintf("  created %d simulated users\n", length(sim_users)))

# Step 3: run simulation
cat("\nrunning behaviour simulation (90 days)...\n")
sim_results <- run_population_simulation(
  sim_users, catalogue, 
  n_days = 90, 
  items_per_day = 12,
  seed = 42,
  verbose = TRUE
)

# collect all interactions
all_interactions <- list()
for (uid in names(sim_results)) {
  all_interactions <- c(all_interactions, sim_results[[uid]]$interactions)
}
cat(sprintf("\ntotal interactions: %d\n", length(all_interactions)))

# Step 4: train/test split
# use first 70 days for training, last 20 for testing
# (days 71-90)
cat("\nsplitting data: train (days 1-70), test (days 71-90)...\n")

base_timestamp <- min(sapply(all_interactions, function(i) i$timestamp))
split_timestamp <- base_timestamp + 70 * 86400

train_interactions <- Filter(function(i) i$timestamp < split_timestamp, 
                              all_interactions)
test_interactions <- Filter(function(i) i$timestamp >= split_timestamp,
                             all_interactions)

cat(sprintf("  train: %d interactions\n", length(train_interactions)))
cat(sprintf("  test:  %d interactions\n", length(test_interactions)))

# TRAIN EACH ALGORITHM

registry <- create_code_registry()
hierarchy <- create_default_hierarchy()
decay_config <- get_decay_config()
weight_config <- get_interaction_config()

# Apc
cat("\ntraining APC...\n")
state_manager <- create_state_manager()
state_manager <- process_all_interactions(
  state_manager, train_interactions, catalogue,
  decay_config = decay_config,
  weight_config = weight_config,
  registry = registry,
  verbose = TRUE
)

# compute momentum and compound codes for each user
apc_momentum <- list()
apc_compounds <- list()
for (uid in names(state_manager$users)) {
  state <- state_manager$users[[uid]]
  apc_momentum[[uid]] <- compute_user_momentum(state)
  user_ints <- Filter(function(i) i$user_id == uid, train_interactions)
  apc_compounds[[uid]] <- detect_compound_codes(user_ints, catalogue)
}

# APC recommendation function
apc_recommend <- function(user_state, catalogue_arg) {
  uid <- user_state$user_id
  mom <- apc_momentum[[uid]]
  comp <- apc_compounds[[uid]]
  
  score_all_items(
    user_state, catalogue_arg,
    compounds = comp,
    momentum = mom,
    hierarchy = hierarchy,
    config = get_scoring_config()
  )
}

# Popularity baseline
cat("training popularity baseline...\n")
pop_recommend <- popularity_baseline(train_interactions, catalogue)

# Content-based cosine
cat("training content-based cosine...\n")
cb_recommend <- content_based_cosine(k = 50)

# User-based cf
cat("training user-based CF...\n")
cf_recommend <- user_cf(state_manager$users, k = 50)

# Matrix factorisation
cat("training matrix factorisation...\n")
user_ids <- names(state_manager$users)
item_ids <- names(catalogue$items)
interaction_matrix <- build_interaction_matrix(
  train_interactions, user_ids, item_ids
)
mf_result <- matrix_factorisation(
  interaction_matrix, n_factors = 20,
  n_iterations = 50, catalogue = catalogue
)
mf_recommend <- mf_result$recommend_fn
cat(sprintf("  MF final RMSE: %.4f\n", mf_result$final_rmse))

# EVALUATE

cat("\n\n")
cat("  EVALUATION\n")
cat("\n\n")

k_values <- c(5, 10, 20)

cat("evaluating APC...\n")
apc_results <- evaluate_algorithm(
  apc_recommend, state_manager$users, test_interactions,
  catalogue, k_values = k_values, verbose = TRUE
)

cat("evaluating popularity baseline...\n")
pop_results <- evaluate_algorithm(
  pop_recommend, state_manager$users, test_interactions,
  catalogue, k_values = k_values
)

cat("evaluating content-based cosine...\n")
cb_results <- evaluate_algorithm(
  cb_recommend, state_manager$users, test_interactions,
  catalogue, k_values = k_values
)

cat("evaluating user-based CF...\n")
cf_results <- evaluate_algorithm(
  cf_recommend, state_manager$users, test_interactions,
  catalogue, k_values = k_values
)

cat("evaluating matrix factorisation...\n")
mf_results <- evaluate_algorithm(
  mf_recommend, state_manager$users, test_interactions,
  catalogue, k_values = k_values
)

# RESULTS

cat("\n\n")
cat("  RESULTS\n")
cat("\n")

print_evaluation_results(pop_results, "Popularity Baseline")
print_evaluation_results(cb_results, "Content-Based Cosine")
print_evaluation_results(cf_results, "User-Based CF")
print_evaluation_results(mf_results, "Matrix Factorisation")
print_evaluation_results(apc_results, "APC (Full)")

all_results <- list(
  "Popularity" = pop_results,
  "Content-Based" = cb_results,
  "User CF" = cf_results,
  "Matrix Fact." = mf_results,
  "APC" = apc_results
)

compare_algorithms(all_results)

# Save plots
cat("\ngenerating comparison plots...\n")

plot_algorithm_comparison(
  all_results,
  metrics = c("ndcg", "precision", "recall", "diversity"),
  output_file = "results/baseline_comparison.png"
)

# Save numeric results
results_df <- do.call(rbind, lapply(names(all_results), function(name) {
  r <- all_results[[name]]
  r$algorithm <- name
  r
}))

write.csv(results_df, "results/baseline_comparison.csv", row.names = FALSE)
cat("results saved to results/baseline_comparison.csv\n")

cat("\n\n")
cat("  EXPERIMENT COMPLETE\n")
cat("\n")
