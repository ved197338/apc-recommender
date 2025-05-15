# Experiment — Compound code detection method evaluation

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
source("R/simulator.R")
source("R/evaluation.R")

# SETUP

set_experiment_seed(789, "compound code comparison")

cat("\n")
cat("  Compound Code Detection Method Comparison\n")
cat("\n\n")

catalogue <- generate_synthetic_catalogue(n_items = 500, seed = 42)
sim_users <- create_sim_user_population(n_users = 20, seed = 42)

cat("running simulation...\n")
sim_results <- run_population_simulation(
  sim_users, catalogue,
  n_days = 80,
  items_per_day = 12,
  seed = 42,
  verbose = TRUE
)

# collect and split interactions
all_ints <- list()
for (uid in names(sim_results)) {
  all_ints <- c(all_ints, sim_results[[uid]]$interactions)
}

base_ts <- min(sapply(all_ints, function(i) i$timestamp))
split_ts <- base_ts + 60 * 86400
train_ints <- Filter(function(i) i$timestamp < split_ts, all_ints)
test_ints <- Filter(function(i) i$timestamp >= split_ts, all_ints)

# train APC state
state_mgr <- create_state_manager()
state_mgr <- process_all_interactions(
  state_mgr, train_ints, catalogue,
  verbose = FALSE
)

# COMPARE DETECTION METHODS

methods <- c("frequency", "mi", "lift", "chi2")
hierarchy <- create_default_hierarchy()

cat("\ndetecting compound codes with each method...\n\n")

all_compounds <- list()
method_stats <- data.frame(
  method = character(0),
  n_compounds = integer(0),
  avg_strength = numeric(0),
  max_strength = numeric(0),
  stringsAsFactors = FALSE
)

for (method in methods) {
  cat(sprintf("  method: %s\n", method))
  
  config <- get_compound_config()
  config$detection_method <- method
  
  method_compounds <- list()
  
  for (uid in names(state_mgr$users)) {
    user_ints <- Filter(function(i) i$user_id == uid, train_ints)
    compounds <- detect_compound_codes(user_ints, catalogue, config)
    method_compounds[[uid]] <- compounds
    
    if (length(compounds) > 0) {
      strengths <- sapply(compounds, function(c) c$strength)
      method_stats <- rbind(method_stats, data.frame(
        method = method,
        n_compounds = length(compounds),
        avg_strength = mean(strengths),
        max_strength = max(strengths),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  all_compounds[[method]] <- method_compounds
  
  # show some example compounds
  example_uid <- names(method_compounds)[1]
  if (length(method_compounds[[example_uid]]) > 0) {
    cat(sprintf("    example from %s:\n", example_uid))
    comps <- method_compounds[[example_uid]]
    sorted <- comps[order(sapply(comps, function(c) -c$strength))]
    for (j in seq_len(min(5, length(sorted)))) {
      cat(sprintf("      %s (strength=%.4f, count=%d)\n",
                  names(sorted)[j], sorted[[j]]$strength, sorted[[j]]$count))
    }
  }
  cat("\n")
}

# method statistics
cat("\nMethod Statistics\n\n")
agg_stats <- aggregate(cbind(n_compounds, avg_strength, max_strength) ~ method,
                        data = method_stats, FUN = mean)
print(agg_stats)

# EVALUATE IMPACT ON RECOMMENDATIONS

cat("\n\nevaluating recommendation impact...\n")

# compute momentum for all users
all_momentum <- list()
for (uid in names(state_mgr$users)) {
  all_momentum[[uid]] <- compute_user_momentum(state_mgr$users[[uid]])
}

# evaluate each method
method_results <- list()

for (method in methods) {
  cat(sprintf("  evaluating: %s\n", method))
  
  recommend_fn <- function(user_state, cat_arg) {
    uid <- user_state$user_id
    score_all_items(user_state, cat_arg,
                    compounds = all_compounds[[method]][[uid]],
                    momentum = all_momentum[[uid]],
                    hierarchy = hierarchy)
  }
  
  results <- evaluate_algorithm(
    recommend_fn, state_mgr$users, test_ints,
    catalogue, k_values = c(5, 10, 20),
    verbose = FALSE
  )
  
  method_results[[method]] <- results
}

# also test without any compounds
cat("  evaluating: no compounds\n")
no_comp_fn <- function(user_state, cat_arg) {
  uid <- user_state$user_id
  score_all_items(user_state, cat_arg,
                  compounds = NULL,
                  momentum = all_momentum[[uid]],
                  hierarchy = hierarchy,
                  config = {
                    cfg <- get_scoring_config()
                    cfg$beta <- 0
                    cfg
                  })
}

method_results[["none"]] <- evaluate_algorithm(
  no_comp_fn, state_mgr$users, test_ints,
  catalogue, k_values = c(5, 10, 20),
  verbose = FALSE
)

# RESULTS

cat("\n\n")
cat("  RESULTS\n")
cat("\n")

compare_algorithms(method_results)

# plot
plot_algorithm_comparison(
  method_results,
  metrics = c("ndcg", "precision", "diversity"),
  output_file = "results/compound_code_comparison.png"
)

# save
results_df <- do.call(rbind, lapply(names(method_results), function(name) {
  r <- method_results[[name]]
  r$method <- name
  r
}))
write.csv(results_df, "results/compound_code_comparison.csv", row.names = FALSE)

cat("\n\n")
cat("  EXPERIMENT COMPLETE\n")
cat("\n")
