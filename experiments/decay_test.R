# Experiment — Temporal decay configuration comparison

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

set_experiment_seed(321, "decay investigation")

cat("\n")
cat("  Temporal Decay Investigation\n")
cat("\n\n")

catalogue <- generate_synthetic_catalogue(n_items = 400, seed = 42)
sim_users <- create_sim_user_population(n_users = 20, seed = 42)

cat("running simulation...\n")
sim_results <- run_population_simulation(
  sim_users, catalogue,
  n_days = 90,
  items_per_day = 12,
  seed = 42,
  verbose = TRUE
)

all_ints <- list()
for (uid in names(sim_results)) {
  all_ints <- c(all_ints, sim_results[[uid]]$interactions)
}

base_ts <- min(sapply(all_ints, function(i) i$timestamp))
split_ts <- base_ts + 70 * 86400
train_ints <- Filter(function(i) i$timestamp < split_ts, all_ints)
test_ints <- Filter(function(i) i$timestamp >= split_ts, all_ints)

# EXPERIMENT 1: Different uniform decay rates

cat("\nExperiment 1: Uniform decay rates\n\n")

lambdas <- c(0.0, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2)
lambda_results <- list()

for (lambda in lambdas) {
  label <- sprintf("lambda=%.3f", lambda)
  cat(sprintf("  testing %s...\n", label))
  
  config <- get_decay_config()
  config$default_lambda <- lambda
  config$category_lambdas <- lapply(config$category_lambdas, function(x) lambda)
  config$dimension_overrides <- lapply(config$dimension_overrides, function(x) lambda)
  config$adaptive <- FALSE
  
  mgr <- create_state_manager()
  mgr <- process_all_interactions(
    mgr, train_ints, catalogue,
    decay_config = config,
    verbose = FALSE
  )
  
  # evaluate
  rec_fn <- function(state, cat_arg) {
    score_all_items(state, cat_arg)
  }
  
  results <- evaluate_algorithm(
    rec_fn, mgr$users, test_ints,
    catalogue, k_values = c(10),
    verbose = FALSE
  )
  
  lambda_results[[label]] <- results
}

# plot NDCG vs lambda
cat("\nDecay Rate vs NDCG@10\n\n")
for (label in names(lambda_results)) {
  r <- lambda_results[[label]]
  cat(sprintf("  %-15s  NDCG=%.4f  Prec=%.4f  Div=%.4f\n",
              label, r$ndcg[1], r$precision[1], r$diversity[1]))
}

# plot the relationship
png("results/decay_rate_vs_ndcg.png", width = 700, height = 400, res = 100)
ndcg_vals <- sapply(lambda_results, function(r) r$ndcg[1])
plot(lambdas, ndcg_vals, type = "b", pch = 20, col = "#3498db", lwd = 2,
     xlab = "Decay Rate (lambda)", ylab = "NDCG@10",
     main = "Effect of Uniform Decay Rate on NDCG@10",
     bty = "l", las = 1)
grid(col = "gray90")
dev.off()

# EXPERIMENT 2: Uniform vs per-category decay

cat("\nExperiment 2: Uniform vs Per-Category Decay\n\n")

# find the best uniform lambda from experiment 1
best_lambda <- lambdas[which.max(ndcg_vals)]
cat(sprintf("  best uniform lambda: %.3f\n", best_lambda))

# train with uniform
mgr_uniform <- create_state_manager()
uniform_config <- get_decay_config()
uniform_config$default_lambda <- best_lambda
uniform_config$category_lambdas <- lapply(uniform_config$category_lambdas, 
                                           function(x) best_lambda)
uniform_config$dimension_overrides <- list()
uniform_config$adaptive <- FALSE

mgr_uniform <- process_all_interactions(
  mgr_uniform, train_ints, catalogue,
  decay_config = uniform_config,
  verbose = FALSE
)

# train with per-category decay (default config)
mgr_percategory <- create_state_manager()
percategory_config <- get_decay_config()
percategory_config$adaptive <- FALSE

mgr_percategory <- process_all_interactions(
  mgr_percategory, train_ints, catalogue,
  decay_config = percategory_config,
  verbose = FALSE
)

# train with adaptive decay
mgr_adaptive <- create_state_manager()
adaptive_config <- get_decay_config()
adaptive_config$adaptive <- TRUE

mgr_adaptive <- process_all_interactions(
  mgr_adaptive, train_ints, catalogue,
  decay_config = adaptive_config,
  verbose = FALSE
)

# evaluate all three
decay_comparison <- list()
for (setup in list(
  list(name = "Uniform", mgr = mgr_uniform),
  list(name = "Per-Category", mgr = mgr_percategory),
  list(name = "Adaptive", mgr = mgr_adaptive)
)) {
  cat(sprintf("  evaluating: %s\n", setup$name))
  rec_fn <- function(state, cat_arg) {
    score_all_items(state, cat_arg)
  }
  decay_comparison[[setup$name]] <- evaluate_algorithm(
    rec_fn, setup$mgr$users, test_ints,
    catalogue, k_values = c(5, 10, 20),
    verbose = FALSE
  )
}

cat("\n")
compare_algorithms(decay_comparison)

# EXPERIMENT 3: Decay and preference type classification

cat("\nExperiment 3: Preference Type Classification\n\n")

# pick a user and show how their preferences are classified
example_uid <- names(state_mgr_percategory$users)[1]
example_state <- mgr_percategory$users[[example_uid]]

cat(sprintf("  example user: %s\n\n", example_uid))

# build preference history from snapshots
if (length(example_state$history) > 5) {
  pref_history <- lapply(example_state$history, function(s) s$positive_prefs)
  timestamps <- sapply(example_state$history, function(s) s$timestamp)
  
  classifications <- classify_preference_type(pref_history, timestamps)
  
  cat("  preference type classifications:\n")
  for (dim_name in names(classifications)) {
    pref_val <- if (dim_name %in% names(example_state$positive_prefs)) {
      example_state$positive_prefs[dim_name]
    } else 0
    
    if (pref_val > 0.05) {
      cat(sprintf("    %-22s  %-12s  (strength=%.3f)\n",
                  dim_name, classifications[[dim_name]], pref_val))
    }
  }
}

# EXPERIMENT 4: Visualise decay curves

cat("\nExperiment 4: Decay Curve Visualisation\n\n")

plot_decay_curves(output_file = "results/decay_curves.png")
cat("  decay curves saved to results/decay_curves.png\n")

# also show half-lives
print_decay_summary()

# SAVE RESULTS

results_df <- do.call(rbind, lapply(names(decay_comparison), function(name) {
  r <- decay_comparison[[name]]
  r$decay_type <- name
  r
}))
write.csv(results_df, "results/decay_comparison.csv", row.names = FALSE)

cat("\n\n")
cat("  EXPERIMENT COMPLETE\n")
cat("\n")
