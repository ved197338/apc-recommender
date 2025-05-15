# Experiment — Relevance vs Diversity tradeoff evaluation

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

# SETUP

set_experiment_seed(555, "exploration strategy comparison")

cat("\n")
cat("  Exploration Strategy Comparison\n")
cat("\n\n")

catalogue <- generate_synthetic_catalogue(n_items = 400, seed = 42)
sim_users <- create_sim_user_population(n_users = 20, seed = 42)

sim_results <- run_population_simulation(
  sim_users, catalogue,
  n_days = 80,
  items_per_day = 12,
  seed = 42,
  verbose = TRUE
)

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

hierarchy <- create_default_hierarchy()

# precompute momentum
all_momentum <- list()
for (uid in names(state_mgr$users)) {
  all_momentum[[uid]] <- compute_user_momentum(state_mgr$users[[uid]])
}

# TEST EACH EXPLORATION STRATEGY

strategies <- c("none", "epsilon", "ucb", "hierarchical", "momentum")
k <- 10

strategy_results <- list()
explore_metrics <- list()

for (strategy in strategies) {
  cat(sprintf("\ntesting strategy: %s\n", strategy))
  
  # define recommendation function with this strategy
  recommend_fn <- function(user_state, cat_arg) {
    uid <- user_state$user_id
    
    # get base recommendations
    base_recs <- score_all_items(
      user_state, cat_arg,
      momentum = all_momentum[[uid]],
      hierarchy = hierarchy
    )
    
    if (strategy == "none") {
      return(head(base_recs, 50))
    }
    
    # apply exploration
    explore_config <- get_exploration_config()
    explore_config$strategy <- strategy
    
    explored <- apply_exploration(
      base_recs, user_state, cat_arg,
      hierarchy = hierarchy,
      momentum = all_momentum[[uid]],
      config = explore_config,
      k = 50
    )
    
    return(explored)
  }
  
  # evaluate
  results <- evaluate_algorithm(
    recommend_fn, state_mgr$users, test_ints,
    catalogue, k_values = c(5, 10, 20),
    verbose = FALSE
  )
  
  strategy_results[[strategy]] <- results
  
  # also compute detailed exploration metrics for each user
  user_metrics <- list()
  for (uid in names(state_mgr$users)) {
    state <- state_mgr$users[[uid]]
    recs <- recommend_fn(state, catalogue)
    recs_top <- head(recs, k)
    
    em <- compute_exploration_metrics(recs_top, state, catalogue)
    user_metrics[[uid]] <- em
  }
  
  # average exploration metrics
  avg_diversity <- mean(sapply(user_metrics, function(m) m$diversity))
  avg_coverage <- mean(sapply(user_metrics, function(m) m$coverage))
  avg_relevance <- mean(sapply(user_metrics, function(m) m$avg_relevance))
  avg_explore_ratio <- mean(sapply(user_metrics, function(m) m$exploration_ratio))
  
  explore_metrics[[strategy]] <- data.frame(
    strategy = strategy,
    avg_diversity = avg_diversity,
    avg_coverage = avg_coverage,
    avg_relevance = avg_relevance,
    avg_explore_ratio = avg_explore_ratio,
    stringsAsFactors = FALSE
  )
  
  cat(sprintf("  diversity=%.4f  coverage=%.4f  relevance=%.4f  explore_ratio=%.4f\n",
              avg_diversity, avg_coverage, avg_relevance, avg_explore_ratio))
}

# RESULTS

cat("\n\n")
cat("  RESULTS\n")
cat("\n")

compare_algorithms(strategy_results)

# exploration-specific metrics
cat("\nExploration Metrics\n\n")
explore_df <- do.call(rbind, explore_metrics)
print(explore_df)

# relevance-diversity tradeoff plot
cat("\ngenerating relevance-diversity tradeoff plot...\n")

png("results/exploration_tradeoff.png", width = 700, height = 500, res = 100)

# get NDCG@10 as relevance proxy
ndcg_vals <- sapply(strategy_results, function(r) r[r$k == 10, "ndcg"])
div_vals <- sapply(strategy_results, function(r) r[r$k == 10, "diversity"])

colors <- c("#95a5a6", "#e74c3c", "#3498db", "#2ecc71", "#f39c12")
names(colors) <- strategies

plot(div_vals, ndcg_vals, 
     pch = 20, cex = 2, col = colors[strategies],
     xlim = range(div_vals) * c(0.9, 1.1),
     ylim = range(ndcg_vals) * c(0.9, 1.1),
     xlab = "Diversity (ILD)",
     ylab = "NDCG@10",
     main = "Relevance-Diversity Tradeoff by Exploration Strategy",
     bty = "l", las = 1)

grid(col = "gray90")

text(div_vals, ndcg_vals, labels = strategies, pos = 3, cex = 0.8)

# draw the pareto frontier (rough)
sorted_by_div <- order(div_vals)
pareto_idx <- c()
max_ndcg <- -Inf
for (idx in rev(sorted_by_div)) {
  if (ndcg_vals[idx] > max_ndcg) {
    pareto_idx <- c(idx, pareto_idx)
    max_ndcg <- ndcg_vals[idx]
  }
}

if (length(pareto_idx) > 1) {
  lines(div_vals[pareto_idx], ndcg_vals[pareto_idx], 
        col = "#e74c3c", lty = 2, lwd = 1)
}

dev.off()

# comparison plot
plot_algorithm_comparison(
  strategy_results,
  metrics = c("ndcg", "precision", "diversity"),
  output_file = "results/exploration_comparison.png"
)

# save
results_df <- do.call(rbind, lapply(names(strategy_results), function(name) {
  r <- strategy_results[[name]]
  r$strategy <- name
  r
}))
write.csv(results_df, "results/exploration_comparison.csv", row.names = FALSE)

cat("\n\n")
cat("  EXPERIMENT COMPLETE\n")
cat("\n")
