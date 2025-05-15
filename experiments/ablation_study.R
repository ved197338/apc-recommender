# Experiment — Component ablation study

#   8. APC minimal (just base similarity with decay)
#

###############################################################################

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

set_experiment_seed(456, "ablation study")

cat("\n")
cat("  APC Ablation Study\n")
cat("\n\n")

# generate data
cat("generating catalogue and simulated users...\n")
catalogue <- generate_synthetic_catalogue(n_items = 400, seed = 42)
sim_users <- create_sim_user_population(n_users = 25, seed = 42)

cat("running simulation...\n")
sim_results <- run_population_simulation(
  sim_users, catalogue,
  n_days = 80,
  items_per_day = 12,
  seed = 42,
  verbose = TRUE
)

# collect interactions
all_interactions <- list()
for (uid in names(sim_results)) {
  all_interactions <- c(all_interactions, sim_results[[uid]]$interactions)
}

# train/test split
base_ts <- min(sapply(all_interactions, function(i) i$timestamp))
split_ts <- base_ts + 60 * 86400

train_ints <- Filter(function(i) i$timestamp < split_ts, all_interactions)
test_ints <- Filter(function(i) i$timestamp >= split_ts, all_interactions)

cat(sprintf("\ntrain: %d interactions, test: %d interactions\n",
            length(train_ints), length(test_ints)))

# TRAIN BASE APC STATE (all variants share the same preference state)

cat("\ntraining shared preference states...\n")
hierarchy <- create_default_hierarchy()
decay_config <- get_decay_config()
weight_config <- get_interaction_config()

# full APC state (with decay)
state_mgr <- create_state_manager()
state_mgr <- process_all_interactions(
  state_mgr, train_ints, catalogue,
  decay_config = decay_config,
  weight_config = weight_config,
  verbose = TRUE
)

# state without decay
no_decay <- get_decay_config()
no_decay$default_lambda <- 0
no_decay$category_lambdas <- lapply(no_decay$category_lambdas, function(x) 0)
no_decay$dimension_overrides <- lapply(no_decay$dimension_overrides, function(x) 0)

state_mgr_nodecay <- create_state_manager()
state_mgr_nodecay <- process_all_interactions(
  state_mgr_nodecay, train_ints, catalogue,
  decay_config = no_decay,
  weight_config = weight_config,
  verbose = FALSE
)

# precompute momentum and compound codes
cat("computing momentum and compound codes...\n")
all_momentum <- list()
all_compounds <- list()

for (uid in names(state_mgr$users)) {
  state <- state_mgr$users[[uid]]
  all_momentum[[uid]] <- compute_user_momentum(state)
  
  user_ints <- Filter(function(i) i$user_id == uid, train_ints)
  all_compounds[[uid]] <- detect_compound_codes(user_ints, catalogue)
}

# DEFINE ABLATION VARIANTS

cat("\ndefining ablation variants...\n")

# helper to create variant-specific scoring configs
make_variant_config <- function(variant) {
  config <- get_scoring_config()
  
  switch(variant,
    "no_compounds" = {
      config$beta <- 0
    },
    "no_momentum" = {
      config$gamma <- 0
    },
    "no_novelty" = {
      config$delta <- 0
    },
    "no_hierarchy" = {
      config$epsilon <- 0
    },
    "no_repetition" = {
      config$zeta <- 0
    },
    "minimal" = {
      config$beta <- 0
      config$gamma <- 0
      config$delta <- 0
      config$epsilon <- 0
    }
  )
  
  # re-normalise remaining weights
  pos_weights <- c(config$alpha, config$beta, config$gamma, 
                   config$delta, config$epsilon)
  total <- sum(pos_weights)
  if (total > 0 && total != 1) {
    config$alpha <- config$alpha / total * 0.9
    config$beta <- config$beta / total * 0.9
    config$gamma <- config$gamma / total * 0.9
    config$delta <- config$delta / total * 0.9
    config$epsilon <- config$epsilon / total * 0.9
  }
  
  return(config)
}

# create recommendation functions for each variant
variants <- list(
  "APC Full" = function(user_state, cat_arg) {
    uid <- user_state$user_id
    score_all_items(user_state, cat_arg,
                    compounds = all_compounds[[uid]],
                    momentum = all_momentum[[uid]],
                    hierarchy = hierarchy,
                    config = get_scoring_config())
  },
  
  "No Decay" = function(user_state, cat_arg) {
    uid <- user_state$user_id
    nd_state <- state_mgr_nodecay$users[[uid]]
    if (is.null(nd_state)) nd_state <- user_state
    score_all_items(nd_state, cat_arg,
                    compounds = all_compounds[[uid]],
                    momentum = all_momentum[[uid]],
                    hierarchy = hierarchy,
                    config = get_scoring_config())
  },
  
  "No Momentum" = function(user_state, cat_arg) {
    uid <- user_state$user_id
    score_all_items(user_state, cat_arg,
                    compounds = all_compounds[[uid]],
                    momentum = NULL,
                    hierarchy = hierarchy,
                    config = make_variant_config("no_momentum"))
  },
  
  "No Compounds" = function(user_state, cat_arg) {
    uid <- user_state$user_id
    score_all_items(user_state, cat_arg,
                    compounds = NULL,
                    momentum = all_momentum[[uid]],
                    hierarchy = hierarchy,
                    config = make_variant_config("no_compounds"))
  },
  
  "No Hierarchy" = function(user_state, cat_arg) {
    uid <- user_state$user_id
    score_all_items(user_state, cat_arg,
                    compounds = all_compounds[[uid]],
                    momentum = all_momentum[[uid]],
                    hierarchy = NULL,
                    config = make_variant_config("no_hierarchy"))
  },
  
  "No Exploration" = function(user_state, cat_arg) {
    uid <- user_state$user_id
    config <- get_scoring_config()
    config$delta <- 0  # no novelty
    score_all_items(user_state, cat_arg,
                    compounds = all_compounds[[uid]],
                    momentum = all_momentum[[uid]],
                    hierarchy = hierarchy,
                    config = config)
  },
  
  "Minimal" = function(user_state, cat_arg) {
    score_all_items(user_state, cat_arg,
                    compounds = NULL,
                    momentum = NULL,
                    hierarchy = NULL,
                    config = make_variant_config("minimal"))
  }
)

# EVALUATE ALL VARIANTS

cat("\nevaluating ablation variants...\n\n")
k_values <- c(5, 10, 20)
ablation_results <- list()

for (variant_name in names(variants)) {
  cat(sprintf("  evaluating: %s...\n", variant_name))
  
  user_states <- if (variant_name == "No Decay") {
    state_mgr_nodecay$users
  } else {
    state_mgr$users
  }
  
  results <- evaluate_algorithm(
    variants[[variant_name]],
    user_states,
    test_ints,
    catalogue,
    k_values = k_values,
    verbose = FALSE
  )
  
  ablation_results[[variant_name]] <- results
}

# RESULTS

cat("\n\n")
cat("  ABLATION RESULTS\n")
cat("\n")

for (name in names(ablation_results)) {
  print_evaluation_results(ablation_results[[name]], name)
}

compare_algorithms(ablation_results)

# Relative performance
# show each variant's performance relative to the full APC
cat("\nRelative Performance (vs APC Full, K=10)\n\n")
full_results <- ablation_results[["APC Full"]]
full_k10 <- full_results[full_results$k == 10, ]

cat(sprintf("  %-20s  %-8s  %-8s  %-8s  %-8s\n",
            "Variant", "ΔNDCG", "ΔPrec", "ΔRecall", "ΔDiv"))
cat(paste(rep("-", 56), collapse = ""), "\n")

for (name in names(ablation_results)) {
  r <- ablation_results[[name]]
  r_k10 <- r[r$k == 10, ]
  
  delta_ndcg <- r_k10$ndcg - full_k10$ndcg
  delta_prec <- r_k10$precision - full_k10$precision
  delta_rec  <- r_k10$recall - full_k10$recall
  delta_div  <- r_k10$diversity - full_k10$diversity
  
  # format with + or - sign
  fmt <- function(x) {
    if (x >= 0) sprintf("+%.4f", x) else sprintf("%.4f", x)
  }
  
  cat(sprintf("  %-20s  %-8s  %-8s  %-8s  %-8s\n",
              name, fmt(delta_ndcg), fmt(delta_prec), 
              fmt(delta_rec), fmt(delta_div)))
}

# Plot
cat("\ngenerating ablation plots...\n")

plot_algorithm_comparison(
  ablation_results,
  metrics = c("ndcg", "precision", "diversity"),
  output_file = "results/ablation_study.png"
)

# bar chart of component contributions at K=10
png("results/ablation_contributions.png", width = 800, height = 500, res = 100)

variant_names <- names(ablation_results)
ndcg_values <- sapply(ablation_results, function(r) r[r$k == 10, "ndcg"])

# sort by NDCG for readability
sorted_idx <- order(ndcg_values, decreasing = TRUE)
variant_names <- variant_names[sorted_idx]
ndcg_values <- ndcg_values[sorted_idx]

colors <- ifelse(variant_names == "APC Full", "#2ecc71", 
                 ifelse(ndcg_values < ndcg_values[variant_names == "APC Full"],
                        "#e74c3c", "#3498db"))

barplot(ndcg_values, names.arg = variant_names,
        col = colors, border = NA, las = 2,
        main = "Ablation Study: NDCG@10 by Variant",
        ylab = "NDCG@10", cex.names = 0.7)

abline(h = ndcg_values[variant_names == "APC Full"], 
       col = "#2ecc71", lty = 2, lwd = 2)

dev.off()
cat("ablation contribution plot saved to results/ablation_contributions.png\n")

# save results
results_df <- do.call(rbind, lapply(names(ablation_results), function(name) {
  r <- ablation_results[[name]]
  r$variant <- name
  r
}))
write.csv(results_df, "results/ablation_study.csv", row.names = FALSE)

cat("\n\n")
cat("  INTERPRETATION\n")
cat("\n\n")
cat("key questions to answer:\n")
cat("  1. which component contributes most to NDCG?\n")
cat("  2. does removing any single component cause a big drop?\n")
cat("  3. is the full model significantly better than the minimal version?\n")
cat("  4. which component most affects diversity?\n")
cat("  5. is the added complexity of compound codes justified?\n\n")
cat("these results should inform whether each component is worth keeping.\n")
cat("if a component adds significant compute cost but negligible performance\n")
cat("gain, it should probably be dropped or simplified.\n")

cat("\n\n")
cat("  EXPERIMENT COMPLETE\n")
cat("\n")
