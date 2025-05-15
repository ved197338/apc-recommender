# Experiment — Temporal preference drift and adaptation speed benchmarking

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

# EXPERIMENT SETUP

set_experiment_seed(123, "preference drift experiment")

cat("\n")
cat("  Preference Drift Adaptation Experiment\n")
cat("\n\n")

# create catalogue
cat("generating catalogue...\n")
catalogue <- generate_synthetic_catalogue(n_items = 500, seed = 42)

# create specific users with known drift schedules
# user 1: gaming → mathematics (complete shift)
drift_user_1 <- create_sim_user(
  user_id = "drift_user_1",
  true_preferences = c(gaming = 0.9, entertainment = 0.7, exciting = 0.5,
                        comedy = 0.4, technology = 0.3),
  archetype = "gamer",
  preference_stability = 0.8,
  exploration_tendency = 0.1,
  noise_level = 0.15
)

drift_user_1 <- schedule_drift(
  drift_user_1,
  start_day = 50, end_day = 60,
  from_dims = c(gaming = 0.9, entertainment = 0.7, comedy = 0.4),
  to_dims = c(mathematics = 0.8, physics = 0.6, educational = 0.5, 
              lecture = 0.4),
  magnitude = 0.8
)

# user 2: tech → creative (gradual shift)
drift_user_2 <- create_sim_user(
  user_id = "drift_user_2",
  true_preferences = c(technology = 0.8, ai = 0.7, cybersecurity = 0.5,
                        educational = 0.5, advanced = 0.4),
  archetype = "tech_enthusiast",
  preference_stability = 0.85,
  exploration_tendency = 0.1,
  noise_level = 0.12
)

drift_user_2 <- schedule_drift(
  drift_user_2,
  start_day = 45, end_day = 75,  # slower drift
  from_dims = c(technology = 0.6, cybersecurity = 0.5),
  to_dims = c(art = 0.7, music = 0.5, design = 0.4, inspiring = 0.4),
  magnitude = 0.6
)

# user 3: science → entertainment (sudden shift)
drift_user_3 <- create_sim_user(
  user_id = "drift_user_3",
  true_preferences = c(mathematics = 0.8, physics = 0.7, space = 0.5,
                        educational = 0.6, documentary = 0.5),
  archetype = "science_nerd",
  preference_stability = 0.9,
  exploration_tendency = 0.1,
  noise_level = 0.1
)

drift_user_3 <- schedule_drift(
  drift_user_3,
  start_day = 50, end_day = 55,  # very sudden
  from_dims = c(mathematics = 0.8, physics = 0.7),
  to_dims = c(comedy = 0.7, entertainment = 0.8, funny = 0.6, drama = 0.4),
  magnitude = 0.85
)

drift_users <- list(
  drift_user_1 = drift_user_1,
  drift_user_2 = drift_user_2,
  drift_user_3 = drift_user_3
)

# RUN SIMULATION

cat("running simulation for drift users (90 days)...\n")
base_timestamp <- as.numeric(Sys.time()) - 90 * 86400
hierarchy <- create_default_hierarchy()
decay_config <- get_decay_config()
weight_config <- get_interaction_config()

# process each drift user
for (uid in names(drift_users)) {
  cat(sprintf("\n%s\n", uid))
  
  sim_user <- drift_users[[uid]]
  
  # simulate 90 days
  sim_result <- run_user_simulation(
    sim_user, catalogue, n_days = 90,
    items_per_day = 15,
    base_timestamp = base_timestamp,
    verbose = TRUE
  )
  
  # split interactions at the drift point
  drift_start <- sim_user$drift_schedule[[1]]$start_day
  split_time <- base_timestamp + drift_start * 86400
  
  pre_drift <- Filter(function(i) i$timestamp < split_time,
                       sim_result$interactions)
  post_drift <- Filter(function(i) i$timestamp >= split_time,
                        sim_result$interactions)
  
  cat(sprintf("  pre-drift interactions:  %d\n", length(pre_drift)))
  cat(sprintf("  post-drift interactions: %d\n", length(post_drift)))
  
# Train all algorithms on pre-drift data
  
  # APC
  cat("  training APC on pre-drift data...\n")
  apc_state <- create_user_state(uid)
  apc_state <- batch_update_preferences(
    apc_state, pre_drift, catalogue,
    decay_config = decay_config,
    weight_config = weight_config
  )
  
  cat("  top preferences before drift:\n")
  pre_prefs <- get_top_preferences(apc_state, k = 5)
  for (j in seq_along(pre_prefs$names)) {
    cat(sprintf("    %-20s %.3f\n", pre_prefs$names[j], pre_prefs$values[j]))
  }
  
# Measure adaptation on post-drift data
  cat("\n  measuring adaptation speed...\n")
  
  # identify items that match old vs new preferences
  drift <- sim_user$drift_schedule[[1]]
  
  # items matching old preferences (from_dims)
  old_pref_items <- character(0)
  new_pref_items <- character(0)
  
  for (item_id in names(catalogue$items)) {
    item <- catalogue$items[[item_id]]
    
    # check old preference match
    if (!is.null(drift$from_dims)) {
      old_match <- sum(sapply(names(drift$from_dims), function(d) {
        if (d %in% names(item$code)) item$code[d] else 0
      }))
      if (old_match > 0.5) {
        old_pref_items <- c(old_pref_items, item_id)
      }
    }
    
    # check new preference match
    new_match <- sum(sapply(names(drift$to_dims), function(d) {
      if (d %in% names(item$code)) item$code[d] else 0
    }))
    if (new_match > 0.5) {
      new_pref_items <- c(new_pref_items, item_id)
    }
  }
  
  cat(sprintf("  items matching old preferences: %d\n", length(old_pref_items)))
  cat(sprintf("  items matching new preferences: %d\n", length(new_pref_items)))
  
  # now process post-drift interactions one at a time and check adaptation
  adaptation_data <- data.frame(
    n_interactions = integer(0),
    old_in_topk = numeric(0),
    new_in_topk = numeric(0),
    algorithm = character(0),
    stringsAsFactors = FALSE
  )
  
# Apc adaptation
  apc_state_copy <- apc_state
  check_every <- 5
  
  for (i in seq_along(post_drift)) {
    interaction <- post_drift[[i]]
    item <- catalogue$items[[interaction$item_id]]
    if (is.null(item)) next
    
    apc_state_copy <- update_preference_state(
      apc_state_copy, interaction, item$code,
      decay_config = decay_config,
      weight_config = weight_config,
      store_snapshot = FALSE
    )
    
    if (i %% check_every == 0 || i == 1) {
      # get APC recommendations
      momentum <- compute_user_momentum(apc_state_copy)
      recs <- score_all_items(apc_state_copy, catalogue,
                               momentum = momentum,
                               hierarchy = hierarchy)
      top_ids <- head(recs$item_id, 10)
      
      old_frac <- sum(top_ids %in% old_pref_items) / min(10, length(old_pref_items))
      new_frac <- sum(top_ids %in% new_pref_items) / min(10, length(new_pref_items))
      
      adaptation_data <- rbind(adaptation_data, data.frame(
        n_interactions = i,
        old_in_topk = old_frac,
        new_in_topk = new_frac,
        algorithm = "APC",
        stringsAsFactors = FALSE
      ))
    }
  }
  
# Content-based (no decay/momentum, just accumulate)
  cb_state <- create_user_state(uid)
  
  # train on pre-drift — but without decay (static accumulation)
  no_decay_config <- get_decay_config()
  no_decay_config$default_lambda <- 0.0  # no decay
  no_decay_config$category_lambdas <- lapply(no_decay_config$category_lambdas, 
                                              function(x) 0)
  
  cb_state <- batch_update_preferences(
    cb_state, pre_drift, catalogue,
    decay_config = no_decay_config,
    weight_config = weight_config
  )
  
  for (i in seq_along(post_drift)) {
    interaction <- post_drift[[i]]
    item <- catalogue$items[[interaction$item_id]]
    if (is.null(item)) next
    
    cb_state <- update_preference_state(
      cb_state, interaction, item$code,
      decay_config = no_decay_config,
      weight_config = weight_config,
      store_snapshot = FALSE
    )
    
    if (i %% check_every == 0 || i == 1) {
      prefs <- get_combined_preferences(cb_state)
      scores <- sapply(catalogue$items, function(it) {
        cosine_similarity(prefs, it$code)
      })
      sorted <- sort(scores, decreasing = TRUE)
      top_ids <- names(sorted)[1:10]
      
      old_frac <- sum(top_ids %in% old_pref_items) / min(10, length(old_pref_items))
      new_frac <- sum(top_ids %in% new_pref_items) / min(10, length(new_pref_items))
      
      adaptation_data <- rbind(adaptation_data, data.frame(
        n_interactions = i,
        old_in_topk = old_frac,
        new_in_topk = new_frac,
        algorithm = "Content-Based (no decay)",
        stringsAsFactors = FALSE
      ))
    }
  }
  
# Popularity (doesn't adapt at all, as expected)
  pop_fn <- popularity_baseline(pre_drift, catalogue)
  pop_recs <- pop_fn(apc_state, catalogue)  # same recommendations always
  pop_top <- head(pop_recs$item_id, 10)
  
  for (i in seq(check_every, length(post_drift), by = check_every)) {
    adaptation_data <- rbind(adaptation_data, data.frame(
      n_interactions = i,
      old_in_topk = sum(pop_top %in% old_pref_items) / min(10, length(old_pref_items)),
      new_in_topk = sum(pop_top %in% new_pref_items) / min(10, length(new_pref_items)),
      algorithm = "Popularity",
      stringsAsFactors = FALSE
    ))
  }
  
# Plot adaptation curves
  cat("\n  plotting adaptation curves...\n")
  
  output_file <- sprintf("results/adaptation_%s.png", uid)
  png(output_file, width = 900, height = 500, res = 100)
  
  algorithms <- unique(adaptation_data$algorithm)
  colors <- c("APC" = "#e74c3c", 
              "Content-Based (no decay)" = "#3498db",
              "Popularity" = "#95a5a6")
  
  plot(NULL, xlim = range(adaptation_data$n_interactions),
       ylim = c(0, 1),
       xlab = "Interactions After Preference Drift",
       ylab = "Fraction in Top-10",
       main = sprintf("Adaptation Speed — %s", uid),
       bty = "l", las = 1)
  
  grid(col = "gray90")
  
  for (alg in algorithms) {
    alg_data <- adaptation_data[adaptation_data$algorithm == alg, ]
    lines(alg_data$n_interactions, alg_data$new_in_topk,
          col = colors[alg], lwd = 2)
    lines(alg_data$n_interactions, alg_data$old_in_topk,
          col = colors[alg], lwd = 1, lty = 2)
  }
  
  legend("right", 
         c(paste(algorithms, "(new prefs)"), paste(algorithms, "(old prefs)")),
         col = rep(colors[algorithms], 2),
         lwd = c(rep(2, length(algorithms)), rep(1, length(algorithms))),
         lty = c(rep(1, length(algorithms)), rep(2, length(algorithms))),
         cex = 0.6, bg = "white")
  
  dev.off()
  cat(sprintf("  saved to %s\n", output_file))
  
  # show the final state
  cat("\n  top preferences after drift:\n")
  post_prefs <- get_top_preferences(apc_state_copy, k = 5)
  for (j in seq_along(post_prefs$names)) {
    cat(sprintf("    %-20s %.3f\n", post_prefs$names[j], post_prefs$values[j]))
  }
  
  # preference evolution plot
  if (length(apc_state_copy$history) > 10) {
    plot_preference_evolution(
      apc_state_copy,
      output_file = sprintf("results/pref_evolution_%s.png", uid)
    )
  }
}

# Summary
cat("\n\n")
cat("  ADAPTATION SPEED RESULTS\n")
cat("\n\n")

cat("interpretation:\n")
cat("  - solid lines show fraction of NEW preference items in top-10\n")
cat("  - dashed lines show fraction of OLD preference items in top-10\n")
cat("  - faster rise of solid line = faster adaptation\n")
cat("  - APC should adapt faster due to temporal decay and momentum\n")
cat("  - content-based without decay is slower because old preferences dilute\n")
cat("  - popularity doesn't adapt at all (as expected)\n\n")

cat("note: these results are from synthetic data. real-world performance\n")
cat("may differ significantly. the experiment shows the mechanism,\n")
cat("not necessarily the magnitude of the advantage.\n")

write.csv(adaptation_data, "results/adaptation_data.csv", row.names = FALSE)
cat("\nadaptation data saved to results/adaptation_data.csv\n")

cat("\n\n")
cat("  EXPERIMENT COMPLETE\n")
cat("\n")
