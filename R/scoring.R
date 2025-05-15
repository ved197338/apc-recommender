# APC Engine — Multi-factor scoring function and recommendation engine

# to comparable scales.
#

###############################################################################

source("R/utils.R")
source("R/codes.R")
source("R/preference_state.R")
source("R/momentum.R")
source("R/compound_codes.R")
source("R/hierarchy.R")

# scoring configuration / coefficients
get_scoring_config <- function() {
  config <- list(
    # scoring weights — these are the greek letters from the formula
    alpha   = 0.35,   # base cosine similarity
    beta    = 0.15,   # compound code similarity
    gamma   = 0.15,   # preference momentum alignment
    delta   = 0.10,   # novelty bonus
    epsilon = 0.15,   # hierarchical similarity
    zeta    = 0.10,   # repetition penalty
    
    # normalisation method for components
    # "minmax" or "zscore" or "none"
    normalisation = "minmax",
    
    # novelty settings
    novelty_decay  = 0.95,  # how fast novelty decays with familiarity
    min_novelty    = 0.0,
    
    # repetition penalty settings
    repetition_window = 50,   # look back this many interactions
    full_penalty_count = 3,   # after this many views, apply full penalty
    
    # negative preference suppression
    negative_suppression = 0.5,  # how much negative prefs reduce score
    
    # score combination method
    # "linear" or "multiplicative"
    combination = "linear"
  )
  
  return(config)
}

# compute base similarity — cosine between user preferences and item code
# this is the bread-and-butter similarity measure
compute_base_similarity <- function(user_prefs, item_code) {
  return(cosine_similarity(user_prefs, item_code))
}

# compute novelty score for an item
# novelty is based on how different the item is from what the user
# has already consumed. novel items get a bonus to encourage diversity.
compute_novelty <- function(item_code, interaction_log, catalogue,
                             config = NULL) {
  if (is.null(config)) config <- get_scoring_config()
  
  if (length(interaction_log) == 0) return(1.0)
  
  # get codes of recently consumed items
  recent_items <- tail(interaction_log, config$repetition_window)
  consumed_codes <- list()
  
  for (interaction in recent_items) {
    item <- catalogue$items[[interaction$item_id]]
    if (!is.null(item)) {
      consumed_codes[[length(consumed_codes) + 1]] <- item$code
    }
  }
  
  if (length(consumed_codes) == 0) return(1.0)
  
  # compute average similarity to consumed items
  sims <- sapply(consumed_codes, function(code) {
    cosine_similarity(item_code, code)
  })
  
  avg_sim <- mean(sims)
  
  # novelty = 1 - average_similarity, with some scaling
  novelty <- (1 - avg_sim) * config$novelty_decay
  novelty <- max(config$min_novelty, novelty)
  
  return(novelty)
}

# compute repetition penalty
# items the user has already seen should be penalised, especially
# if they saw them recently
compute_repetition_penalty <- function(item_id, interaction_log,
                                        config = NULL) {
  if (is.null(config)) config <- get_scoring_config()
  
  if (length(interaction_log) == 0) return(0)
  
  # count how many times the user has seen this item recently
  recent <- tail(interaction_log, config$repetition_window)
  view_count <- sum(sapply(recent, function(i) i$item_id == item_id))
  
  if (view_count == 0) return(0)
  
  # penalty increases with view count, saturating at full_penalty_count
  penalty <- min(1.0, view_count / config$full_penalty_count)
  
  # also factor in recency — very recent views get a stronger penalty
  last_view_idx <- max(which(sapply(recent, function(i) i$item_id == item_id)))
  recency_factor <- 1 - (last_view_idx / length(recent))
  
  # combine count-based and recency-based penalty
  penalty <- penalty * (0.5 + 0.5 * recency_factor)
  
  return(penalty)
}

# compute negative preference penalty
# if the item matches the user's negative preferences, penalise it
compute_negative_penalty <- function(item_code, negative_prefs, 
                                      config = NULL) {
  if (is.null(config)) config <- get_scoring_config()
  
  if (length(negative_prefs) == 0) return(0)
  
  # cosine between item and negative preferences
  neg_sim <- cosine_similarity(item_code, negative_prefs)
  
  # scale by suppression factor
  return(neg_sim * config$negative_suppression)
}

# compute the full APC recommendation score
#
# this is the main scoring function that brings everything together.
# returns a single numeric score for a user-item pair.
#
# takes all the pre-computed components to avoid redundant computation
# when scoring many items at once.
compute_apc_score <- function(user_state, item, 
                               compounds = NULL,
                               momentum = NULL,
                               hierarchy = NULL,
                               catalogue = NULL,
                               config = NULL) {
  if (is.null(config)) config <- get_scoring_config()
  
  user_prefs <- get_combined_preferences(user_state)
  item_code <- item$code
  
  # 1. Base similarity (alpha)
  base_sim <- compute_base_similarity(user_prefs, item_code)
  
  # 2. Compound similarity (beta)
  compound_sim <- 0
  if (!is.null(compounds) && length(compounds) > 0) {
    compound_sim <- compute_compound_similarity(user_prefs, item_code, compounds)
  }
  
  # 3. Preference momentum (gamma)
  momentum_score <- 0
  if (!is.null(momentum) && length(momentum) > 0) {
    momentum_score <- compute_momentum_score(item_code, momentum)
  }
  
  # 4. Novelty (delta)
  novelty <- 1.0
  if (!is.null(catalogue)) {
    novelty <- compute_novelty(item_code, user_state$interaction_log,
                               catalogue, config)
  }
  
  # 5. Hierarchical similarity (epsilon)
  hier_sim <- 0
  if (!is.null(hierarchy)) {
    hier_sim <- compute_hierarchical_similarity(user_prefs, item_code, hierarchy)
  }
  
  # 6. Repetition penalty (zeta)
  rep_penalty <- compute_repetition_penalty(item$id, user_state$interaction_log,
                                             config)
  
  # 7. Negative preference penalty (additional)
  neg_penalty <- compute_negative_penalty(item_code, user_state$negative_prefs,
                                           config)
  
  # combine components
  if (config$combination == "linear") {
    score <- config$alpha   * base_sim +
             config$beta    * compound_sim +
             config$gamma   * momentum_score +
             config$delta   * novelty +
             config$epsilon * hier_sim -
             config$zeta    * rep_penalty -
             neg_penalty
  } else if (config$combination == "multiplicative") {
    # multiplicative — more aggressive, items need to score well on everything
    pos_score <- (0.5 + base_sim)^config$alpha * 
                 (0.5 + compound_sim)^config$beta *
                 (0.5 + momentum_score)^config$gamma *
                 (0.5 + novelty)^config$delta *
                 (0.5 + hier_sim)^config$epsilon
    
    neg_score <- (1 - config$zeta * rep_penalty) * (1 - neg_penalty)
    
    score <- pos_score * neg_score
  }
  
  return(score)
}

# score all items in a catalogue for a given user
# returns a sorted data frame with item IDs and scores
score_all_items <- function(user_state, catalogue,
                             compounds = NULL,
                             momentum = NULL,
                             hierarchy = NULL,
                             config = NULL,
                             exclude_seen = FALSE) {
  if (is.null(config)) config <- get_scoring_config()
  
  # items to skip
  seen_items <- character(0)
  if (exclude_seen) {
    seen_items <- unique(sapply(user_state$interaction_log, function(i) i$item_id))
  }
  
  # score every item
  results <- data.frame(
    item_id = character(0),
    score   = numeric(0),
    title   = character(0),
    stringsAsFactors = FALSE
  )
  
  for (item_id in names(catalogue$items)) {
    if (item_id %in% seen_items) next
    
    item <- catalogue$items[[item_id]]
    
    score <- compute_apc_score(
      user_state, item,
      compounds = compounds,
      momentum = momentum,
      hierarchy = hierarchy,
      catalogue = catalogue,
      config = config
    )
    
    results <- rbind(results, data.frame(
      item_id = item_id,
      score   = score,
      title   = item$title,
      stringsAsFactors = FALSE
    ))
  }
  
  # sort by score descending
  results <- results[order(-results$score), ]
  rownames(results) <- NULL
  
  return(results)
}

# get top-K recommendations
get_recommendations <- function(user_state, catalogue, k = 10,
                                 compounds = NULL,
                                 momentum = NULL,
                                 hierarchy = NULL,
                                 config = NULL,
                                 exploration_config = NULL) {
  scores <- score_all_items(
    user_state, catalogue,
    compounds = compounds,
    momentum = momentum,
    hierarchy = hierarchy,
    config = config,
    exclude_seen = TRUE
  )
  
  # basic top-K
  top_k_items <- head(scores, k)
  
  return(top_k_items)
}

# score decomposition — breaks down the score into its components
# useful for debugging why an item was recommended
explain_score <- function(user_state, item,
                           compounds = NULL,
                           momentum = NULL,
                           hierarchy = NULL,
                           catalogue = NULL,
                           config = NULL) {
  if (is.null(config)) config <- get_scoring_config()
  
  user_prefs <- get_combined_preferences(user_state)
  item_code <- item$code
  
  base_sim <- compute_base_similarity(user_prefs, item_code)
  compound_sim <- if (!is.null(compounds)) {
    compute_compound_similarity(user_prefs, item_code, compounds)
  } else 0
  
  momentum_score <- if (!is.null(momentum)) {
    compute_momentum_score(item_code, momentum)
  } else 0
  
  novelty <- if (!is.null(catalogue)) {
    compute_novelty(item_code, user_state$interaction_log, catalogue, config)
  } else 1.0
  
  hier_sim <- if (!is.null(hierarchy)) {
    compute_hierarchical_similarity(user_prefs, item_code, hierarchy)
  } else 0
  
  rep_penalty <- compute_repetition_penalty(item$id, user_state$interaction_log,
                                             config)
  neg_penalty <- compute_negative_penalty(item_code, user_state$negative_prefs,
                                           config)
  
  # compute weighted contributions
  breakdown <- data.frame(
    component = c("Base Similarity", "Compound Similarity",
                  "Momentum", "Novelty", "Hierarchical",
                  "Repetition Penalty", "Negative Penalty"),
    raw_value = c(base_sim, compound_sim, momentum_score, novelty,
                  hier_sim, rep_penalty, neg_penalty),
    weight = c(config$alpha, config$beta, config$gamma, config$delta,
               config$epsilon, -config$zeta, -1),
    weighted = c(
      config$alpha * base_sim,
      config$beta * compound_sim,
      config$gamma * momentum_score,
      config$delta * novelty,
      config$epsilon * hier_sim,
      -config$zeta * rep_penalty,
      -neg_penalty
    ),
    stringsAsFactors = FALSE
  )
  
  total <- sum(breakdown$weighted)
  breakdown$pct_contribution <- breakdown$weighted / abs(total) * 100
  
  return(list(
    total = total,
    breakdown = breakdown
  ))
}

# print score explanation
print_explanation <- function(explanation, item_title = "") {
  cat(sprintf("Score Explanation%s\n",
              if (nchar(item_title) > 0) paste0(": ", item_title) else ""))
  cat(sprintf("  Total Score: %.4f\n\n", explanation$total))
  
  for (i in seq_len(nrow(explanation$breakdown))) {
    row <- explanation$breakdown[i, ]
    bar_len <- max(0, round(abs(row$pct_contribution) / 5))
    bar <- paste(rep("█", bar_len), collapse = "")
    sign <- if (row$weighted >= 0) "+" else "-"
    
    cat(sprintf("  %-22s  raw=%6.3f  weighted=%s%6.3f  %s\n",
                row$component, row$raw_value, sign, 
                abs(row$weighted), bar))
  }
}

# learn scoring coefficients using grid search
# this is a simple approach — try many coefficient combinations and
# pick the one that maximizes a target metric on held-out data
learn_coefficients <- function(train_states, test_interactions, catalogue,
                                compounds_list = NULL,
                                momentum_list = NULL,
                                hierarchy = NULL,
                                metric = "ndcg",
                                k = 10,
                                n_grid = 10,
                                seed = 42) {
  set.seed(seed)
  
  # define grid of coefficient values
  # they should sum to approximately 1 (but we normalise anyway)
  param_grid <- expand.grid(
    alpha   = seq(0.1, 0.5, length.out = n_grid),
    beta    = seq(0.0, 0.3, length.out = max(3, n_grid %/% 2)),
    gamma   = seq(0.0, 0.3, length.out = max(3, n_grid %/% 2)),
    delta   = seq(0.0, 0.2, length.out = max(3, n_grid %/% 3)),
    epsilon = seq(0.0, 0.3, length.out = max(3, n_grid %/% 2)),
    zeta    = seq(0.0, 0.2, length.out = max(3, n_grid %/% 3))
  )
  
  # subsample if grid is too large
  if (nrow(param_grid) > 500) {
    param_grid <- param_grid[sample(nrow(param_grid), 500), ]
  }
  
  # normalise each row to sum to ~1
  for (i in seq_len(nrow(param_grid))) {
    total <- sum(param_grid[i, c("alpha", "beta", "gamma", "delta", "epsilon")])
    if (total > 0) {
      param_grid[i, c("alpha", "beta", "gamma", "delta", "epsilon")] <- 
        param_grid[i, c("alpha", "beta", "gamma", "delta", "epsilon")] / total
    }
  }
  
  best_score <- -Inf
  best_config <- NULL
  
  message(sprintf("searching %d coefficient combinations...", nrow(param_grid)))
  
  for (i in seq_len(nrow(param_grid))) {
    config <- get_scoring_config()
    config$alpha   <- param_grid$alpha[i]
    config$beta    <- param_grid$beta[i]
    config$gamma   <- param_grid$gamma[i]
    config$delta   <- param_grid$delta[i]
    config$epsilon <- param_grid$epsilon[i]
    config$zeta    <- param_grid$zeta[i]
    
    # evaluate on test data
    # (simplified — in the real evaluation we'd use proper metrics)
    # for now, just compute average score of items the user actually interacted with
    total_metric <- 0
    n_users <- 0
    
    for (uid in names(train_states)) {
      state <- train_states[[uid]]
      user_test <- test_interactions[sapply(test_interactions, 
                                           function(x) x$user_id == uid)]
      
      if (length(user_test) == 0) next
      
      scored <- score_all_items(state, catalogue, config = config)
      
      # check if the test items appear in the top-K
      test_item_ids <- sapply(user_test, function(x) x$item_id)
      
      if (nrow(scored) == 0) next
      
      top_ids <- head(scored$item_id, k)
      hits <- sum(test_item_ids %in% top_ids)
      total_metric <- total_metric + hits / min(k, length(test_item_ids))
      n_users <- n_users + 1
    }
    
    avg_metric <- if (n_users > 0) total_metric / n_users else 0
    
    if (avg_metric > best_score) {
      best_score <- avg_metric
      best_config <- config
    }
    
    if (i %% 50 == 0) {
      message(sprintf("  %d/%d tested, best so far: %.4f", 
                      i, nrow(param_grid), best_score))
    }
  }
  
  message(sprintf("best coefficients found (score=%.4f):", best_score))
  message(sprintf("  alpha=%.3f beta=%.3f gamma=%.3f delta=%.3f epsilon=%.3f zeta=%.3f",
                  best_config$alpha, best_config$beta, best_config$gamma,
                  best_config$delta, best_config$epsilon, best_config$zeta))
  
  return(list(
    config = best_config,
    score = best_score
  ))
}
