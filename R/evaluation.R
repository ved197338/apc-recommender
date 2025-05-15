# APC Engine — Evaluation metrics (NDCG, Recall, Diversity, Adaptation Speed)

#   - Novelty
#   - Adaptation Speed (APC-specific)
#

###############################################################################

source("R/utils.R")

# Precision@K
# fraction of recommended items that are relevant
precision_at_k <- function(recommended, relevant, k = 10) {
  top_k_recs <- head(recommended, k)
  n_relevant <- sum(top_k_recs %in% relevant)
  return(n_relevant / k)
}

# Recall@K
# fraction of relevant items that are recommended
recall_at_k <- function(recommended, relevant, k = 10) {
  if (length(relevant) == 0) return(0)
  
  top_k_recs <- head(recommended, k)
  n_relevant <- sum(top_k_recs %in% relevant)
  return(n_relevant / length(relevant))
}

# NDCG@K — Normalised Discounted Cumulative Gain
# position-aware metric: items at the top matter more
ndcg_at_k <- function(recommended, relevant, k = 10) {
  top_k_recs <- head(recommended, k)
  
  # compute DCG
  dcg <- 0
  for (i in seq_along(top_k_recs)) {
    if (top_k_recs[i] %in% relevant) {
      dcg <- dcg + 1 / log2(i + 1)
    }
  }
  
  # compute ideal DCG (all relevant items at the top)
  n_rel <- min(length(relevant), k)
  idcg <- sum(1 / log2(seq_len(n_rel) + 1))
  
  if (idcg == 0) return(0)
  
  return(dcg / idcg)
}

# Hit Rate@K
# 1 if at least one relevant item is in the top-K, 0 otherwise
hit_rate_at_k <- function(recommended, relevant, k = 10) {
  top_k_recs <- head(recommended, k)
  return(as.numeric(any(top_k_recs %in% relevant)))
}

# MRR — Mean Reciprocal Rank
# 1/rank of the first relevant item in the recommendation list
mrr <- function(recommended, relevant) {
  for (i in seq_along(recommended)) {
    if (recommended[i] %in% relevant) {
      return(1 / i)
    }
  }
  return(0)
}

# catalogue coverage
# fraction of items in the catalogue that appear in ANY recommendation list
catalogue_coverage <- function(all_recommendations, catalogue_size) {
  unique_recommended <- unique(unlist(all_recommendations))
  return(length(unique_recommended) / catalogue_size)
}

# intra-list diversity (ILD)
# average pairwise dissimilarity within a recommendation list
intra_list_diversity <- function(recommendations, item_codes) {
  n <- length(recommendations)
  if (n < 2) return(0)
  
  total_dissim <- 0
  n_pairs <- 0
  
  for (i in 1:(n-1)) {
    code_i <- item_codes[[recommendations[i]]]
    if (is.null(code_i)) next
    
    for (j in (i+1):n) {
      code_j <- item_codes[[recommendations[j]]]
      if (is.null(code_j)) next
      
      sim <- cosine_similarity(code_i, code_j)
      total_dissim <- total_dissim + (1 - sim)
      n_pairs <- n_pairs + 1
    }
  }
  
  if (n_pairs == 0) return(0)
  return(total_dissim / n_pairs)
}

# novelty — average self-information of recommended items
# popular items have low novelty, rare items have high novelty
recommendation_novelty <- function(recommendations, item_popularity) {
  # item_popularity should be a named vector with popularity counts/scores
  total_pop <- sum(item_popularity)
  
  novelties <- sapply(recommendations, function(item_id) {
    pop <- if (item_id %in% names(item_popularity)) {
      item_popularity[item_id]
    } else {
      1  # unseen items are maximally novel... well, close to it
    }
    
    # self-information: -log2(p)
    p <- pop / total_pop
    return(-log2(max(p, 1e-10)))
  })
  
  return(mean(novelties))
}

# evaluate a recommendation algorithm on a set of users
# returns a comprehensive metrics summary
evaluate_algorithm <- function(algorithm_fn, user_states, test_interactions,
                                catalogue, k_values = c(5, 10, 20),
                                verbose = FALSE) {
  # group test interactions by user
  user_test_items <- list()
  for (interaction in test_interactions) {
    uid <- interaction$user_id
    if (is.null(user_test_items[[uid]])) {
      user_test_items[[uid]] <- character(0)
    }
    # only positive interactions count as "relevant"
    if (interaction$type %in% c("watch", "like", "save", "complete", "replay")) {
      user_test_items[[uid]] <- unique(c(user_test_items[[uid]], interaction$item_id))
    }
  }
  
  # compute item popularity from test interactions
  item_pop <- table(sapply(test_interactions, function(i) i$item_id))
  
  # get item codes for diversity computation
  item_codes <- lapply(catalogue$items, function(item) item$code)
  
  # store all recommendation lists for coverage
  all_recs <- list()
  
  # initialise metric accumulators
  metrics <- list()
  for (k in k_values) {
    metrics[[paste0("k_", k)]] <- list(
      precision = numeric(0),
      recall    = numeric(0),
      ndcg      = numeric(0),
      hit_rate  = numeric(0),
      mrr_vals  = numeric(0),
      diversity = numeric(0),
      novelty   = numeric(0)
    )
  }
  
  n_users <- length(user_states)
  
  for (i in seq_along(user_states)) {
    uid <- names(user_states)[i]
    state <- user_states[[uid]]
    
    if (verbose && i %% 10 == 0) {
      message(sprintf("  evaluating user %d/%d", i, n_users))
    }
    
    # get relevant items for this user
    relevant <- user_test_items[[uid]]
    if (is.null(relevant) || length(relevant) == 0) next
    
    # get recommendations from the algorithm
    recs <- algorithm_fn(state, catalogue)
    rec_ids <- recs$item_id
    
    all_recs[[uid]] <- rec_ids
    
    # compute metrics for each k
    for (k in k_values) {
      k_key <- paste0("k_", k)
      top_ids <- head(rec_ids, k)
      
      metrics[[k_key]]$precision <- c(metrics[[k_key]]$precision,
                                       precision_at_k(rec_ids, relevant, k))
      metrics[[k_key]]$recall <- c(metrics[[k_key]]$recall,
                                    recall_at_k(rec_ids, relevant, k))
      metrics[[k_key]]$ndcg <- c(metrics[[k_key]]$ndcg,
                                  ndcg_at_k(rec_ids, relevant, k))
      metrics[[k_key]]$hit_rate <- c(metrics[[k_key]]$hit_rate,
                                      hit_rate_at_k(rec_ids, relevant, k))
      metrics[[k_key]]$mrr_vals <- c(metrics[[k_key]]$mrr_vals,
                                      mrr(rec_ids, relevant))
      metrics[[k_key]]$diversity <- c(metrics[[k_key]]$diversity,
                                       intra_list_diversity(top_ids, item_codes))
      metrics[[k_key]]$novelty <- c(metrics[[k_key]]$novelty,
                                     recommendation_novelty(top_ids, item_pop))
    }
  }
  
  # aggregate metrics
  results <- list()
  for (k in k_values) {
    k_key <- paste0("k_", k)
    m <- metrics[[k_key]]
    
    results[[k_key]] <- data.frame(
      k = k,
      precision = mean(m$precision, na.rm = TRUE),
      recall    = mean(m$recall, na.rm = TRUE),
      ndcg      = mean(m$ndcg, na.rm = TRUE),
      hit_rate  = mean(m$hit_rate, na.rm = TRUE),
      mrr       = mean(m$mrr_vals, na.rm = TRUE),
      diversity = mean(m$diversity, na.rm = TRUE),
      novelty   = mean(m$novelty, na.rm = TRUE),
      n_users   = length(m$precision),
      stringsAsFactors = FALSE
    )
  }
  
  # catalogue coverage
  coverage <- catalogue_coverage(all_recs, catalogue$n_items)
  
  combined <- do.call(rbind, results)
  rownames(combined) <- NULL
  combined$coverage <- coverage
  
  return(combined)
}

# adaptation speed measurement
# this is THE key experiment for APC: how fast does it adapt when
# a user's preferences change?
#
# we measure the number of interactions needed before the top-K
# recommendations reflect the new preference
measure_adaptation_speed <- function(algorithm_fn, user_state, catalogue,
                                      old_preference_items,
                                      new_preference_items,
                                      interactions_after_drift,
                                      k = 10,
                                      check_interval = 5) {
  # at each check point, see if the new preference items are in top-K
  state <- user_state
  adaptation_curve <- data.frame(
    n_interactions    = integer(0),
    old_pref_in_topk  = numeric(0),
    new_pref_in_topk  = numeric(0),
    stringsAsFactors  = FALSE
  )
  
  for (i in seq_along(interactions_after_drift)) {
    interaction <- interactions_after_drift[[i]]
    
    # update state (assuming the algorithm_fn handles this)
    # this is simplified — in practice you'd need the full update pipeline
    
    if (i %% check_interval == 0 || i == 1 || i == length(interactions_after_drift)) {
      # get recommendations
      recs <- algorithm_fn(state, catalogue)
      top_ids <- head(recs$item_id, k)
      
      # check presence of old and new preference items
      old_fraction <- sum(top_ids %in% old_preference_items) / 
                      min(k, length(old_preference_items))
      new_fraction <- sum(top_ids %in% new_preference_items) /
                      min(k, length(new_preference_items))
      
      adaptation_curve <- rbind(adaptation_curve, data.frame(
        n_interactions = i,
        old_pref_in_topk = old_fraction,
        new_pref_in_topk = new_fraction,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # find adaptation point — when new_pref first exceeds old_pref
  adaptation_point <- NA
  for (i in seq_len(nrow(adaptation_curve))) {
    if (adaptation_curve$new_pref_in_topk[i] > 
        adaptation_curve$old_pref_in_topk[i]) {
      adaptation_point <- adaptation_curve$n_interactions[i]
      break
    }
  }
  
  return(list(
    curve = adaptation_curve,
    adaptation_point = adaptation_point,
    final_new_fraction = tail(adaptation_curve$new_pref_in_topk, 1),
    final_old_fraction = tail(adaptation_curve$old_pref_in_topk, 1)
  ))
}

# baseline algorithms for comparison

# 1. popularity baseline — recommend most popular items
popularity_baseline <- function(interaction_history, catalogue, k = 50) {
  # count interactions per item
  item_counts <- table(sapply(interaction_history, function(i) i$item_id))
  sorted_items <- names(sort(item_counts, decreasing = TRUE))
  
  # return as recommendation function
  recommend_fn <- function(user_state, catalogue) {
    data.frame(
      item_id = head(sorted_items, k),
      score = seq(k, 1, length.out = min(k, length(sorted_items))),
      title = sapply(head(sorted_items, k), function(id) {
        if (id %in% names(catalogue$items)) catalogue$items[[id]]$title else id
      }),
      stringsAsFactors = FALSE
    )
  }
  
  return(recommend_fn)
}

# 2. content-based cosine similarity
content_based_cosine <- function(k = 50) {
  recommend_fn <- function(user_state, catalogue) {
    prefs <- get_combined_preferences(user_state)
    
    if (length(prefs) == 0) {
      # cold start — return random items
      ids <- sample(names(catalogue$items), min(k, catalogue$n_items))
      return(data.frame(
        item_id = ids,
        score = rep(0, length(ids)),
        title = sapply(ids, function(id) catalogue$items[[id]]$title),
        stringsAsFactors = FALSE
      ))
    }
    
    scores <- sapply(catalogue$items, function(item) {
      cosine_similarity(prefs, item$code)
    })
    
    sorted <- sort(scores, decreasing = TRUE)
    top_ids <- names(sorted)[1:min(k, length(sorted))]
    
    data.frame(
      item_id = top_ids,
      score = sorted[top_ids],
      title = sapply(top_ids, function(id) catalogue$items[[id]]$title),
      stringsAsFactors = FALSE
    )
  }
  
  return(recommend_fn)
}

# 3. user-based collaborative filtering (simplified)
# in a real system this would use user-user similarity
# here we approximate it since we're in a synthetic setting
user_cf <- function(all_states, k = 50) {
  recommend_fn <- function(user_state, catalogue) {
    uid <- user_state$user_id
    user_prefs <- get_combined_preferences(user_state)
    
    if (length(user_prefs) == 0 || length(all_states) < 2) {
      return(content_based_cosine(k)(user_state, catalogue))
    }
    
    # find similar users
    user_sims <- sapply(all_states, function(other_state) {
      if (other_state$user_id == uid) return(-1)
      other_prefs <- get_combined_preferences(other_state)
      cosine_similarity(user_prefs, other_prefs)
    })
    
    # top 10 similar users
    sorted_users <- sort(user_sims, decreasing = TRUE)
    top_users <- names(sorted_users)[1:min(10, length(sorted_users))]
    top_user_sims <- sorted_users[top_users]
    
    # aggregate their preferences
    agg_prefs <- numeric(0)
    for (i in seq_along(top_users)) {
      sim_uid <- top_users[i]
      sim <- top_user_sims[i]
      
      if (sim <= 0) next
      
      other_prefs <- get_combined_preferences(all_states[[sim_uid]])
      agg_prefs <- sparse_add(agg_prefs, sparse_multiply(other_prefs, sim))
    }
    
    if (length(agg_prefs) == 0) {
      return(content_based_cosine(k)(user_state, catalogue))
    }
    
    # score items based on aggregated preferences
    scores <- sapply(catalogue$items, function(item) {
      cosine_similarity(agg_prefs, item$code)
    })
    
    sorted <- sort(scores, decreasing = TRUE)
    top_ids <- names(sorted)[1:min(k, length(sorted))]
    
    data.frame(
      item_id = top_ids,
      score = sorted[top_ids],
      title = sapply(top_ids, function(id) catalogue$items[[id]]$title),
      stringsAsFactors = FALSE
    )
  }
  
  return(recommend_fn)
}

# 4. matrix factorisation (simplified NMF-like approach)
# this is a very basic version — just to have something to compare against
matrix_factorisation <- function(interaction_matrix, n_factors = 20,
                                  n_iterations = 50, learning_rate = 0.01,
                                  reg = 0.02, catalogue = NULL) {
  # interaction_matrix: users x items
  n_users <- nrow(interaction_matrix)
  n_items <- ncol(interaction_matrix)
  
  # initialise factor matrices
  set.seed(42)
  P <- matrix(rnorm(n_users * n_factors, 0, 0.1), n_users, n_factors)
  Q <- matrix(rnorm(n_items * n_factors, 0, 0.1), n_items, n_factors)
  
  rownames(P) <- rownames(interaction_matrix)
  rownames(Q) <- colnames(interaction_matrix)
  
  # SGD to minimise reconstruction error
  non_zero <- which(interaction_matrix > 0, arr.ind = TRUE)
  
  for (iter in seq_len(n_iterations)) {
    # shuffle
    perm <- sample(nrow(non_zero))
    total_error <- 0
    
    for (idx in perm) {
      u <- non_zero[idx, 1]
      i <- non_zero[idx, 2]
      
      r_ui <- interaction_matrix[u, i]
      pred <- sum(P[u, ] * Q[i, ])
      error <- r_ui - pred
      
      total_error <- total_error + error^2
      
      # update
      P[u, ] <- P[u, ] + learning_rate * (error * Q[i, ] - reg * P[u, ])
      Q[i, ] <- Q[i, ] + learning_rate * (error * P[u, ] - reg * Q[i, ])
    }
    
    rmse <- sqrt(total_error / nrow(non_zero))
  }
  
  # return recommendation function
  recommend_fn <- function(user_state, catalogue_arg) {
    uid <- user_state$user_id
    
    if (!(uid %in% rownames(P))) {
      # fallback for unknown users
      return(content_based_cosine(50)(user_state, catalogue_arg))
    }
    
    user_factors <- P[uid, ]
    scores <- Q %*% user_factors
    item_ids <- rownames(Q)
    named_scores <- setNames(as.numeric(scores), item_ids)
    
    sorted <- sort(named_scores, decreasing = TRUE)
    top_ids <- names(sorted)[1:min(50, length(sorted))]
    
    data.frame(
      item_id = top_ids,
      score = sorted[top_ids],
      title = sapply(top_ids, function(id) {
        if (id %in% names(catalogue_arg$items)) {
          catalogue_arg$items[[id]]$title
        } else id
      }),
      stringsAsFactors = FALSE
    )
  }
  
  return(list(
    recommend_fn = recommend_fn,
    P = P,
    Q = Q,
    final_rmse = rmse
  ))
}

# build interaction matrix from interaction logs
build_interaction_matrix <- function(all_interactions, user_ids, item_ids,
                                      positive_only = TRUE) {
  mat <- matrix(0, length(user_ids), length(item_ids),
                dimnames = list(user_ids, item_ids))
  
  positive_types <- c("watch", "like", "save", "complete", "replay", "comment")
  
  for (interaction in all_interactions) {
    uid <- interaction$user_id
    iid <- interaction$item_id
    
    if (!(uid %in% user_ids) || !(iid %in% item_ids)) next
    
    if (positive_only && !(interaction$type %in% positive_types)) next
    
    # weight by interaction type
    weight <- switch(interaction$type,
      "watch" = 1,
      "like" = 2,
      "save" = 2.5,
      "complete" = 1.5,
      "replay" = 2.5,
      "comment" = 2,
      "click" = 0.5,
      "skip" = -0.5,
      "dislike" = -2,
      0.5
    )
    
    mat[uid, iid] <- mat[uid, iid] + weight
  }
  
  return(mat)
}

# print evaluation results nicely
print_evaluation_results <- function(results, algorithm_name = "") {
  cat(sprintf("\nEvaluation Results: %s\n\n", algorithm_name))
  
  cat(sprintf("  %-6s  %-8s  %-8s  %-8s  %-8s  %-8s  %-8s  %-8s  %-8s\n",
              "K", "Prec", "Recall", "NDCG", "HitRate", "MRR", "Div", "Nov", "Cov"))
  cat(paste(rep("-", 82), collapse = ""), "\n")
  
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    cat(sprintf("  %-6d  %-8.4f  %-8.4f  %-8.4f  %-8.4f  %-8.4f  %-8.4f  %-8.4f  %-8.4f\n",
                row$k, row$precision, row$recall, row$ndcg,
                row$hit_rate, row$mrr, row$diversity, row$novelty, row$coverage))
  }
  cat("\n")
}

# compare multiple algorithms side by side
compare_algorithms <- function(algorithm_results) {
  # algorithm_results is a named list of evaluation data frames
  
  cat("\nAlgorithm Comparison\n\n")
  
  # for each K value, show all algorithms
  all_k <- unique(unlist(lapply(algorithm_results, function(r) r$k)))
  
  for (k in sort(all_k)) {
    cat(sprintf("K = %d\n", k))
    cat(sprintf("  %-20s  %-8s  %-8s  %-8s  %-8s  %-8s\n",
                "Algorithm", "Prec", "Recall", "NDCG", "Div", "Nov"))
    cat(paste(rep("-", 66), collapse = ""), "\n")
    
    for (alg_name in names(algorithm_results)) {
      res <- algorithm_results[[alg_name]]
      row <- res[res$k == k, ]
      
      if (nrow(row) == 0) next
      
      cat(sprintf("  %-20s  %-8.4f  %-8.4f  %-8.4f  %-8.4f  %-8.4f\n",
                  alg_name, row$precision, row$recall, row$ndcg,
                  row$diversity, row$novelty))
    }
    cat("\n")
  }
}

# plot comparison charts
plot_algorithm_comparison <- function(algorithm_results, 
                                       metrics = c("ndcg", "precision", "diversity"),
                                       output_file = NULL) {
  if (!is.null(output_file)) {
    png(output_file, width = 1000, height = 400 * length(metrics), res = 100)
  }
  
  par(mfrow = c(length(metrics), 1), mar = c(4, 4, 3, 1))
  
  alg_names <- names(algorithm_results)
  colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6",
              "#1abc9c", "#e67e22")
  
  for (metric in metrics) {
    # get all k values
    all_k <- sort(unique(unlist(lapply(algorithm_results, function(r) r$k))))
    
    # set up plot
    y_range <- range(unlist(lapply(algorithm_results, function(r) r[[metric]])),
                     na.rm = TRUE)
    y_range[1] <- max(0, y_range[1] - 0.05)
    y_range[2] <- min(1, y_range[2] + 0.05)
    
    plot(NULL, xlim = range(all_k), ylim = y_range,
         xlab = "K", ylab = toupper(metric),
         main = sprintf("%s by K", toupper(metric)),
         bty = "l", las = 1)
    
    grid(col = "gray90")
    
    for (i in seq_along(alg_names)) {
      res <- algorithm_results[[alg_names[i]]]
      lines(res$k, res[[metric]], col = colors[i], lwd = 2, type = "b",
            pch = 20)
    }
    
    legend("bottomright", alg_names, col = colors[seq_along(alg_names)],
           lwd = 2, pch = 20, cex = 0.7, bg = "white")
  }
  
  if (!is.null(output_file)) {
    dev.off()
    message(sprintf("comparison plot saved to %s", output_file))
  }
}

# plot adaptation speed comparison
plot_adaptation_speed <- function(adaptation_results, output_file = NULL) {
  if (!is.null(output_file)) {
    png(output_file, width = 800, height = 500, res = 100)
  }
  
  alg_names <- names(adaptation_results)
  colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6")
  
  # find global x and y ranges
  x_range <- c(0, max(unlist(lapply(adaptation_results, function(r) {
    max(r$curve$n_interactions)
  }))))
  
  plot(NULL, xlim = x_range, ylim = c(0, 1),
       xlab = "Interactions After Preference Drift",
       ylab = "Fraction of New Preference in Top-K",
       main = "Adaptation Speed Comparison",
       bty = "l", las = 1)
  
  grid(col = "gray90")
  
  for (i in seq_along(alg_names)) {
    curve <- adaptation_results[[alg_names[i]]]$curve
    lines(curve$n_interactions, curve$new_pref_in_topk,
          col = colors[i], lwd = 2)
    
    # mark adaptation point
    ap <- adaptation_results[[alg_names[i]]]$adaptation_point
    if (!is.na(ap)) {
      abline(v = ap, col = colors[i], lty = 2, lwd = 1)
    }
  }
  
  legend("bottomright", alg_names, col = colors[seq_along(alg_names)],
         lwd = 2, cex = 0.7, bg = "white")
  
  if (!is.null(output_file)) {
    dev.off()
    message(sprintf("adaptation plot saved to %s", output_file))
  }
}
