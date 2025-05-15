# APC Engine — Exploration strategies (UCB, Hierarchical, Epsilon-greedy)

#   4. momentum-based: explore in the direction of emerging interests
#

###############################################################################

source("R/utils.R")
source("R/hierarchy.R")

# exploration configuration
get_exploration_config <- function() {
  config <- list(
    # which strategy to use
    strategy = "hierarchical",  # "epsilon", "ucb", "hierarchical", "momentum"
    
    # epsilon-greedy parameters
    epsilon = 0.1,  # probability of exploring
    epsilon_decay = 0.999,  # decay epsilon over time
    min_epsilon = 0.02,
    
    # UCB parameters
    ucb_c = 1.5,    # exploration coefficient
    
    # hierarchical exploration
    hier_distance = 2,  # how far to explore in the hierarchy
    hier_weight = 0.3,  # weight for hierarchically adjacent items
    
    # momentum-based exploration
    momentum_threshold = 0.02,  # minimum momentum to trigger exploration
    momentum_explore_weight = 0.4,
    
    # general settings
    max_explore_fraction = 0.3,  # at most 30% of recommendations are exploratory
    diversity_target = 0.5       # target diversity (entropy) in recommendations
  )
  
  return(config)
}

# apply exploration to a recommendation list
# takes the base recommendations and mixes in exploratory items
apply_exploration <- function(base_recommendations, user_state, catalogue,
                               hierarchy = NULL, momentum = NULL,
                               config = NULL, k = 10) {
  if (is.null(config)) config <- get_exploration_config()
  
  result <- switch(config$strategy,
    "epsilon"       = epsilon_greedy_explore(base_recommendations, catalogue, 
                                             config, k),
    "ucb"           = ucb_explore(base_recommendations, user_state, catalogue,
                                  config, k),
    "hierarchical"  = hierarchical_explore(base_recommendations, user_state,
                                            catalogue, hierarchy, config, k),
    "momentum"      = momentum_explore(base_recommendations, user_state,
                                        catalogue, momentum, config, k),
    base_recommendations  # fallback — no exploration
  )
  
  return(result)
}

# epsilon-greedy exploration
# with probability epsilon, replace a recommendation with a random item
# simplest approach, good baseline
epsilon_greedy_explore <- function(base_recs, catalogue, config, k) {
  n_explore <- max(1, round(k * config$epsilon))
  n_exploit <- k - n_explore
  
  # keep top (k - n_explore) from base recommendations
  exploit_items <- head(base_recs, n_exploit)
  
  # pick random items not in the exploit set
  all_items <- names(catalogue$items)
  available <- setdiff(all_items, exploit_items$item_id)
  
  if (length(available) == 0) return(base_recs)
  
  explore_ids <- sample(available, min(n_explore, length(available)))
  
  # create entries for exploratory items
  explore_items <- data.frame(
    item_id = explore_ids,
    score = rep(-0.01, length(explore_ids)),  # low score to indicate exploration
    title = sapply(explore_ids, function(id) catalogue$items[[id]]$title),
    stringsAsFactors = FALSE
  )
  
  # interleave — put exploratory items at positions 3, 6, 9, etc.
  result <- exploit_items
  for (i in seq_len(nrow(explore_items))) {
    insert_pos <- min(nrow(result) + 1, 2 + (i - 1) * 3)
    if (insert_pos <= nrow(result)) {
      result <- rbind(
        result[1:(insert_pos-1), ],
        explore_items[i, ],
        result[insert_pos:nrow(result), ]
      )
    } else {
      result <- rbind(result, explore_items[i, ])
    }
  }
  
  return(head(result, k))
}

# UCB-based exploration
# score = estimated_value + c * sqrt(log(t) / n_i)
# items that have been shown less get a higher exploration bonus
ucb_explore <- function(base_recs, user_state, catalogue, config, k) {
  total_impressions <- user_state$n_interactions
  if (total_impressions == 0) total_impressions <- 1
  
  # count how many times each item has been shown/interacted with
  item_counts <- table(sapply(user_state$interaction_log, function(i) i$item_id))
  
  # compute UCB bonus for each item in the catalogue
  all_items <- names(catalogue$items)
  ucb_scores <- numeric(length(all_items))
  names(ucb_scores) <- all_items
  
  for (item_id in all_items) {
    n_i <- if (item_id %in% names(item_counts)) item_counts[item_id] else 0
    
    if (n_i == 0) {
      # never seen — maximum exploration bonus
      ucb_scores[item_id] <- config$ucb_c * sqrt(log(total_impressions + 1))
    } else {
      ucb_scores[item_id] <- config$ucb_c * sqrt(log(total_impressions) / n_i)
    }
  }
  
  # combine base scores with UCB bonus
  if (nrow(base_recs) > 0) {
    base_scores <- setNames(base_recs$score, base_recs$item_id)
    
    # normalise both to [0, 1]
    base_norm <- min_max_scale(base_scores)
    names(base_norm) <- names(base_scores)
    
    ucb_norm <- min_max_scale(ucb_scores)
    
    # combined score
    combined <- numeric(length(all_items))
    names(combined) <- all_items
    
    for (item_id in all_items) {
      base_val <- if (item_id %in% names(base_norm)) base_norm[item_id] else 0
      combined[item_id] <- (1 - config$epsilon) * base_val + 
                            config$epsilon * ucb_norm[item_id]
    }
    
    # sort and return top-k
    sorted <- sort(combined, decreasing = TRUE)
    top_ids <- names(sorted)[1:min(k, length(sorted))]
    
    result <- data.frame(
      item_id = top_ids,
      score = sorted[top_ids],
      title = sapply(top_ids, function(id) catalogue$items[[id]]$title),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  return(base_recs)
}

# hierarchical exploration
# find items that are adjacent in the content hierarchy to the user's
# existing preferences, but not directly matching them
#
# this is the most interesting exploration strategy for APC because
# it leverages the hierarchical structure we've built
hierarchical_explore <- function(base_recs, user_state, catalogue,
                                  hierarchy, config, k) {
  if (is.null(hierarchy)) {
    # fallback to epsilon-greedy if no hierarchy
    return(epsilon_greedy_explore(base_recs, catalogue, config, k))
  }
  
  # get user's top preference dimensions
  prefs <- get_combined_preferences(user_state)
  top_prefs <- top_k(prefs, 5)
  
  if (length(top_prefs$names) == 0) {
    return(epsilon_greedy_explore(base_recs, catalogue, config, k))
  }
  
  # find adjacent dimensions in the hierarchy
  adjacent_dims <- find_adjacent_dimensions(top_prefs$names, hierarchy,
                                             max_distance = config$hier_distance)
  
  # remove dimensions the user already has strong preferences for
  strong_prefs <- names(prefs[prefs > quantile(prefs[prefs > 0], 0.7)])
  explore_dims <- setdiff(adjacent_dims, strong_prefs)
  
  if (length(explore_dims) == 0) {
    return(base_recs)
  }
  
  # find items that have high values in these adjacent dimensions
  n_explore <- max(1, round(k * config$max_explore_fraction))
  
  explore_scores <- sapply(catalogue$items, function(item) {
    # score based on how much the item covers the adjacent dimensions
    coverage <- sum(sapply(explore_dims, function(d) {
      if (d %in% names(item$code)) item$code[d] else 0
    }))
    
    # but also want some overlap with existing preferences (not totally random)
    overlap <- cosine_similarity(prefs, item$code)
    
    # balance: adjacent exploration with some preference connection
    config$hier_weight * coverage + (1 - config$hier_weight) * overlap
  })
  
  # exclude items already in base recommendations
  available <- setdiff(names(explore_scores), base_recs$item_id)
  explore_scores <- explore_scores[available]
  
  if (length(explore_scores) == 0) return(base_recs)
  
  # pick top exploratory items
  sorted_explore <- sort(explore_scores, decreasing = TRUE)
  explore_ids <- names(sorted_explore)[1:min(n_explore, length(sorted_explore))]
  
  explore_items <- data.frame(
    item_id = explore_ids,
    score = sorted_explore[explore_ids],
    title = sapply(explore_ids, function(id) catalogue$items[[id]]$title),
    stringsAsFactors = FALSE
  )
  
  # combine: take top (k - n_explore) from base, add exploratory items
  n_base <- k - nrow(explore_items)
  result <- rbind(
    head(base_recs, n_base),
    explore_items
  )
  
  return(head(result, k))
}

# momentum-based exploration
# explore in the direction of emerging interests
# if the user is showing early signs of interest in something,
# proactively recommend more of it
momentum_explore <- function(base_recs, user_state, catalogue,
                              momentum, config, k) {
  if (is.null(momentum) || length(momentum) == 0) {
    return(base_recs)
  }
  
  # find emerging interests
  emerging <- list()
  for (dim_name in names(momentum)) {
    mom <- momentum[[dim_name]]
    if (mom$direction == "rising" && mom$velocity > config$momentum_threshold &&
        mom$confidence > 0.3) {
      emerging[[dim_name]] <- mom
    }
  }
  
  if (length(emerging) == 0) return(base_recs)
  
  # find items that match emerging interests
  emerging_dims <- names(emerging)
  n_explore <- max(1, round(k * config$max_explore_fraction))
  
  explore_scores <- sapply(catalogue$items, function(item) {
    score <- sum(sapply(emerging_dims, function(d) {
      item_val <- if (d %in% names(item$code)) item$code[d] else 0
      item_val * emerging[[d]]$velocity * emerging[[d]]$confidence
    }))
    return(score)
  })
  
  # exclude already recommended items
  available <- setdiff(names(explore_scores), base_recs$item_id)
  explore_scores <- explore_scores[available]
  
  if (length(explore_scores) == 0) return(base_recs)
  
  sorted <- sort(explore_scores, decreasing = TRUE)
  explore_ids <- names(sorted)[1:min(n_explore, length(sorted))]
  
  explore_items <- data.frame(
    item_id = explore_ids,
    score = sorted[explore_ids],
    title = sapply(explore_ids, function(id) catalogue$items[[id]]$title),
    stringsAsFactors = FALSE
  )
  
  n_base <- k - nrow(explore_items)
  result <- rbind(
    head(base_recs, n_base),
    explore_items
  )
  
  return(head(result, k))
}

# measure recommendation diversity
# computes the average pairwise dissimilarity among recommended items
measure_diversity <- function(recommendations, catalogue) {
  n <- nrow(recommendations)
  if (n < 2) return(0)
  
  codes <- lapply(recommendations$item_id, function(id) {
    catalogue$items[[id]]$code
  })
  
  total_dissim <- 0
  n_pairs <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      sim <- cosine_similarity(codes[[i]], codes[[j]])
      total_dissim <- total_dissim + (1 - sim)
      n_pairs <- n_pairs + 1
    }
  }
  
  return(total_dissim / n_pairs)
}

# measure coverage — what fraction of the catalogue's content dimensions
# are represented in the recommendations?
measure_coverage <- function(recommendations, catalogue) {
  all_dims <- catalogue$all_dimensions
  
  recommended_dims <- unique(unlist(lapply(recommendations$item_id, function(id) {
    item <- catalogue$items[[id]]
    names(item$code[item$code > 0.1])
  })))
  
  return(length(recommended_dims) / length(all_dims))
}

# compute exploration metrics — how much exploration happened
compute_exploration_metrics <- function(recommendations, user_state, catalogue) {
  prefs <- get_combined_preferences(user_state)
  if (length(prefs) == 0) {
    return(list(
      diversity = 0,
      coverage = 0,
      avg_novelty = 1,
      exploration_ratio = 1
    ))
  }
  
  # average similarity to user preferences (lower = more exploration)
  avg_sim <- mean(sapply(recommendations$item_id, function(id) {
    item <- catalogue$items[[id]]
    cosine_similarity(prefs, item$code)
  }))
  
  # count items with low preference similarity (exploratory)
  n_explore <- sum(sapply(recommendations$item_id, function(id) {
    item <- catalogue$items[[id]]
    cosine_similarity(prefs, item$code) < 0.3
  }))
  
  return(list(
    diversity = measure_diversity(recommendations, catalogue),
    coverage = measure_coverage(recommendations, catalogue),
    avg_relevance = avg_sim,
    exploration_ratio = n_explore / nrow(recommendations)
  ))
}
