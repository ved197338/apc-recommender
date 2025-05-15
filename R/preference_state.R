# APC Engine — User preference state management and dynamic updates

#   - state snapshots for tracking evolution over time
#   - positive and negative preference separation
#

###############################################################################

source("R/utils.R")
source("R/codes.R")
source("R/interaction_weights.R")
source("R/decay.R")

# create a new user preference state
#
# each user state contains:
#   - positive preferences (things they like)
#   - negative preferences (things they don't like)
#   - the timestamp of the last update
#   - a history of snapshots for tracking evolution
#   - interaction count for statistics
create_user_state <- function(user_id, initial_prefs = NULL) {
  state <- list(
    user_id         = user_id,
    positive_prefs  = if (!is.null(initial_prefs)) initial_prefs 
                      else numeric(0),
    negative_prefs  = numeric(0),
    last_update     = as.numeric(Sys.time()),
    n_interactions  = 0,
    created_at      = as.numeric(Sys.time()),
    history         = list(),   # snapshots
    interaction_log = list()    # keep track of what happened when
  )
  
  class(state) <- "apc_user_state"
  return(state)
}

# print method for user state
print.apc_user_state <- function(x, ...) {
  cat(sprintf("User: %s\n", x$user_id))
  cat(sprintf("  Interactions: %d\n", x$n_interactions))
  cat(sprintf("  Active since: %s\n", 
              format(as.POSIXct(x$created_at, origin = "1970-01-01"), 
                     "%Y-%m-%d")))
  cat(sprintf("  Last update: %s\n",
              format(as.POSIXct(x$last_update, origin = "1970-01-01"),
                     "%Y-%m-%d %H:%M")))
  
  if (length(x$positive_prefs) > 0) {
    cat("\n  Top positive preferences:\n")
    print_vector(x$positive_prefs, top_n = 8)
  }
  
  if (length(x$negative_prefs) > 0 && any(x$negative_prefs != 0)) {
    cat("\n  Top negative preferences:\n")
    print_vector(sort(x$negative_prefs), top_n = 5)
  }
}

# update user preference state with a new interaction
#
# this is THE core function of APC. everything comes together here.
#
# steps:
#   1. compute time delta since last update
#   2. apply decay to existing preferences
#   3. compute interaction weight
#   4. get item content code
#   5. update preference vector
#   6. store snapshot if requested
#
# the function modifies the state in place (well, returns a new copy
# because R doesn't do real in-place mutation, but you know what i mean)
update_preference_state <- function(state, interaction, item_code,
                                    decay_config = NULL,
                                    weight_config = NULL,
                                    registry = NULL,
                                    store_snapshot = TRUE,
                                    adaptive_lambdas = NULL) {
  stopifnot(inherits(state, "apc_user_state"))
  
  # 1. time delta
  delta_t <- interaction$timestamp - state$last_update
  if (delta_t < 0) {
    warning("interaction timestamp is before last update — out of order?")
    delta_t <- 0
  }
  
  # 2. apply decay to existing preferences
  if (delta_t > 0 && length(state$positive_prefs) > 0) {
    decayed <- apply_dual_decay(
      state$positive_prefs, 
      state$negative_prefs,
      delta_t,
      config = decay_config,
      registry = registry,
      adaptive_lambdas = adaptive_lambdas
    )
    state$positive_prefs <- decayed$positive
    state$negative_prefs <- decayed$negative
  }
  
  # 3. compute interaction weight
  weight <- compute_interaction_weight(
    interaction,
    config = weight_config,
    interaction_history = state$interaction_log,
    current_time = interaction$timestamp
  )
  
  # 4. update preference vector based on weight direction
  if (weight >= 0) {
    # positive update — add to positive preferences
    contribution <- item_code * weight
    state$positive_prefs <- sparse_add(state$positive_prefs, contribution)
  } else {
    # negative update — add to negative preferences (with positive magnitude)
    contribution <- item_code * abs(weight)
    state$negative_prefs <- sparse_add(state$negative_prefs, contribution)
  }
  
  # 5. update metadata
  state$last_update <- interaction$timestamp
  state$n_interactions <- state$n_interactions + 1
  
  # keep interaction in the log (but cap it to save memory)
  if (length(state$interaction_log) >= 1000) {
    # keep the most recent 800
    state$interaction_log <- state$interaction_log[
      (length(state$interaction_log) - 799):length(state$interaction_log)
    ]
  }
  state$interaction_log[[length(state$interaction_log) + 1]] <- interaction
  
  # 6. store snapshot if requested
  if (store_snapshot) {
    snapshot <- list(
      timestamp       = interaction$timestamp,
      positive_prefs  = state$positive_prefs,
      negative_prefs  = state$negative_prefs,
      n_interactions  = state$n_interactions,
      interaction_type = interaction$type,
      weight          = weight
    )
    state$history[[length(state$history) + 1]] <- snapshot
  }
  
  return(state)
}

# batch update — process multiple interactions in chronological order
# this is the typical use case: you have a sequence of interactions
# and you want to build up the preference state
batch_update_preferences <- function(state, interactions, catalogue,
                                      decay_config = NULL,
                                      weight_config = NULL,
                                      registry = NULL,
                                      snapshot_interval = 1,
                                      verbose = FALSE) {
  # sort interactions by timestamp
  timestamps <- sapply(interactions, function(i) i$timestamp)
  sorted_idx <- order(timestamps)
  
  n_total <- length(interactions)
  
  for (i in seq_along(sorted_idx)) {
    idx <- sorted_idx[i]
    interaction <- interactions[[idx]]
    
    # get the item's content code
    item <- catalogue$items[[interaction$item_id]]
    if (is.null(item)) {
      if (verbose) {
        message(sprintf("  skipping interaction %d — item %s not in catalogue",
                        i, interaction$item_id))
      }
      next
    }
    
    # decide whether to store a snapshot
    store <- (i %% snapshot_interval == 0) || (i == n_total)
    
    # update the state
    state <- update_preference_state(
      state, interaction, item$code,
      decay_config = decay_config,
      weight_config = weight_config,
      registry = registry,
      store_snapshot = store
    )
    
    if (verbose && i %% 100 == 0) {
      message(sprintf("  processed %d/%d interactions", i, n_total))
    }
  }
  
  return(state)
}

# get the combined preference vector
# merges positive and negative preferences into a single vector
# 
# the combination is: combined = positive - alpha * negative
# where alpha controls how much negative prefs suppress positive ones
get_combined_preferences <- function(state, alpha = 0.5) {
  stopifnot(inherits(state, "apc_user_state"))
  
  combined <- state$positive_prefs
  
  if (length(state$negative_prefs) > 0) {
    # subtract negative preferences scaled by alpha
    for (dim_name in names(state$negative_prefs)) {
      if (dim_name %in% names(combined)) {
        combined[dim_name] <- combined[dim_name] - 
                               alpha * state$negative_prefs[dim_name]
      } else {
        combined[dim_name] <- -alpha * state$negative_prefs[dim_name]
      }
    }
  }
  
  return(combined)
}

# get normalised preference vector (for similarity computations)
get_normalized_preferences <- function(state, alpha = 0.5) {
  combined <- get_combined_preferences(state, alpha)
  return(normalize_vector(combined))
}

# extract preference time series for a specific dimension
# useful for plotting and momentum calculations
get_dimension_history <- function(state, dim_name, type = "positive") {
  if (length(state$history) == 0) {
    return(list(timestamps = numeric(0), values = numeric(0)))
  }
  
  timestamps <- sapply(state$history, function(s) s$timestamp)
  
  values <- sapply(state$history, function(s) {
    prefs <- if (type == "positive") s$positive_prefs else s$negative_prefs
    if (dim_name %in% names(prefs)) prefs[dim_name] else 0
  })
  
  return(list(timestamps = timestamps, values = values))
}

# compute preference diversity
# measures how spread out the user's preferences are across dimensions
# uses entropy — high entropy means diverse preferences
compute_preference_diversity <- function(state, alpha = 0.5) {
  combined <- get_combined_preferences(state, alpha)
  
  if (length(combined) == 0) return(0)
  
  # only consider positive values for entropy
  pos_vals <- combined[combined > 0]
  if (length(pos_vals) == 0) return(0)
  
  # normalise to probability distribution
  probs <- pos_vals / sum(pos_vals)
  
  return(entropy(probs))
}

# get top preferences — most useful for recommendation and debugging
get_top_preferences <- function(state, k = 10, alpha = 0.5) {
  combined <- get_combined_preferences(state, alpha)
  return(top_k(combined, k))
}

# create a user state manager
# handles multiple users and provides batch operations
create_state_manager <- function() {
  manager <- list(
    users = list(),
    n_users = 0
  )
  class(manager) <- "apc_state_manager"
  return(manager)
}

# get or create a user state in the manager
get_user_state <- function(manager, user_id) {
  if (user_id %in% names(manager$users)) {
    return(manager$users[[user_id]])
  }
  
  state <- create_user_state(user_id)
  manager$users[[user_id]] <- state
  manager$n_users <- length(manager$users)
  
  return(list(manager = manager, state = state))
}

# update a user state in the manager
set_user_state <- function(manager, state) {
  manager$users[[state$user_id]] <- state
  manager$n_users <- length(manager$users)
  return(manager)
}

# process all interactions for all users
# sorts by timestamp globally and processes in order
process_all_interactions <- function(manager, interactions, catalogue,
                                     decay_config = NULL,
                                     weight_config = NULL,
                                     registry = NULL,
                                     verbose = FALSE) {
  # group interactions by user
  user_interactions <- list()
  for (interaction in interactions) {
    uid <- interaction$user_id
    if (is.null(user_interactions[[uid]])) {
      user_interactions[[uid]] <- list()
    }
    user_interactions[[uid]][[length(user_interactions[[uid]]) + 1]] <- interaction
  }
  
  # process each user
  n_users <- length(user_interactions)
  for (i in seq_along(user_interactions)) {
    uid <- names(user_interactions)[i]
    user_ints <- user_interactions[[uid]]
    
    if (verbose) {
      message(sprintf("processing user %s (%d/%d) — %d interactions",
                      uid, i, n_users, length(user_ints)))
    }
    
    # get or create user state
    if (uid %in% names(manager$users)) {
      state <- manager$users[[uid]]
    } else {
      state <- create_user_state(uid)
    }
    
    # batch update
    state <- batch_update_preferences(
      state, user_ints, catalogue,
      decay_config = decay_config,
      weight_config = weight_config,
      registry = registry,
      verbose = verbose
    )
    
    manager <- set_user_state(manager, state)
  }
  
  return(manager)
}

# compute preference similarity between two users
# this isn't the main recommendation approach (that's content-based)
# but it's useful for evaluation and comparison
user_preference_similarity <- function(state_a, state_b, alpha = 0.5) {
  prefs_a <- get_combined_preferences(state_a, alpha)
  prefs_b <- get_combined_preferences(state_b, alpha)
  
  return(cosine_similarity(prefs_a, prefs_b))
}

# export user state to a flat data frame for analysis
state_to_dataframe <- function(state) {
  if (length(state$history) == 0) {
    return(data.frame())
  }
  
  rows <- list()
  for (i in seq_along(state$history)) {
    snap <- state$history[[i]]
    prefs <- snap$positive_prefs
    
    for (dim_name in names(prefs)) {
      rows[[length(rows) + 1]] <- data.frame(
        user_id        = state$user_id,
        timestamp      = snap$timestamp,
        snapshot_idx   = i,
        dimension      = dim_name,
        positive_value = prefs[dim_name],
        negative_value = if (dim_name %in% names(snap$negative_prefs))
                           snap$negative_prefs[dim_name] else 0,
        n_interactions = snap$n_interactions,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows) == 0) return(data.frame())
  
  do.call(rbind, rows)
}
