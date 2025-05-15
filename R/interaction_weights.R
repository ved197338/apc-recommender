# APC Engine — Interaction weight calculation and signal mapping

###############################################################################

source("R/utils.R")

# interaction types and their base configurations
# 
# each interaction type has:
#   - base_weight: starting weight before any adjustments
#   - direction: positive/negative/neutral signal
#   - confidence: how confident we are that this interaction reflects
#                 true preference (0 to 1)
#   - min_duration: minimum meaningful duration in seconds (for time-based)
#
# i went back and forth on these values a lot. the current ones are
# based on my reading of recommendation system literature and some
# intuition about what different behaviours mean.
get_interaction_config <- function() {
  config <- list(
# Positive signals
    watch = list(
      base_weight = 1.0,
      direction   = "positive",
      confidence  = 0.7,    # watching alone is moderately informative
      min_duration = 10     # at least 10 seconds of actual watching
    ),
    like = list(
      base_weight = 2.0,
      direction   = "positive",
      confidence  = 0.9,    # explicit signal, high confidence
      min_duration = 0
    ),
    save = list(
      base_weight = 2.5,
      direction   = "positive",
      confidence  = 0.85,
      min_duration = 0
    ),
    share = list(
      base_weight = 3.0,
      direction   = "positive",
      confidence  = 0.95,   # sharing is a very strong signal
      min_duration = 0
    ),
    replay = list(
      base_weight = 2.5,
      direction   = "positive",
      confidence  = 0.9,
      min_duration = 0
    ),
    complete = list(
      base_weight = 1.5,
      direction   = "positive",
      confidence  = 0.75,
      min_duration = 0
    ),
    comment = list(
      base_weight = 2.0,
      direction   = "positive",
      confidence  = 0.85,
      min_duration = 0
    ),
    subscribe = list(
      base_weight = 3.5,
      direction   = "positive",
      confidence  = 0.95,
      min_duration = 0
    ),
    
# Negative signals
    skip = list(
      base_weight = -0.5,
      direction   = "negative",
      confidence  = 0.4,    # low confidence — skipping could be contextual
      min_duration = 0
    ),
    dislike = list(
      base_weight = -2.0,
      direction   = "negative",
      confidence  = 0.9,
      min_duration = 0
    ),
    hide = list(
      base_weight = -2.5,
      direction   = "negative",
      confidence  = 0.95,
      min_duration = 0
    ),
    not_interested = list(
      base_weight = -3.0,
      direction   = "negative",
      confidence  = 0.95,
      min_duration = 0
    ),
    
# Implicit signals
    click = list(
      base_weight = 0.3,
      direction   = "positive",
      confidence  = 0.3,    # clicking means interest but could be clickbait
      min_duration = 0
    ),
    impression = list(
      base_weight = 0.0,
      direction   = "neutral",
      confidence  = 0.05,   # just seeing something means almost nothing
      min_duration = 0
    ),
    dwell = list(
      base_weight = 0.5,
      direction   = "positive",
      confidence  = 0.5,
      min_duration = 5      # dwelling for less than 5 seconds doesn't count
    )
  )
  
  return(config)
}

# create an interaction event
# this records a single user-item interaction with all its signals
create_interaction <- function(user_id, item_id, interaction_type,
                               timestamp = NULL,
                               duration_watched = 0,
                               item_duration = NA,
                               percentage_watched = NA,
                               extra_signals = list()) {
  if (is.null(timestamp)) {
    timestamp <- as.numeric(Sys.time())
  }
  
  # compute percentage watched if we have both durations
  if (is.na(percentage_watched) && !is.na(item_duration) && item_duration > 0) {
    percentage_watched <- min(1.0, duration_watched / item_duration)
  }
  
  interaction <- list(
    user_id             = user_id,
    item_id             = item_id,
    type                = interaction_type,
    timestamp           = timestamp,
    duration_watched    = duration_watched,
    item_duration       = item_duration,
    percentage_watched  = percentage_watched,
    extra_signals       = extra_signals
  )
  
  class(interaction) <- "apc_interaction"
  return(interaction)
}

# compute the interaction weight from an interaction event
#
# this is the main function that turns a raw interaction into a numerical
# weight. the weight gets multiplied by the item's content code vector
# in the preference update step.
#
# the formula incorporates:
#   1. base weight for the interaction type
#   2. completion bonus (for watching-type interactions)
#   3. duration factor (longer engagement = stronger signal)
#   4. repetition boost (repeated consumption amplifies the signal)
#   5. recency factor (more recent interactions get a small boost)
#   6. confidence scaling
#
# returns a single numeric value, can be positive or negative
compute_interaction_weight <- function(interaction, config = NULL,
                                       interaction_history = NULL,
                                       current_time = NULL) {
  if (is.null(config)) {
    config <- get_interaction_config()
  }
  
  if (is.null(current_time)) {
    current_time <- as.numeric(Sys.time())
  }
  
  itype <- interaction$type
  
  # lookup config for this interaction type
  if (!(itype %in% names(config))) {
    warning(sprintf("unknown interaction type '%s', using click defaults", itype))
    itype_config <- config[["click"]]
  } else {
    itype_config <- config[[itype]]
  }
  
  base_w <- itype_config$base_weight
  confidence <- itype_config$confidence
  
# Completion factor
  # for watch-type interactions, how much they watched matters a lot
  # sigmoid curve so there's a sweet spot around 60-80% completion
  completion_factor <- 1.0
  if (!is.na(interaction$percentage_watched)) {
    pct <- interaction$percentage_watched
    
    if (pct < 0.05) {
      # basically didn't watch — this is close to a skip
      completion_factor <- 0.1
    } else if (pct < 0.25) {
      # watched a bit but lost interest
      completion_factor <- 0.3 + 0.4 * (pct / 0.25)
    } else if (pct < 0.75) {
      # decent engagement, linear ramp
      completion_factor <- 0.7 + 0.6 * ((pct - 0.25) / 0.5)
    } else if (pct < 0.95) {
      # good completion
      completion_factor <- 1.3
    } else {
      # watched the whole thing — strong signal
      completion_factor <- 1.5
    }
  }
  
# Duration factor
  # longer content that gets watched = stronger signal
  # but diminishing returns after a certain point
  duration_factor <- 1.0
  if (interaction$duration_watched > 0) {
    # log scale to handle very long videos
    # baseline is 5 minutes (300 seconds)
    duration_factor <- 0.5 + 0.5 * log1p(interaction$duration_watched / 300)
    duration_factor <- min(duration_factor, 2.5)  # cap it
  }
  
# Repetition boost
  # if the user has consumed similar content before, each new interaction
  # is a stronger signal (they're choosing it repeatedly)
  repetition_factor <- 1.0
  if (!is.null(interaction_history)) {
    # count how many times this user interacted with this specific item
    same_item_count <- sum(sapply(interaction_history, function(h) {
      h$item_id == interaction$item_id && 
        h$user_id == interaction$user_id &&
        h$type %in% c("watch", "replay", "complete")
    }))
    
    if (same_item_count > 0) {
      # diminishing returns on repetition
      repetition_factor <- 1.0 + 0.3 * log1p(same_item_count)
    }
  }
  
# Recency boost
  # very recent interactions get a small boost
  # this helps the system respond faster to current behaviour
  recency_factor <- 1.0
  delta_t <- current_time - interaction$timestamp
  if (delta_t < 3600) {
    # within the last hour — give a boost
    recency_factor <- 1.2
  } else if (delta_t < 86400) {
    # within the last day
    recency_factor <- 1.1
  }
  
# Combine everything
  # the final weight is the base weight scaled by all the factors
  # negative interactions get simpler treatment (no completion bonus etc)
  if (itype_config$direction == "negative") {
    # for negative signals, we use base weight and confidence directly
    # completion factor works differently — a quick skip is less negative
    # than watching 40% and then disliking
    neg_engagement_factor <- 1.0
    if (!is.na(interaction$percentage_watched)) {
      pct <- interaction$percentage_watched
      if (pct > 0.3) {
        # watched a fair bit before the negative action — more informative
        neg_engagement_factor <- 1.0 + 0.5 * pct
      }
    }
    
    weight <- base_w * confidence * neg_engagement_factor * recency_factor
  } else {
    weight <- base_w * confidence * completion_factor * 
              duration_factor * repetition_factor * recency_factor
  }
  
  return(weight)
}

# batch compute weights for a list of interactions
compute_batch_weights <- function(interactions, config = NULL,
                                   current_time = NULL) {
  if (is.null(config)) config <- get_interaction_config()
  if (is.null(current_time)) current_time <- as.numeric(Sys.time())
  
  weights <- numeric(length(interactions))
  
  for (i in seq_along(interactions)) {
    # pass previous interactions as history for repetition calculation
    history <- if (i > 1) interactions[1:(i-1)] else NULL
    weights[i] <- compute_interaction_weight(
      interactions[[i]], 
      config = config,
      interaction_history = history,
      current_time = current_time
    )
  }
  
  return(weights)
}

# learnable weight model
#
# this is a placeholder for eventually learning the weight parameters
# from data. right now it just wraps the hand-tuned config, but the
# interface is designed so we can later fit the parameters using
# logistic regression or a small neural net.
#
# the idea is: given an interaction and its outcome (did the user
# engage more with similar content later?), learn what weight to assign.
create_weight_model <- function(method = "fixed") {
  model <- list(
    method = method,
    config = get_interaction_config(),
    fitted = FALSE,
    parameters = list()
  )
  
  if (method == "fixed") {
    model$predict <- function(interaction, history = NULL, current_time = NULL) {
      compute_interaction_weight(interaction, 
                                config = model$config,
                                interaction_history = history,
                                current_time = current_time)
    }
  } else if (method == "logistic") {
    # parameterised version — coefficients multiply feature values
    model$parameters <- list(
      beta_type        = rep(1.0, length(model$config)),
      beta_completion  = 1.0,
      beta_duration    = 0.5,
      beta_repetition  = 0.3,
      beta_recency     = 0.1,
      intercept        = 0.0,
      learning_rate    = 0.01
    )
    
    model$predict <- function(interaction, history = NULL, current_time = NULL) {
      # extract features
      features <- extract_weight_features(interaction, history, current_time)
      
      # linear combination
      score <- model$parameters$intercept
      score <- score + features$type_idx * model$parameters$beta_type[features$type_idx]
      score <- score + features$completion * model$parameters$beta_completion
      score <- score + features$duration * model$parameters$beta_duration
      score <- score + features$repetition * model$parameters$beta_repetition
      score <- score + features$recency * model$parameters$beta_recency
      
      # sigmoid for positive, negate for negative
      if (features$direction == "negative") {
        return(-sigmoid(score))
      } else {
        return(sigmoid(score))
      }
    }
    
    model$update <- function(interaction, true_weight, history = NULL, 
                             current_time = NULL) {
      # gradient update — this would be called during training
      predicted <- model$predict(interaction, history, current_time)
      error <- true_weight - predicted
      
      features <- extract_weight_features(interaction, history, current_time)
      lr <- model$parameters$learning_rate
      
      # simple gradient descent
      model$parameters$intercept <<- model$parameters$intercept + lr * error
      model$parameters$beta_completion <<- model$parameters$beta_completion + 
                                            lr * error * features$completion
      model$parameters$beta_duration <<- model$parameters$beta_duration + 
                                          lr * error * features$duration
      model$parameters$beta_repetition <<- model$parameters$beta_repetition + 
                                            lr * error * features$repetition
      model$parameters$beta_recency <<- model$parameters$beta_recency + 
                                         lr * error * features$recency
      
      return(invisible(NULL))
    }
  }
  
  class(model) <- "apc_weight_model"
  return(model)
}

# extract features from an interaction for the learnable weight model
extract_weight_features <- function(interaction, history = NULL, 
                                     current_time = NULL) {
  if (is.null(current_time)) current_time <- as.numeric(Sys.time())
  
  config <- get_interaction_config()
  type_names <- names(config)
  type_idx <- match(interaction$type, type_names)
  if (is.na(type_idx)) type_idx <- match("click", type_names)
  
  direction <- config[[interaction$type]]$direction
  
  # completion
  completion <- ifelse(is.na(interaction$percentage_watched), 0.5,
                       interaction$percentage_watched)
  
  # duration (log-scaled)
  duration <- log1p(interaction$duration_watched / 300)
  
  # repetition count
  rep_count <- 0
  if (!is.null(history)) {
    rep_count <- sum(sapply(history, function(h) {
      h$item_id == interaction$item_id && h$user_id == interaction$user_id
    }))
  }
  repetition <- log1p(rep_count)
  
  # recency
  delta_t <- current_time - interaction$timestamp
  recency <- exp(-delta_t / 86400)  # decay over one day
  
  return(list(
    type_idx    = type_idx,
    direction   = direction,
    completion  = completion,
    duration    = duration,
    repetition  = repetition,
    recency     = recency
  ))
}

# summarise interaction weights for a user
# useful for debugging and understanding what's driving the preference state
summarise_weights <- function(interactions, weights) {
  stopifnot(length(interactions) == length(weights))
  
  types <- sapply(interactions, function(i) i$type)
  
  summary_df <- data.frame(
    type = types,
    weight = weights,
    stringsAsFactors = FALSE
  )
  
  agg <- aggregate(weight ~ type, data = summary_df, 
                   FUN = function(x) c(mean = mean(x), 
                                       sum = sum(x), 
                                       count = length(x)))
  
  result <- data.frame(
    type  = agg$type,
    mean_weight = agg$weight[, "mean"],
    total_weight = agg$weight[, "sum"],
    count = agg$weight[, "count"]
  )
  
  return(result[order(-result$total_weight), ])
}
