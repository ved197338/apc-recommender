# APC Engine — Explicit negative preference and suppression modelling

#   - integrating negative prefs into recommendations
#

###############################################################################

source("R/utils.R")

# negative preference configuration
get_negative_config <- function() {
  config <- list(
    # minimum number of negative signals before we treat a dimension
    # as genuinely disliked (prevents one-off suppression)
    min_negative_count = 3,
    
    # how much a single negative interaction contributes
    # (lower than positive to avoid over-suppression)
    negative_weight_scale = 0.6,
    
    # decay rate for negative preferences
    # negative prefs decay faster than positive (forgive and forget)
    negative_decay_multiplier = 1.5,
    
    # threshold below which negative preference is ignored
    suppression_threshold = 0.05,
    
    # maximum suppression factor (never completely suppress a category)
    max_suppression = 0.8,
    
    # how many repeated skips of similar content counts as "significant"
    repeated_skip_threshold = 3,
    
    # confidence accumulation — more negative signals = more confidence
    # that this is a real dislike, not just context
    confidence_floor = 0.2,
    confidence_ceiling = 0.95,
    confidence_growth_rate = 0.15
  )
  
  return(config)
}

# analyse negative interaction patterns for a user
# looks at the interaction history and identifies dimensions that
# have consistent negative signals
analyse_negative_patterns <- function(interaction_log, catalogue,
                                       config = NULL) {
  if (is.null(config)) config <- get_negative_config()
  
  if (length(interaction_log) == 0) return(list())
  
  # count negative interactions per dimension
  dim_negative_counts <- list()
  dim_total_exposure <- list()
  
  negative_types <- c("skip", "dislike", "hide", "not_interested")
  
  for (interaction in interaction_log) {
    item <- catalogue$items[[interaction$item_id]]
    if (is.null(item)) next
    
    # track exposure for each dimension
    for (dim_name in names(item$code)) {
      if (item$code[dim_name] < 0.1) next
      
      if (is.null(dim_total_exposure[[dim_name]])) {
        dim_total_exposure[[dim_name]] <- 0
        dim_negative_counts[[dim_name]] <- 0
      }
      
      dim_total_exposure[[dim_name]] <- dim_total_exposure[[dim_name]] + 1
      
      if (interaction$type %in% negative_types) {
        dim_negative_counts[[dim_name]] <- dim_negative_counts[[dim_name]] + 1
      }
      
      # also count low completion as implicit negative
      if (interaction$type == "watch" && !is.na(interaction$percentage_watched)) {
        if (interaction$percentage_watched < 0.1) {
          dim_negative_counts[[dim_name]] <- dim_negative_counts[[dim_name]] + 0.5
        }
      }
    }
  }
  
  # identify dimensions with significant negative patterns
  negative_patterns <- list()
  
  for (dim_name in names(dim_negative_counts)) {
    neg_count <- dim_negative_counts[[dim_name]]
    total_exposure <- dim_total_exposure[[dim_name]]
    
    if (neg_count < config$min_negative_count) next
    if (total_exposure < 5) next
    
    # negative ratio
    neg_ratio <- neg_count / total_exposure
    
    # confidence builds with more data
    confidence <- config$confidence_floor + 
                  config$confidence_growth_rate * log1p(neg_count)
    confidence <- min(confidence, config$confidence_ceiling)
    
    negative_patterns[[dim_name]] <- list(
      count = neg_count,
      exposure = total_exposure,
      ratio = neg_ratio,
      confidence = confidence,
      suppression = min(config$max_suppression, neg_ratio * confidence)
    )
  }
  
  return(negative_patterns)
}

# detect "content abandonment" — the user keeps trying content in a
# category but gives up every time. this is stronger negative evidence
# than a single skip.
detect_content_abandonment <- function(interaction_log, catalogue,
                                        window_size = 20) {
  if (length(interaction_log) < window_size) return(list())
  
  # look at the recent window
  recent <- tail(interaction_log, window_size)
  
  # group by dimension and track completion rates
  dim_completions <- list()
  
  for (interaction in recent) {
    if (interaction$type != "watch") next
    
    item <- catalogue$items[[interaction$item_id]]
    if (is.null(item)) next
    
    pct <- interaction$percentage_watched
    if (is.na(pct)) next
    
    for (dim_name in names(item$code)) {
      if (item$code[dim_name] < 0.2) next
      
      if (is.null(dim_completions[[dim_name]])) {
        dim_completions[[dim_name]] <- numeric(0)
      }
      dim_completions[[dim_name]] <- c(dim_completions[[dim_name]], pct)
    }
  }
  
  # find dimensions with consistently low completion
  abandoned <- list()
  for (dim_name in names(dim_completions)) {
    completions <- dim_completions[[dim_name]]
    
    if (length(completions) < 3) next
    
    mean_completion <- mean(completions)
    max_completion <- max(completions)
    
    # if mean completion is below 20% and they never finished one...
    if (mean_completion < 0.2 && max_completion < 0.5) {
      abandoned[[dim_name]] <- list(
        mean_completion = mean_completion,
        n_attempts = length(completions),
        max_completion = max_completion
      )
    }
  }
  
  return(abandoned)
}

# apply negative preference suppression to recommendation scores
# reduces the score of items matching negative preferences
apply_negative_suppression <- function(item_code, negative_patterns,
                                        config = NULL) {
  if (is.null(config)) config <- get_negative_config()
  
  if (length(negative_patterns) == 0) return(1.0)  # no suppression
  
  suppression_factor <- 1.0
  
  for (dim_name in names(negative_patterns)) {
    pattern <- negative_patterns[[dim_name]]
    
    if (dim_name %in% names(item_code)) {
      dim_value <- item_code[dim_name]
      
      # the suppression is proportional to both the item's dimension value
      # and the pattern's confidence/strength
      dim_suppression <- dim_value * pattern$suppression
      
      suppression_factor <- suppression_factor * (1 - dim_suppression)
    }
  }
  
  # never go below (1 - max_suppression)
  suppression_factor <- max(1 - config$max_suppression, suppression_factor)
  
  return(suppression_factor)
}

# distinguish between "lack of interest" and "active dislike"
# this is important — not clicking on cooking content might just mean
# you haven't been exposed to it, not that you hate cooking.
classify_negative_type <- function(negative_patterns, exposure_data) {
  classifications <- list()
  
  for (dim_name in names(negative_patterns)) {
    pattern <- negative_patterns[[dim_name]]
    
    if (pattern$ratio > 0.6 && pattern$count >= 5) {
      classifications[[dim_name]] <- "active_dislike"
    } else if (pattern$ratio > 0.3) {
      classifications[[dim_name]] <- "mild_dislike"
    } else {
      classifications[[dim_name]] <- "indifferent"
    }
  }
  
  return(classifications)
}

# print negative preference summary
print_negative_summary <- function(negative_patterns) {
  if (length(negative_patterns) == 0) {
    cat("  (no significant negative patterns detected)\n")
    return(invisible(NULL))
  }
  
  cat("Negative Preference Patterns\n\n")
  
  # sort by suppression strength
  suppressions <- sapply(negative_patterns, function(p) p$suppression)
  sorted_idx <- order(suppressions, decreasing = TRUE)
  
  for (idx in sorted_idx) {
    dim_name <- names(negative_patterns)[idx]
    pattern <- negative_patterns[[dim_name]]
    
    bar_len <- round(pattern$suppression * 20)
    bar <- paste(rep("█", bar_len), collapse = "")
    
    cat(sprintf("  %-22s  neg_count=%d  ratio=%.2f  conf=%.2f  suppress=%.2f  %s\n",
                dim_name, pattern$count, pattern$ratio,
                pattern$confidence, pattern$suppression, bar))
  }
}
