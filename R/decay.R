# APC Engine — Temporal preference decay mechanisms

###############################################################################

source("R/utils.R")

# default decay configuration
#
# lambda values represent decay rates. higher = faster decay.
# these are per-day rates.
#
# spent a lot of time thinking about what reasonable defaults would be.
# the key insight is that topic interests tend to be more persistent
# than format or mood preferences.
#
# e.g., if someone likes AI content, that probably persists. but if
# they're in a "relaxing" mood, that might change by tomorrow.
get_decay_config <- function() {
  config <- list(
    # default decay rate for unspecified dimensions
    default_lambda = 0.02,
    
    # per-category decay rates
    # these represent the average lambda for dimensions in each category
    category_lambdas = list(
      genre      = 0.01,   # genres are pretty stable interests
      topic      = 0.015,  # topics are fairly persistent
      style      = 0.025,  # style preferences change somewhat
      format     = 0.03,   # format preferences are more volatile
      complexity = 0.02,   # complexity preference is moderately stable
      mood       = 0.05    # mood is the most transient
    ),
    
    # per-dimension overrides (if we want specific dimensions to decay differently)
    # this is where domain knowledge can help
    dimension_overrides = list(
      # some genres stick around longer than others
      documentary  = 0.008,  # documentary watchers tend to stay documentary watchers
      comedy       = 0.015,  # comedy is pretty stable but slightly less so
      
      # educational content — once someone gets into learning, it persists
      educational  = 0.01,
      tutorial     = 0.015,
      lecture      = 0.012,
      
      # mood stuff decays faster
      exciting     = 0.06,
      relaxing     = 0.06,
      intense      = 0.06,
      funny        = 0.05,
      suspenseful  = 0.07
    ),
    
    # minimum preference value — below this we just zero it out
    # saves computation on near-zero values
    min_threshold = 0.001,
    
    # adaptive decay settings
    # when enabled, lambda gets adjusted based on how consistent
    # the user's interactions with this dimension have been
    adaptive = TRUE,
    
    # how much the adaptive system can modify the base lambda
    adaptive_range = c(0.5, 2.0)  # multiply base lambda by [0.5, 2.0]
  )
  
  return(config)
}

# get the decay rate for a specific dimension
# checks dimension overrides first, then category defaults, then global default
get_lambda <- function(dim_name, config = NULL, registry = NULL) {
  if (is.null(config)) config <- get_decay_config()
  
  # check dimension override
  if (dim_name %in% names(config$dimension_overrides)) {
    return(config$dimension_overrides[[dim_name]])
  }
  
  # check category
  if (!is.null(registry)) {
    cat <- get_dimension_category(dim_name, registry)
    if (!is.na(cat) && cat %in% names(config$category_lambdas)) {
      return(config$category_lambdas[[cat]])
    }
  }
  
  return(config$default_lambda)
}

# apply exponential decay to a preference vector
#
# P_j(t) = P_j(t-1) * exp(-lambda_j * delta_t)
#
# delta_t is in days (converted from seconds internally)
#
# this is the core decay function. it's pretty simple mathematically
# but the per-dimension lambda makes it more nuanced than a uniform decay.
apply_decay <- function(preference_vector, delta_t_seconds, 
                        config = NULL, registry = NULL,
                        adaptive_lambdas = NULL) {
  if (is.null(config)) config <- get_decay_config()
  
  # convert to days — our lambda values are daily rates
  delta_t <- delta_t_seconds / (24 * 3600)
  
  if (delta_t <= 0) return(preference_vector)
  
  decayed <- preference_vector
  
  for (dim_name in names(decayed)) {
    # get the decay rate for this dimension
    lambda <- get_lambda(dim_name, config, registry)
    
    # apply adaptive adjustment if available
    if (!is.null(adaptive_lambdas) && dim_name %in% names(adaptive_lambdas)) {
      lambda <- adaptive_lambdas[[dim_name]]
    }
    
    # exponential decay
    decay_factor <- exp(-lambda * delta_t)
    decayed[dim_name] <- decayed[dim_name] * decay_factor
    
    # threshold small values to zero
    if (abs(decayed[dim_name]) < config$min_threshold) {
      decayed[dim_name] <- 0
    }
  }
  
  # remove zero-valued dimensions to keep things sparse
  decayed <- decayed[decayed != 0]
  
  return(decayed)
}

# apply decay to both positive and negative preference vectors
# (used when we have separate positive/negative tracking)
apply_dual_decay <- function(pos_prefs, neg_prefs, delta_t_seconds,
                             config = NULL, registry = NULL,
                             adaptive_lambdas = NULL) {
  pos_decayed <- apply_decay(pos_prefs, delta_t_seconds, config, registry,
                             adaptive_lambdas)
  
  # negative preferences decay slightly faster — people forgive/forget
  neg_config <- config
  if (!is.null(neg_config)) {
    neg_config$default_lambda <- neg_config$default_lambda * 1.3
  }
  neg_decayed <- apply_decay(neg_prefs, delta_t_seconds, neg_config, registry,
                             adaptive_lambdas)
  
  return(list(
    positive = pos_decayed,
    negative = neg_decayed
  ))
}

# compute adaptive lambda values based on interaction consistency
#
# the idea: if a user consistently interacts with a dimension over time,
# it should decay more slowly (it's a real interest). if they had a burst
# of interactions with it but it's sporadic, it should decay faster.
#
# we measure consistency by looking at the coefficient of variation
# of the time gaps between interactions for each dimension.
#
# low CV = consistent = lower lambda (slower decay)
# high CV = sporadic = higher lambda (faster decay)
compute_adaptive_lambdas <- function(interaction_history, item_codes,
                                     config = NULL) {
  if (is.null(config)) config <- get_decay_config()
  
  # get all dimensions that the user has interacted with
  all_dims <- unique(unlist(lapply(item_codes, names)))
  
  adaptive_lambdas <- list()
  
  for (dim_name in all_dims) {
    base_lambda <- get_lambda(dim_name, config)
    
    # find all interactions that touched this dimension
    timestamps <- c()
    for (i in seq_along(interaction_history)) {
      int <- interaction_history[[i]]
      code <- item_codes[[int$item_id]]
      if (!is.null(code) && dim_name %in% names(code) && code[dim_name] > 0.1) {
        timestamps <- c(timestamps, int$timestamp)
      }
    }
    
    if (length(timestamps) < 3) {
      # not enough data — use default, maybe slightly higher to be conservative
      adaptive_lambdas[[dim_name]] <- base_lambda * 1.2
      next
    }
    
    # sort timestamps and compute gaps
    timestamps <- sort(timestamps)
    gaps <- diff(timestamps) / (24 * 3600)  # convert to days
    
    if (length(gaps) == 0 || all(gaps == 0)) {
      adaptive_lambdas[[dim_name]] <- base_lambda
      next
    }
    
    # coefficient of variation of gaps
    mean_gap <- mean(gaps)
    sd_gap <- sd(gaps)
    cv <- safe_div(sd_gap, mean_gap, default = 1.0)
    
    # map CV to lambda multiplier
    # CV close to 0 = very regular = multiply by range[1] (slower decay)
    # CV large = irregular = multiply by range[2] (faster decay)
    cv_clamped <- clamp(cv, 0, 3)  # cap at 3 to avoid extreme values
    
    range_low <- config$adaptive_range[1]
    range_high <- config$adaptive_range[2]
    
    # linear mapping from [0, 3] to [range_low, range_high]
    multiplier <- range_low + (cv_clamped / 3) * (range_high - range_low)
    
    adaptive_lambdas[[dim_name]] <- base_lambda * multiplier
    
    # also factor in recency — if the most recent interaction was long ago,
    # that's evidence of declining interest
    time_since_last <- (as.numeric(Sys.time()) - max(timestamps)) / (24 * 3600)
    if (time_since_last > 30) {
      # haven't interacted in over a month — speed up decay a bit
      recency_boost <- 1.0 + 0.02 * (time_since_last - 30)
      recency_boost <- min(recency_boost, 2.0)
      adaptive_lambdas[[dim_name]] <- adaptive_lambdas[[dim_name]] * recency_boost
    }
  }
  
  return(adaptive_lambdas)
}

# classify preference type based on decay behaviour
#
# this is interesting — by looking at the history of a preference dimension,
# we can classify it as:
#   - persistent: stable, slowly decaying, long history
#   - emerging:   recently appeared, growing, short history
#   - temporary:  appeared and is already fading
#   - declining:  was strong but is clearly getting weaker
#
# returns a named list with classification for each dimension
classify_preference_type <- function(preference_history, timestamps) {
  # preference_history is a list of named vectors (one per timestamp)
  # timestamps is a numeric vector of the same length
  
  if (length(preference_history) < 3) {
    # not enough data to classify
    return(list())
  }
  
  all_dims <- unique(unlist(lapply(preference_history, names)))
  
  classifications <- list()
  
  for (dim_name in all_dims) {
    # extract the time series for this dimension
    values <- sapply(preference_history, function(pv) {
      if (dim_name %in% names(pv)) pv[dim_name] else 0
    })
    
    if (all(values == 0)) next
    
    n <- length(values)
    
    # compute some features for classification
    current_val <- values[n]
    max_val <- max(values)
    mean_val <- mean(values)
    
    # when did it first appear (above threshold)?
    first_active <- min(which(values > 0.05))
    active_fraction <- sum(values > 0.05) / n
    
    # trend in the recent half
    if (n >= 4) {
      recent <- values[(n %/% 2 + 1):n]
      older  <- values[1:(n %/% 2)]
      recent_mean <- mean(recent)
      older_mean <- mean(older)
    } else {
      recent_mean <- values[n]
      older_mean <- values[1]
    }
    
    # simple slope of recent values
    recent_n <- min(10, n)
    recent_vals <- values[(n - recent_n + 1):n]
    slope <- if (length(recent_vals) > 1) {
      t_seq <- seq_along(recent_vals)
      coef(lm(recent_vals ~ t_seq))[2]
    } else {
      0
    }
    
    # classify
    if (active_fraction > 0.6 && current_val > 0.3 && abs(slope) < 0.05) {
      classifications[[dim_name]] <- "persistent"
    } else if (first_active > n * 0.6 && slope > 0.02 && current_val > 0.15) {
      classifications[[dim_name]] <- "emerging"
    } else if (current_val < max_val * 0.3 && slope < -0.02) {
      classifications[[dim_name]] <- "declining"
    } else if (active_fraction < 0.3 && max_val > 0.2) {
      classifications[[dim_name]] <- "temporary"
    } else if (slope > 0.01) {
      classifications[[dim_name]] <- "emerging"
    } else if (slope < -0.01) {
      classifications[[dim_name]] <- "declining"
    } else {
      classifications[[dim_name]] <- "persistent"
    }
  }
  
  return(classifications)
}

# visualise decay curves for different lambda values
# handy for understanding and debugging the decay behaviour
plot_decay_curves <- function(lambdas = NULL, max_days = 90, 
                              initial_value = 1.0,
                              output_file = NULL) {
  if (is.null(lambdas)) {
    lambdas <- c(
      "mood (lambda=0.05)"      = 0.05,
      "format (lambda=0.03)"    = 0.03,
      "style (lambda=0.025)"    = 0.025,
      "topic (lambda=0.015)"    = 0.015,
      "genre (lambda=0.01)"     = 0.01,
      "deep interest (lambda=0.005)" = 0.005
    )
  }
  
  days <- seq(0, max_days, by = 0.5)
  
  if (!is.null(output_file)) {
    png(output_file, width = 800, height = 500, res = 100)
  }
  
  colors <- c("#e74c3c", "#e67e22", "#f1c40f", "#2ecc71", "#3498db", "#9b59b6")
  
  plot(NULL, xlim = c(0, max_days), ylim = c(0, initial_value * 1.05),
       xlab = "Days", ylab = "Preference Strength",
       main = "Temporal Preference Decay by Category",
       bty = "l", las = 1)
  
  grid(col = "gray90")
  
  for (i in seq_along(lambdas)) {
    vals <- initial_value * exp(-lambdas[i] * days)
    lines(days, vals, col = colors[i], lwd = 2)
  }
  
  legend("topright", names(lambdas), col = colors[1:length(lambdas)],
         lwd = 2, cex = 0.8, bg = "white")
  
  if (!is.null(output_file)) {
    dev.off()
    message(sprintf("decay curves saved to %s", output_file))
  }
}

# half-life computation — useful for interpreting lambda values
# half-life = ln(2) / lambda (in the same units as lambda)
compute_half_life <- function(lambda) {
  if (lambda <= 0) return(Inf)
  return(log(2) / lambda)
}

# print a nice summary of decay configuration
print_decay_summary <- function(config = NULL) {
  if (is.null(config)) config <- get_decay_config()
  
  cat("Decay Configuration Summary\n\n")
  cat(sprintf("Default lambda: %.4f (half-life: %.1f days)\n",
              config$default_lambda, 
              compute_half_life(config$default_lambda)))
  cat(sprintf("Min threshold: %.4f\n", config$min_threshold))
  cat(sprintf("Adaptive: %s\n\n", ifelse(config$adaptive, "YES", "NO")))
  
  cat("Category decay rates:\n")
  for (cat_name in names(config$category_lambdas)) {
    lambda <- config$category_lambdas[[cat_name]]
    hl <- compute_half_life(lambda)
    cat(sprintf("  %-15s lambda=%.4f  half-life=%.1f days\n",
                cat_name, lambda, hl))
  }
  
  if (length(config$dimension_overrides) > 0) {
    cat("\nDimension overrides:\n")
    for (dim_name in names(config$dimension_overrides)) {
      lambda <- config$dimension_overrides[[dim_name]]
      hl <- compute_half_life(lambda)
      cat(sprintf("  %-15s lambda=%.4f  half-life=%.1f days\n",
                  dim_name, lambda, hl))
    }
  }
}
