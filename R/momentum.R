# APC Engine — Preference momentum, velocity, and trajectory analysis

###############################################################################

source("R/utils.R")

# momentum configuration
get_momentum_config <- function() {
  config <- list(
    # window sizes for computing momentum (in number of snapshots)
    short_window  = 5,     # recent momentum
    medium_window = 15,    # medium-term trend
    long_window   = 50,    # long-term direction
    
    # EMA alpha for smoothing the momentum signal
    smoothing_alpha = 0.3,
    
    # minimum number of data points to compute meaningful momentum
    min_data_points = 3,
    
    # threshold for classifying momentum direction
    rising_threshold   = 0.02,
    falling_threshold  = -0.02,
    
    # weight multipliers for how much momentum influences recommendations
    # these scale the momentum contribution in the scoring function
    emerging_boost  = 1.5,   # boost items matching emerging interests
    declining_penalty = 0.7, # reduce weight on declining interests
    
    # acceleration detection
    acceleration_window = 5,
    acceleration_threshold = 0.01
  )
  
  return(config)
}

# compute momentum for a single preference dimension
#
# returns momentum metrics including:
#   - velocity: rate of change (slope of recent trend)
#   - direction: "rising", "falling", "stable"
#   - strength: absolute rate of change (magnitude)
#   - acceleration: whether the rate of change is itself changing
#   - confidence: how reliable this momentum estimate is
compute_dimension_momentum <- function(values, timestamps = NULL,
                                        config = NULL) {
  if (is.null(config)) config <- get_momentum_config()
  
  n <- length(values)
  
  # not enough data
  if (n < config$min_data_points) {
    return(list(
      velocity     = 0,
      direction    = "unknown",
      strength     = 0,
      acceleration = 0,
      confidence   = 0,
      trend_values = values
    ))
  }
  
  # smooth the values first to reduce noise
  smoothed <- ema(values, alpha = config$smoothing_alpha)
  
  # compute velocity using different windows
  compute_slope <- function(vals, window) {
    w <- min(window, length(vals))
    if (w < 2) return(0)
    
    recent <- vals[(length(vals) - w + 1):length(vals)]
    t_vals <- seq_along(recent)
    
    # deal with edge cases
    if (length(unique(recent)) <= 1) return(0)
    if (sd(recent) < 1e-12) return(0)
    
    fit <- lm(recent ~ t_vals)
    return(coef(fit)[2])
  }
  
  short_velocity  <- compute_slope(smoothed, config$short_window)
  medium_velocity <- compute_slope(smoothed, config$medium_window)
  long_velocity   <- compute_slope(smoothed, config$long_window)
  
  # combined velocity — weighted average of different windows
  # gives more weight to recent trend but considers longer-term direction
  velocity <- 0.5 * short_velocity + 0.3 * medium_velocity + 0.2 * long_velocity
  
  # direction classification
  direction <- if (velocity > config$rising_threshold) {
    "rising"
  } else if (velocity < config$falling_threshold) {
    "falling"
  } else {
    "stable"
  }
  
  # strength is just the magnitude
  strength <- abs(velocity)
  
  # acceleration — is the velocity itself changing?
  acceleration <- 0
  if (n >= config$acceleration_window + 2) {
    # compute velocity at two different points in time
    mid <- n %/% 2
    v1 <- compute_slope(smoothed[1:mid], min(config$acceleration_window, mid - 1))
    v2 <- compute_slope(smoothed[(mid+1):n], 
                        min(config$acceleration_window, n - mid - 1))
    acceleration <- v2 - v1
  }
  
  # confidence — based on how much data we have and how consistent the trend is
  # more data + consistent trend = higher confidence
  confidence <- min(1.0, n / 30)  # max confidence at 30+ data points
  
  # adjust confidence based on R-squared of the recent trend
  if (n >= 3) {
    recent_n <- min(config$short_window, n)
    recent_vals <- smoothed[(n - recent_n + 1):n]
    if (length(unique(recent_vals)) > 1) {
      t_seq <- seq_along(recent_vals)
      fit <- lm(recent_vals ~ t_seq)
      r_squared <- summary(fit)$r.squared
      confidence <- confidence * (0.5 + 0.5 * r_squared)
    }
  }
  
  return(list(
    velocity     = velocity,
    direction    = direction,
    strength     = strength,
    acceleration = acceleration,
    confidence   = confidence,
    trend_values = smoothed,
    short_velocity  = short_velocity,
    medium_velocity = medium_velocity,
    long_velocity   = long_velocity
  ))
}

# compute momentum for ALL preference dimensions of a user
# returns a named list of momentum objects
compute_user_momentum <- function(state, config = NULL) {
  if (is.null(config)) config <- get_momentum_config()
  
  if (length(state$history) < config$min_data_points) {
    return(list())
  }
  
  # collect all dimensions that appear in the history
  all_dims <- unique(unlist(lapply(state$history, function(s) {
    names(s$positive_prefs)
  })))
  
  momentum_results <- list()
  
  for (dim_name in all_dims) {
    # extract time series for this dimension
    history <- get_dimension_history(state, dim_name, type = "positive")
    
    if (length(history$values) < config$min_data_points) next
    
    mom <- compute_dimension_momentum(
      history$values, 
      history$timestamps,
      config = config
    )
    
    # only store if there's something interesting going on
    if (mom$confidence > 0.1) {
      momentum_results[[dim_name]] <- mom
    }
  }
  
  return(momentum_results)
}

# get a summary of all momentum signals
# returns a nice data frame sorted by velocity
get_momentum_summary <- function(momentum_results) {
  if (length(momentum_results) == 0) {
    return(data.frame(
      dimension = character(0),
      velocity = numeric(0),
      direction = character(0),
      strength = numeric(0),
      confidence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  df <- data.frame(
    dimension  = names(momentum_results),
    velocity   = sapply(momentum_results, function(m) m$velocity),
    direction  = sapply(momentum_results, function(m) m$direction),
    strength   = sapply(momentum_results, function(m) m$strength),
    confidence = sapply(momentum_results, function(m) m$confidence),
    acceleration = sapply(momentum_results, function(m) m$acceleration),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # sort by absolute velocity, descending
  df <- df[order(-abs(df$velocity)), ]
  
  return(df)
}

# print momentum with nice arrows
# this was fun to write — gives a visual indicator of what's happening
print_momentum <- function(momentum_results, top_n = 15) {
  summary <- get_momentum_summary(momentum_results)
  
  if (nrow(summary) == 0) {
    cat("  (no momentum data yet)\n")
    return(invisible(NULL))
  }
  
  show_n <- min(top_n, nrow(summary))
  
  cat("Preference Momentum\n\n")
  
  for (i in seq_len(show_n)) {
    row <- summary[i, ]
    
    # pick an indicator based on direction and strength
    if (row$direction == "rising") {
      if (row$strength > 0.1) {
        indicator <- "↑↑"
      } else {
        indicator <- "↑ "
      }
      speed <- if (row$strength > 0.1) "rapidly" 
               else if (row$strength > 0.04) "moderately"
               else "slowly"
    } else if (row$direction == "falling") {
      if (row$strength > 0.1) {
        indicator <- "↓↓"
      } else {
        indicator <- "↓ "
      }
      speed <- if (row$strength > 0.1) "rapidly"
               else if (row$strength > 0.04) "moderately"
               else "slowly"
    } else {
      indicator <- "→ "
      speed <- "stable"
    }
    
    # add acceleration marker
    acc_marker <- ""
    if (abs(row$acceleration) > 0.01) {
      acc_marker <- if (row$acceleration > 0) " (accelerating)" else " (decelerating)"
    }
    
    cat(sprintf("  %-22s %s  %-10s %s  [conf: %.2f]%s\n",
                row$dimension, indicator, speed,
                formatC(row$velocity, format = "f", digits = 4),
                row$confidence, acc_marker))
  }
  
  if (nrow(summary) > show_n) {
    cat(sprintf("  ... and %d more dimensions\n", nrow(summary) - show_n))
  }
}

# compute momentum-based preference score for an item
# items that align with rising preferences get a boost
# items matching declining preferences get penalised
compute_momentum_score <- function(item_code, momentum_results, config = NULL) {
  if (is.null(config)) config <- get_momentum_config()
  
  if (length(momentum_results) == 0) return(0)
  
  score <- 0
  n_dims <- 0
  
  for (dim_name in names(item_code)) {
    if (dim_name %in% names(momentum_results)) {
      mom <- momentum_results[[dim_name]]
      dim_value <- item_code[dim_name]
      
      # the contribution is the item's dimension value weighted by
      # the momentum velocity and confidence
      contribution <- dim_value * mom$velocity * mom$confidence
      
      score <- score + contribution
      n_dims <- n_dims + 1
    }
  }
  
  # normalise by number of matched dimensions to keep scale reasonable
  if (n_dims > 0) {
    score <- score / sqrt(n_dims)
  }
  
  return(score)
}

# detect emerging interests
# an emerging interest is one that has:
#   - rising momentum
#   - relatively low current strength (so it's genuinely NEW)
#   - decent confidence
detect_emerging_interests <- function(state, momentum_results,
                                      min_velocity = 0.03,
                                      max_current_strength = 0.5) {
  prefs <- get_combined_preferences(state)
  
  emerging <- list()
  
  for (dim_name in names(momentum_results)) {
    mom <- momentum_results[[dim_name]]
    
    if (mom$direction != "rising") next
    if (mom$velocity < min_velocity) next
    if (mom$confidence < 0.2) next
    
    # check current preference strength
    current_val <- if (dim_name %in% names(prefs)) prefs[dim_name] else 0
    
    if (current_val < max_current_strength) {
      emerging[[dim_name]] <- list(
        velocity    = mom$velocity,
        current     = current_val,
        confidence  = mom$confidence,
        acceleration = mom$acceleration
      )
    }
  }
  
  return(emerging)
}

# detect declining interests — opposite of emerging
detect_declining_interests <- function(state, momentum_results,
                                       min_decline_rate = 0.03,
                                       min_current_strength = 0.2) {
  prefs <- get_combined_preferences(state)
  
  declining <- list()
  
  for (dim_name in names(momentum_results)) {
    mom <- momentum_results[[dim_name]]
    
    if (mom$direction != "falling") next
    if (abs(mom$velocity) < min_decline_rate) next
    
    # only flag it if it was actually significant before
    current_val <- if (dim_name %in% names(prefs)) prefs[dim_name] else 0
    
    if (current_val > min_current_strength) {
      declining[[dim_name]] <- list(
        velocity    = mom$velocity,
        current     = current_val,
        confidence  = mom$confidence,
        acceleration = mom$acceleration
      )
    }
  }
  
  return(declining)
}

# plot preference evolution for selected dimensions
plot_preference_evolution <- function(state, dimensions = NULL, 
                                      output_file = NULL) {
  if (length(state$history) < 2) {
    message("not enough history to plot")
    return(invisible(NULL))
  }
  
  # if no dimensions specified, pick the top 6
  if (is.null(dimensions)) {
    prefs <- get_combined_preferences(state)
    top <- top_k(prefs, 6)
    dimensions <- top$names
  }
  
  if (!is.null(output_file)) {
    png(output_file, width = 900, height = 600, res = 100)
  }
  
  # extract time series for each dimension
  all_data <- list()
  time_range <- c(Inf, -Inf)
  val_range <- c(0, 0)
  
  for (dim_name in dimensions) {
    hist <- get_dimension_history(state, dim_name)
    all_data[[dim_name]] <- hist
    
    if (length(hist$timestamps) > 0) {
      time_range[1] <- min(time_range[1], min(hist$timestamps))
      time_range[2] <- max(time_range[2], max(hist$timestamps))
      val_range[2] <- max(val_range[2], max(hist$values))
    }
  }
  
  # convert timestamps to days from start
  t_start <- time_range[1]
  
  colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c",
              "#e67e22", "#34495e")
  
  plot(NULL, xlim = c(0, (time_range[2] - t_start) / 86400),
       ylim = c(val_range[1], val_range[2] * 1.1),
       xlab = "Days", ylab = "Preference Strength",
       main = sprintf("Preference Evolution — User %s", state$user_id),
       bty = "l", las = 1)
  
  grid(col = "gray90")
  
  for (i in seq_along(dimensions)) {
    dim_name <- dimensions[i]
    hist <- all_data[[dim_name]]
    
    if (length(hist$timestamps) == 0) next
    
    days <- (hist$timestamps - t_start) / 86400
    lines(days, hist$values, col = colors[i], lwd = 2)
    points(days, hist$values, col = colors[i], pch = 20, cex = 0.5)
  }
  
  legend("topright", dimensions, col = colors[seq_along(dimensions)],
         lwd = 2, cex = 0.7, bg = "white")
  
  if (!is.null(output_file)) {
    dev.off()
    message(sprintf("preference evolution plot saved to %s", output_file))
  }
}

# plot momentum diagram
plot_momentum_diagram <- function(momentum_results, top_n = 15,
                                   output_file = NULL) {
  summary <- get_momentum_summary(momentum_results)
  if (nrow(summary) == 0) {
    message("no momentum data to plot")
    return(invisible(NULL))
  }
  
  show_n <- min(top_n, nrow(summary))
  summary <- summary[1:show_n, ]
  
  if (!is.null(output_file)) {
    png(output_file, width = 800, height = 500, res = 100)
  }
  
  # colour by direction
  colors <- ifelse(summary$direction == "rising", "#2ecc71",
                   ifelse(summary$direction == "falling", "#e74c3c", "#95a5a6"))
  
  # scale bar width by confidence
  bar_widths <- 0.5 + summary$confidence * 0.5
  
  barplot(summary$velocity, names.arg = summary$dimension,
          col = colors, border = NA,
          main = "Preference Momentum",
          ylab = "Velocity (change rate)",
          las = 2, cex.names = 0.7)
  
  abline(h = 0, col = "gray40", lty = 2)
  
  legend("topleft", c("Rising", "Stable", "Falling"),
         fill = c("#2ecc71", "#95a5a6", "#e74c3c"),
         border = NA, cex = 0.8, bg = "white")
  
  if (!is.null(output_file)) {
    dev.off()
    message(sprintf("momentum diagram saved to %s", output_file))
  }
}
