# APC Engine — Core vector utilities and similarity metrics

# normalise a vector to unit length (L2 norm)
# returns zero vector if input is all zeros — don't want NaN propagation
normalize_vector <- function(v) {
  n <- sqrt(sum(v^2))
  if (n < 1e-12) {
    return(rep(0, length(v)))
  }
  return(v / n)
}

# min-max scaling to [0,1] range
# useful for making different scoring components comparable
min_max_scale <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (abs(rng[2] - rng[1]) < 1e-12) {
    return(rep(0.5, length(x)))
  }
  return((x - rng[1]) / (rng[2] - rng[1]))
}

# z-score standardisation
z_score_normalize <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  if (is.na(s) || s < 1e-12) {
    return(rep(0, length(x)))
  }
  return((x - mu) / s)
}

# cosine similarity between two named vectors
# handles the case where they might have different names by aligning them
cosine_similarity <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  
  # if both vectors have names, align them
  if (!is.null(names(a)) && !is.null(names(b))) {
    all_names <- union(names(a), names(b))
    a_full <- rep(0, length(all_names))
    b_full <- rep(0, length(all_names))
    names(a_full) <- all_names
    names(b_full) <- all_names
    a_full[names(a)] <- a
    b_full[names(b)] <- b
    a <- a_full
    b <- b_full
  }
  
  dot_prod <- sum(a * b)
  norm_a <- sqrt(sum(a^2))
  norm_b <- sqrt(sum(b^2))
  
  if (norm_a < 1e-12 || norm_b < 1e-12) return(0)
  
  return(dot_prod / (norm_a * norm_b))
}

# weighted cosine — like regular cosine but with dimension weights
# Weighted cosine similarity accounting for category weights.
# hierarchical similarity stuff where some dimensions matter more
weighted_cosine_similarity <- function(a, b, weights = NULL) {
  if (length(a) == 0 || length(b) == 0) return(0)
  
  # align named vectors
  if (!is.null(names(a)) && !is.null(names(b))) {
    all_names <- union(names(a), names(b))
    a_full <- rep(0, length(all_names))
    b_full <- rep(0, length(all_names))
    names(a_full) <- all_names
    names(b_full) <- all_names
    a_full[names(a)] <- a
    b_full[names(b)] <- b
    a <- a_full
    b <- b_full
    
    if (!is.null(weights)) {
      w_full <- rep(1, length(all_names))
      names(w_full) <- all_names
      common <- intersect(names(weights), all_names)
      w_full[common] <- weights[common]
      weights <- w_full
    }
  }
  
  if (is.null(weights)) {
    weights <- rep(1, length(a))
  }
  
  w_a <- a * sqrt(weights)
  w_b <- b * sqrt(weights)
  
  dot_prod <- sum(w_a * w_b)
  norm_a <- sqrt(sum(w_a^2))
  norm_b <- sqrt(sum(w_b^2))
  
  if (norm_a < 1e-12 || norm_b < 1e-12) return(0)
  
  return(dot_prod / (norm_a * norm_b))
}

# softmax function — used in a few places for turning scores into
# probabilities. numerically stable version with max subtraction
softmax <- function(x, temperature = 1.0) {
  x_scaled <- x / temperature
  x_shifted <- x_scaled - max(x_scaled)
  exp_x <- exp(x_shifted)
  return(exp_x / sum(exp_x))
}

# sigmoid function — sometimes need to squash things to [0,1]
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# create a named numeric vector from a list of name-value pairs
# Construct feature vector from list.
make_feature_vector <- function(...) {
  args <- list(...)
  if (length(args) == 1 && is.list(args[[1]])) {
    args <- args[[1]]
  }
  v <- unlist(args)
  return(v)
}

# safe division — avoids NaN and Inf
safe_div <- function(a, b, default = 0) {
  result <- ifelse(abs(b) < 1e-12, default, a / b)
  return(result)
}

# exponential moving average — used in momentum calculations
# alpha closer to 1 means more weight on recent values
ema <- function(values, alpha = 0.3) {
  n <- length(values)
  if (n == 0) return(numeric(0))
  
  result <- numeric(n)
  result[1] <- values[1]
  
  for (i in 2:n) {
    result[i] <- alpha * values[i] + (1 - alpha) * result[i - 1]
  }
  return(result)
}

# clamp values to a range
clamp <- function(x, lower = 0, upper = 1) {
  pmax(lower, pmin(upper, x))
}

# sparse vector operations
# working with named vectors as "sparse" representation
# this is simpler than using Matrix package for our use case
sparse_add <- function(a, b) {
  if (length(a) == 0) return(b)
  if (length(b) == 0) return(a)
  
  all_names <- union(names(a), names(b))
  result <- rep(0, length(all_names))
  names(result) <- all_names
  
  result[names(a)] <- result[names(a)] + a
  result[names(b)] <- result[names(b)] + b
  
  return(result)
}

sparse_multiply <- function(v, scalar) {
  return(v * scalar)
}

# compute the entropy of a probability vector
# used for measuring diversity and uncertainty
entropy <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)  # make sure it sums to 1
  return(-sum(p * log2(p)))
}

# top-k indices from a named numeric vector
# returns names and values of the k largest elements
top_k <- function(v, k = 10) {
  k <- min(k, length(v))
  if (k == 0) return(list(names = character(0), values = numeric(0)))
  
  idx <- order(v, decreasing = TRUE)[1:k]
  return(list(
    names  = names(v)[idx],
    values = unname(v[idx]),
    indices = idx
  ))
}

# set random seed with a descriptive wrapper
# makes experiments reproducible
set_experiment_seed <- function(seed, description = "") {
  set.seed(seed)
  if (nchar(description) > 0) {
    message(sprintf("[seed=%d] %s", seed, description))
  }
}

# timestamp helper — used for logging experiment progress
log_msg <- function(...) {
  msg <- paste0(...)
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] %s", timestamp, msg))
}

# mutual information between two discrete variables
# used in compound code detection
mutual_information <- function(x, y) {
  # contingency table
  tab <- table(x, y)
  n <- sum(tab)
  
  # joint probability
  p_xy <- tab / n
  
  # marginals
  p_x <- rowSums(p_xy)
  p_y <- colSums(p_xy)
  
  mi <- 0
  for (i in seq_len(nrow(p_xy))) {
    for (j in seq_len(ncol(p_xy))) {
      if (p_xy[i, j] > 0) {
        mi <- mi + p_xy[i, j] * log2(p_xy[i, j] / (p_x[i] * p_y[j]))
      }
    }
  }
  return(mi)
}

# rolling window statistics
# need this for the momentum calculations over time windows
rolling_mean <- function(x, window = 5) {
  n <- length(x)
  if (n < window) {
    return(rep(mean(x), n))
  }
  
  result <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - window + 1)
    result[i] <- mean(x[start:i])
  }
  return(result)
}

rolling_slope <- function(x, window = 5) {
  n <- length(x)
  result <- numeric(n)
  
  for (i in seq_len(n)) {
    start <- max(1, i - window + 1)
    if (i - start < 1) {
      result[i] <- 0
      next
    }
    segment <- x[start:i]
    t_vals <- seq_along(segment)
    
    # simple linear regression slope
    if (length(unique(segment)) == 1) {
      result[i] <- 0
    } else {
      fit <- lm(segment ~ t_vals)
      result[i] <- coef(fit)[2]
    }
  }
  return(result)
}

# compute jaccard similarity between two sets (as character vectors)
jaccard_similarity <- function(a, b) {
  if (length(a) == 0 && length(b) == 0) return(1)
  inter <- length(intersect(a, b))
  uni   <- length(union(a, b))
  return(inter / uni)
}

# create an empty preference vector with given dimension names
# just zeros but with the right names attached
empty_preference_vector <- function(dim_names) {
  v <- rep(0, length(dim_names))
  names(v) <- dim_names
  return(v)
}

# check if a value is effectively zero (within floating point tolerance)
is_effectively_zero <- function(x, tol = 1e-10) {
  return(abs(x) < tol)
}

# pretty print a named vector (for debugging mostly)
print_vector <- function(v, top_n = 10, digits = 3) {
  if (length(v) == 0) {
    cat("  (empty vector)\n")
    return(invisible(NULL))
  }
  
  sorted <- sort(v, decreasing = TRUE)
  show_n <- min(top_n, length(sorted))
  
  for (i in seq_len(show_n)) {
    cat(sprintf("  %-25s %s\n", 
                names(sorted)[i], 
                formatC(sorted[i], digits = digits, format = "f")))
  }
  
  if (length(sorted) > show_n) {
    cat(sprintf("  ... and %d more dimensions\n", length(sorted) - show_n))
  }
  return(invisible(NULL))
}
