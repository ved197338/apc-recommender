# APC Engine — Compound code detection and association analysis

#   - multiple approaches to compound detection (compared experimentally)
#

###############################################################################

source("R/utils.R")

# compound code configuration
get_compound_config <- function() {
  config <- list(
    # maximum order of compound codes (2 = pairs, 3 = triples)
    max_order = 2,
    
    # minimum number of co-occurrences to consider a compound code
    min_cooccurrence = 5,
    
    # significance threshold for keeping a compound code
    significance_threshold = 0.05,
    
    # methods for detecting significant compounds
    # "frequency" — simple co-occurrence frequency
    # "mi" — mutual information
    # "lift" — association rule lift measure
    # "chi2" — chi-squared test of independence
    detection_method = "mi",
    
    # pruning settings
    max_compounds = 200,  # absolute cap on number of compound codes
    prune_threshold = 0.01,  # remove compounds below this strength
    
    # minimum individual dimension strength to consider for pairing
    min_individual_strength = 0.1,
    
    # how much compound codes contribute relative to individual codes
    compound_weight = 0.5  # in the scoring function
  )
  
  return(config)
}

# detect compound codes from a user's interaction history
#
# looks at which dimensions co-occur in items the user has interacted with
# and determines which co-occurrences are statistically meaningful.
#
# returns a list of compound codes with their strengths
detect_compound_codes <- function(interaction_history, catalogue,
                                   config = NULL) {
  if (is.null(config)) config <- get_compound_config()
  
  if (length(interaction_history) < config$min_cooccurrence) {
    return(list())
  }
  
  # collect content codes from all interacted items
  item_codes <- list()
  for (interaction in interaction_history) {
    item <- catalogue$items[[interaction$item_id]]
    if (!is.null(item)) {
      item_codes[[length(item_codes) + 1]] <- item$code
    }
  }
  
  if (length(item_codes) < config$min_cooccurrence) {
    return(list())
  }
  
  # find frequently co-occurring dimensions
  # first, get all dimensions that appear often enough
  dim_counts <- table(unlist(lapply(item_codes, function(c) {
    names(c[c > config$min_individual_strength])
  })))
  
  active_dims <- names(dim_counts[dim_counts >= 3])
  
  if (length(active_dims) < 2) {
    return(list())
  }
  
  # compute compound codes based on configured method
  compounds <- switch(config$detection_method,
    "frequency" = detect_compounds_frequency(item_codes, active_dims, config),
    "mi"        = detect_compounds_mi(item_codes, active_dims, config),
    "lift"      = detect_compounds_lift(item_codes, active_dims, config),
    "chi2"      = detect_compounds_chi2(item_codes, active_dims, config),
    detect_compounds_mi(item_codes, active_dims, config)  # default
  )
  
  # prune to max number of compounds
  if (length(compounds) > config$max_compounds) {
    strengths <- sapply(compounds, function(c) c$strength)
    keep_idx <- order(strengths, decreasing = TRUE)[1:config$max_compounds]
    compounds <- compounds[keep_idx]
  }
  
  return(compounds)
}

# frequency-based compound detection
# simplest approach: just count how often dimensions co-occur
detect_compounds_frequency <- function(item_codes, active_dims, config) {
  n_items <- length(item_codes)
  compounds <- list()
  
  # pairwise co-occurrence
  for (i in seq_along(active_dims)) {
    for (j in (i+1):min(length(active_dims), i + 50)) {
      if (j > length(active_dims)) break
      
      dim_a <- active_dims[i]
      dim_b <- active_dims[j]
      
      # count items where both dimensions are active
      cooccur <- sum(sapply(item_codes, function(code) {
        a_val <- if (dim_a %in% names(code)) code[dim_a] else 0
        b_val <- if (dim_b %in% names(code)) code[dim_b] else 0
        a_val > config$min_individual_strength && 
          b_val > config$min_individual_strength
      }))
      
      if (cooccur >= config$min_cooccurrence) {
        # compute average joint strength
        joint_strengths <- sapply(item_codes, function(code) {
          a_val <- if (dim_a %in% names(code)) code[dim_a] else 0
          b_val <- if (dim_b %in% names(code)) code[dim_b] else 0
          if (a_val > 0 && b_val > 0) sqrt(a_val * b_val) else 0
        })
        
        avg_strength <- mean(joint_strengths[joint_strengths > 0])
        
        compound_name <- paste(dim_a, dim_b, sep = " × ")
        compounds[[compound_name]] <- list(
          dims     = c(dim_a, dim_b),
          count    = cooccur,
          strength = avg_strength * (cooccur / n_items),
          method   = "frequency"
        )
      }
    }
  }
  
  return(compounds)
}

# mutual information-based compound detection
# MI measures how much knowing one dimension tells you about another
# high MI = the dimensions are strongly associated
detect_compounds_mi <- function(item_codes, active_dims, config) {
  n_items <- length(item_codes)
  compounds <- list()
  
  # binarise codes: is each dimension "active" (above threshold) or not?
  binary_matrix <- matrix(0, nrow = n_items, ncol = length(active_dims))
  colnames(binary_matrix) <- active_dims
  
  for (i in seq_len(n_items)) {
    code <- item_codes[[i]]
    for (d in active_dims) {
      if (d %in% names(code) && code[d] > config$min_individual_strength) {
        binary_matrix[i, d] <- 1
      }
    }
  }
  
  # compute pairwise MI
  for (i in seq_along(active_dims)) {
    for (j in (i+1):min(length(active_dims), i + 50)) {
      if (j > length(active_dims)) break
      
      dim_a <- active_dims[i]
      dim_b <- active_dims[j]
      
      # compute MI between the two binary columns
      mi <- mutual_information(binary_matrix[, dim_a], binary_matrix[, dim_b])
      
      # also compute the co-occurrence count
      cooccur <- sum(binary_matrix[, dim_a] == 1 & binary_matrix[, dim_b] == 1)
      
      if (mi > 0.05 && cooccur >= config$min_cooccurrence) {
        # compute average joint strength weighted by MI
        joint_strengths <- sapply(item_codes, function(code) {
          a_val <- if (dim_a %in% names(code)) code[dim_a] else 0
          b_val <- if (dim_b %in% names(code)) code[dim_b] else 0
          if (a_val > 0 && b_val > 0) sqrt(a_val * b_val) else 0
        })
        
        avg_strength <- mean(joint_strengths[joint_strengths > 0])
        
        compound_name <- paste(dim_a, dim_b, sep = " × ")
        compounds[[compound_name]] <- list(
          dims         = c(dim_a, dim_b),
          count        = cooccur,
          mi           = mi,
          strength     = avg_strength * mi,
          method       = "mi"
        )
      }
    }
  }
  
  return(compounds)
}

# lift-based compound detection
# lift = P(A,B) / (P(A) * P(B))
# lift > 1 means the dimensions co-occur more than expected by chance
detect_compounds_lift <- function(item_codes, active_dims, config) {
  n_items <- length(item_codes)
  compounds <- list()
  
  # compute individual dimension frequencies
  dim_freq <- sapply(active_dims, function(d) {
    sum(sapply(item_codes, function(code) {
      d %in% names(code) && code[d] > config$min_individual_strength
    })) / n_items
  })
  names(dim_freq) <- active_dims
  
  for (i in seq_along(active_dims)) {
    for (j in (i+1):min(length(active_dims), i + 50)) {
      if (j > length(active_dims)) break
      
      dim_a <- active_dims[i]
      dim_b <- active_dims[j]
      
      # joint frequency
      joint_freq <- sum(sapply(item_codes, function(code) {
        a_val <- if (dim_a %in% names(code)) code[dim_a] else 0
        b_val <- if (dim_b %in% names(code)) code[dim_b] else 0
        a_val > config$min_individual_strength && 
          b_val > config$min_individual_strength
      })) / n_items
      
      # lift
      expected <- dim_freq[dim_a] * dim_freq[dim_b]
      if (expected < 1e-10) next
      
      lift <- joint_freq / expected
      cooccur <- round(joint_freq * n_items)
      
      if (lift > 1.5 && cooccur >= config$min_cooccurrence) {
        # compute strength incorporating lift
        joint_strengths <- sapply(item_codes, function(code) {
          a_val <- if (dim_a %in% names(code)) code[dim_a] else 0
          b_val <- if (dim_b %in% names(code)) code[dim_b] else 0
          if (a_val > 0 && b_val > 0) sqrt(a_val * b_val) else 0
        })
        
        avg_strength <- mean(joint_strengths[joint_strengths > 0])
        
        compound_name <- paste(dim_a, dim_b, sep = " × ")
        compounds[[compound_name]] <- list(
          dims     = c(dim_a, dim_b),
          count    = cooccur,
          lift     = lift,
          strength = avg_strength * log(lift),  # log-lift scaling
          method   = "lift"
        )
      }
    }
  }
  
  return(compounds)
}

# chi-squared test for compound detection
# tests whether the co-occurrence pattern is significantly different
# from what we'd expect if the dimensions were independent
detect_compounds_chi2 <- function(item_codes, active_dims, config) {
  n_items <- length(item_codes)
  compounds <- list()
  
  # binarise
  binary_matrix <- matrix(0, nrow = n_items, ncol = length(active_dims))
  colnames(binary_matrix) <- active_dims
  
  for (i in seq_len(n_items)) {
    code <- item_codes[[i]]
    for (d in active_dims) {
      if (d %in% names(code) && code[d] > config$min_individual_strength) {
        binary_matrix[i, d] <- 1
      }
    }
  }
  
  for (i in seq_along(active_dims)) {
    for (j in (i+1):min(length(active_dims), i + 50)) {
      if (j > length(active_dims)) break
      
      dim_a <- active_dims[i]
      dim_b <- active_dims[j]
      
      # contingency table
      tab <- table(
        factor(binary_matrix[, dim_a], levels = c(0, 1)),
        factor(binary_matrix[, dim_b], levels = c(0, 1))
      )
      
      # chi-squared test (with continuity correction)
      # suppress warnings about small expected values
      test_result <- tryCatch(
        suppressWarnings(chisq.test(tab)),
        error = function(e) list(p.value = 1, statistic = 0)
      )
      
      cooccur <- tab[2, 2]
      
      if (test_result$p.value < config$significance_threshold && 
          cooccur >= config$min_cooccurrence) {
        # compute strength based on chi-squared statistic
        chi2_normalized <- unname(test_result$statistic) / n_items
        
        joint_strengths <- sapply(item_codes, function(code) {
          a_val <- if (dim_a %in% names(code)) code[dim_a] else 0
          b_val <- if (dim_b %in% names(code)) code[dim_b] else 0
          if (a_val > 0 && b_val > 0) sqrt(a_val * b_val) else 0
        })
        
        avg_strength <- mean(joint_strengths[joint_strengths > 0])
        
        compound_name <- paste(dim_a, dim_b, sep = " × ")
        compounds[[compound_name]] <- list(
          dims      = c(dim_a, dim_b),
          count     = cooccur,
          chi2      = unname(test_result$statistic),
          p_value   = test_result$p.value,
          strength  = avg_strength * sqrt(chi2_normalized),
          method    = "chi2"
        )
      }
    }
  }
  
  return(compounds)
}

# compute compound preference scores for a user
# based on their preference vector and the detected compound codes
compute_compound_preference <- function(preference_vector, compounds) {
  if (length(compounds) == 0) return(numeric(0))
  
  scores <- numeric(length(compounds))
  names(scores) <- names(compounds)
  
  for (i in seq_along(compounds)) {
    comp <- compounds[[i]]
    dims <- comp$dims
    
    # get the user's preference strength in each dimension
    dim_vals <- sapply(dims, function(d) {
      if (d %in% names(preference_vector)) preference_vector[d] else 0
    })
    
    # compound score is the geometric mean of dimension values
    # multiplied by the compound's own strength
    # geometric mean because we want BOTH dimensions to be present
    if (all(dim_vals > 0)) {
      geo_mean <- exp(mean(log(dim_vals)))
      scores[i] <- geo_mean * comp$strength
    } else {
      scores[i] <- 0
    }
  }
  
  return(scores[scores > 0])
}

# compute compound similarity between user preferences and an item
compute_compound_similarity <- function(user_prefs, item_code, compounds) {
  if (length(compounds) == 0) return(0)
  
  user_compound <- compute_compound_preference(user_prefs, compounds)
  item_compound <- compute_compound_preference(item_code, compounds)
  
  if (length(user_compound) == 0 || length(item_compound) == 0) return(0)
  
  return(cosine_similarity(user_compound, item_compound))
}

# compare compound detection methods
# runs all four methods and returns their results for comparison
compare_compound_methods <- function(interaction_history, catalogue) {
  methods <- c("frequency", "mi", "lift", "chi2")
  results <- list()
  
  for (method in methods) {
    config <- get_compound_config()
    config$detection_method <- method
    
    compounds <- detect_compound_codes(interaction_history, catalogue, config)
    
    results[[method]] <- list(
      n_compounds = length(compounds),
      compounds = compounds,
      top_compounds = if (length(compounds) > 0) {
        strengths <- sapply(compounds, function(c) c$strength)
        head(sort(strengths, decreasing = TRUE), 10)
      } else {
        numeric(0)
      }
    )
  }
  
  return(results)
}

# print compound code summary
print_compounds <- function(compounds, top_n = 20) {
  if (length(compounds) == 0) {
    cat("  (no compound codes detected)\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("Compound Codes (%d total)\n\n", length(compounds)))
  
  # sort by strength
  strengths <- sapply(compounds, function(c) c$strength)
  sorted_idx <- order(strengths, decreasing = TRUE)
  
  show_n <- min(top_n, length(sorted_idx))
  
  for (i in seq_len(show_n)) {
    idx <- sorted_idx[i]
    comp <- compounds[[idx]]
    name <- names(compounds)[idx]
    
    cat(sprintf("  %-35s  strength=%.4f  count=%d  [%s]\n",
                name, comp$strength, comp$count, comp$method))
  }
  
  if (length(compounds) > show_n) {
    cat(sprintf("  ... and %d more\n", length(compounds) - show_n))
  }
}

# prune compounds that are no longer significant
# should be called periodically as user preferences evolve
prune_compounds <- function(compounds, preference_vector, config = NULL) {
  if (is.null(config)) config <- get_compound_config()
  
  if (length(compounds) == 0) return(compounds)
  
  # remove compounds where the user no longer has strength in both dimensions
  keep <- sapply(compounds, function(comp) {
    dims <- comp$dims
    dim_vals <- sapply(dims, function(d) {
      if (d %in% names(preference_vector)) preference_vector[d] else 0
    })
    
    # keep if both dimensions are still active AND strength is above threshold
    all(dim_vals > config$min_individual_strength * 0.5) && 
      comp$strength > config$prune_threshold
  })
  
  pruned <- compounds[keep]
  
  n_removed <- length(compounds) - length(pruned)
  if (n_removed > 0) {
    message(sprintf("pruned %d compound codes (keeping %d)", 
                    n_removed, length(pruned)))
  }
  
  return(pruned)
}
