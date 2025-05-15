# APC Engine — Synthetic user behavior simulator and preference drift engine

#   - viewing, skipping, liking, saving behaviour
#   - different user archetypes (casual, power user, explorer, etc.)
#
# important: the simulator should NOT be so simple that every algorithm
# trivially solves it. it needs to have realistic noise and complexity.
#

###############################################################################

source("R/utils.R")
source("R/codes.R")
source("R/interaction_weights.R")

# create a simulated user with hidden preferences
create_sim_user <- function(user_id, true_preferences, archetype = "normal",
                             preference_stability = 0.8,
                             exploration_tendency = 0.15,
                             noise_level = 0.2) {
  user <- list(
    user_id              = user_id,
    true_prefs           = true_preferences,
    archetype            = archetype,
    preference_stability = preference_stability,  # how stable are interests
    exploration_tendency = exploration_tendency,   # how likely to try new things
    noise_level          = noise_level,            # randomness in behaviour
    drift_schedule       = list(),                 # planned preference changes
    temporary_interests  = list(),                 # temporary bursts
    interaction_count    = 0
  )
  
  class(user) <- "sim_user"
  return(user)
}

# schedule a preference drift for a simulated user
# at the specified day, the user's preferences begin shifting
schedule_drift <- function(sim_user, start_day, end_day,
                            from_dims = NULL, to_dims,
                            magnitude = 0.5) {
  drift <- list(
    start_day = start_day,
    end_day   = end_day,
    from_dims = from_dims,    # dimensions that decrease (NULL = none)
    to_dims   = to_dims,      # dimensions that increase
    magnitude = magnitude,     # how strong the shift is
    active    = FALSE
  )
  
  sim_user$drift_schedule[[length(sim_user$drift_schedule) + 1]] <- drift
  return(sim_user)
}

# add a temporary interest burst
# a period where the user gets into something for a short time
add_temporary_interest <- function(sim_user, start_day, duration_days,
                                    dims, intensity = 0.6) {
  temp <- list(
    start_day = start_day,
    end_day   = start_day + duration_days,
    dims      = dims,
    intensity = intensity
  )
  
  sim_user$temporary_interests[[length(sim_user$temporary_interests) + 1]] <- temp
  return(sim_user)
}

# get a user's effective preferences at a given time
# this considers the base preferences, any active drifts, and
# temporary interests
get_effective_preferences <- function(sim_user, day) {
  prefs <- sim_user$true_prefs
  
  # apply preference drifts
  for (drift in sim_user$drift_schedule) {
    if (day >= drift$start_day && day <= drift$end_day) {
      # linear interpolation of the drift
      progress <- (day - drift$start_day) / (drift$end_day - drift$start_day)
      
      # decrease old dimensions
      if (!is.null(drift$from_dims)) {
        for (d in names(drift$from_dims)) {
          if (d %in% names(prefs)) {
            prefs[d] <- prefs[d] * (1 - progress * drift$magnitude)
          }
        }
      }
      
      # increase new dimensions
      for (d in names(drift$to_dims)) {
        current <- if (d %in% names(prefs)) prefs[d] else 0
        target <- drift$to_dims[d] * drift$magnitude
        prefs[d] <- current + progress * target
      }
    } else if (day > drift$end_day) {
      # drift has completed — apply fully
      if (!is.null(drift$from_dims)) {
        for (d in names(drift$from_dims)) {
          if (d %in% names(prefs)) {
            prefs[d] <- prefs[d] * (1 - drift$magnitude)
          }
        }
      }
      for (d in names(drift$to_dims)) {
        current <- if (d %in% names(prefs)) prefs[d] else 0
        prefs[d] <- current + drift$to_dims[d] * drift$magnitude
      }
    }
  }
  
  # apply temporary interests
  for (temp in sim_user$temporary_interests) {
    if (day >= temp$start_day && day <= temp$end_day) {
      # bell curve intensity — peaks in the middle
      mid <- (temp$start_day + temp$end_day) / 2
      half_width <- (temp$end_day - temp$start_day) / 2
      t_scaled <- (day - mid) / half_width  # [-1, 1]
      intensity <- temp$intensity * exp(-2 * t_scaled^2)
      
      for (d in names(temp$dims)) {
        if (d %in% names(prefs)) {
          prefs[d] <- prefs[d] + intensity * temp$dims[d]
        } else {
          prefs[d] <- intensity * temp$dims[d]
        }
      }
    }
  }
  
  # clamp to [0, 1]
  prefs <- pmax(0, pmin(1, prefs))
  
  return(prefs)
}

# simulate a single interaction decision
# given the user's effective preferences and a candidate item,
# decide what the user does
#
# the simulation model:
#   1. user sees the item (impression)
#   2. decide whether to click based on preference match + noise
#   3. if clicked, decide how much to watch based on preference match
#   4. decide on explicit actions (like, save, skip) based on engagement
#
# returns a list of interactions for this item
simulate_interaction <- function(sim_user, item, day, current_timestamp,
                                  config = NULL) {
  effective_prefs <- get_effective_preferences(sim_user, day)
  
  # how well does this item match the user's current preferences?
  match_score <- cosine_similarity(effective_prefs, item$code)
  
  # add noise — humans aren't perfectly consistent
  noise <- rnorm(1, 0, sim_user$noise_level)
  noisy_match <- clamp(match_score + noise, 0, 1)
  
  # exploration factor — sometimes users click on things they wouldn't usually
  explore_boost <- 0
  if (runif(1) < sim_user$exploration_tendency) {
    explore_boost <- runif(1, 0.1, 0.4)
  }
  
  click_prob <- sigmoid((noisy_match + explore_boost - 0.3) * 5)
  
  interactions <- list()
  
  # step 1: impression (always)
  # (we don't always record impressions — only sometimes)
  
  # step 2: click decision
  if (runif(1) < click_prob) {
    # user clicked!
    click_interaction <- create_interaction(
      user_id = sim_user$user_id,
      item_id = item$id,
      interaction_type = "click",
      timestamp = current_timestamp,
      duration_watched = 0,
      item_duration = item$duration
    )
    interactions[[length(interactions) + 1]] <- click_interaction
    
    # step 3: how much do they watch?
    # higher match = more likely to complete
    if (!is.na(item$duration) && item$duration > 0) {
      # base completion rate depends on match score
      base_completion <- 0.2 + 0.7 * noisy_match
      
      # add variability
      completion_noise <- rnorm(1, 0, 0.15)
      actual_completion <- clamp(base_completion + completion_noise, 0.01, 1.0)
      
      duration_watched <- actual_completion * item$duration
      
      # watch interaction
      watch_interaction <- create_interaction(
        user_id = sim_user$user_id,
        item_id = item$id,
        interaction_type = "watch",
        timestamp = current_timestamp + runif(1, 1, 5),
        duration_watched = duration_watched,
        item_duration = item$duration,
        percentage_watched = actual_completion
      )
      interactions[[length(interactions) + 1]] <- watch_interaction
      
      # step 4: explicit actions based on engagement
      
      # skip — if they watched very little
      if (actual_completion < 0.1) {
        skip_interaction <- create_interaction(
          user_id = sim_user$user_id,
          item_id = item$id,
          interaction_type = "skip",
          timestamp = current_timestamp + duration_watched + 1,
          percentage_watched = actual_completion
        )
        interactions[[length(interactions) + 1]] <- skip_interaction
      }
      
      # complete — if they watched almost all of it
      if (actual_completion > 0.9) {
        complete_interaction <- create_interaction(
          user_id = sim_user$user_id,
          item_id = item$id,
          interaction_type = "complete",
          timestamp = current_timestamp + duration_watched + 1,
          percentage_watched = actual_completion
        )
        interactions[[length(interactions) + 1]] <- complete_interaction
      }
      
      # like — correlated with match and completion
      like_prob <- 0.3 * noisy_match + 0.3 * actual_completion
      if (runif(1) < like_prob) {
        like_interaction <- create_interaction(
          user_id = sim_user$user_id,
          item_id = item$id,
          interaction_type = "like",
          timestamp = current_timestamp + duration_watched + 2,
          percentage_watched = actual_completion
        )
        interactions[[length(interactions) + 1]] <- like_interaction
      }
      
      # dislike — more likely if match is low AND they watched enough to judge
      if (actual_completion > 0.2 && noisy_match < 0.25) {
        dislike_prob <- 0.15 * (1 - noisy_match)
        if (runif(1) < dislike_prob) {
          dislike_interaction <- create_interaction(
            user_id = sim_user$user_id,
            item_id = item$id,
            interaction_type = "dislike",
            timestamp = current_timestamp + duration_watched + 2,
            percentage_watched = actual_completion
          )
          interactions[[length(interactions) + 1]] <- dislike_interaction
        }
      }
      
      # save — only for high engagement
      if (actual_completion > 0.6 && noisy_match > 0.6) {
        save_prob <- 0.2 * noisy_match
        if (runif(1) < save_prob) {
          save_interaction <- create_interaction(
            user_id = sim_user$user_id,
            item_id = item$id,
            interaction_type = "save",
            timestamp = current_timestamp + duration_watched + 3,
            percentage_watched = actual_completion
          )
          interactions[[length(interactions) + 1]] <- save_interaction
        }
      }
      
      # replay — rare, only for very high engagement
      if (actual_completion > 0.85 && noisy_match > 0.75) {
        replay_prob <- 0.08
        if (runif(1) < replay_prob) {
          replay_interaction <- create_interaction(
            user_id = sim_user$user_id,
            item_id = item$id,
            interaction_type = "replay",
            timestamp = current_timestamp + duration_watched + 
                        runif(1, 60, 3600),
            percentage_watched = runif(1, 0.5, 1.0)
          )
          interactions[[length(interactions) + 1]] <- replay_interaction
        }
      }
    }
  } else {
    # user didn't click — this is an implicit skip
    # only record this sometimes (not every non-click)
    if (match_score < 0.15 && runif(1) < 0.3) {
      skip_interaction <- create_interaction(
        user_id = sim_user$user_id,
        item_id = item$id,
        interaction_type = "skip",
        timestamp = current_timestamp,
        percentage_watched = 0
      )
      interactions[[length(interactions) + 1]] <- skip_interaction
    }
  }
  
  return(interactions)
}

# simulate a full day of activity for a user
# the user gets shown some items and interacts with them
simulate_day <- function(sim_user, catalogue, day,
                          items_per_day = 20,
                          base_timestamp = NULL) {
  if (is.null(base_timestamp)) {
    base_timestamp <- as.numeric(Sys.time()) - (120 - day) * 86400
  }
  
  # select items to show — mix of relevant and random
  effective_prefs <- get_effective_preferences(sim_user, day)
  
  # score all items by relevance
  item_scores <- sapply(catalogue$items, function(item) {
    cosine_similarity(effective_prefs, item$code) + rnorm(1, 0, 0.1)
  })
  
  # 70% relevant items, 30% random (simulating what a real feed would show)
  n_relevant <- round(items_per_day * 0.7)
  n_random <- items_per_day - n_relevant
  
  sorted_items <- names(sort(item_scores, decreasing = TRUE))
  
  relevant_items <- head(sorted_items, n_relevant * 2)
  relevant_sample <- sample(relevant_items, min(n_relevant, length(relevant_items)))
  
  remaining <- setdiff(names(catalogue$items), relevant_sample)
  random_sample <- sample(remaining, min(n_random, length(remaining)))
  
  shown_items <- c(relevant_sample, random_sample)
  shown_items <- sample(shown_items)  # shuffle
  
  # simulate interactions with each shown item
  all_interactions <- list()
  
  for (i in seq_along(shown_items)) {
    item_id <- shown_items[i]
    item <- catalogue$items[[item_id]]
    
    # timestamp within the day
    time_offset <- (i / length(shown_items)) * 14 * 3600  # spread across 14 hours
    timestamp <- base_timestamp + day * 86400 + 8 * 3600 + time_offset
    
    interactions <- simulate_interaction(sim_user, item, day, timestamp)
    all_interactions <- c(all_interactions, interactions)
  }
  
  # update interaction count
  sim_user$interaction_count <- sim_user$interaction_count + length(all_interactions)
  
  return(list(
    sim_user = sim_user,
    interactions = all_interactions
  ))
}

# run a full simulation for a user over multiple days
run_user_simulation <- function(sim_user, catalogue, n_days = 90,
                                 items_per_day = 15, 
                                 base_timestamp = NULL,
                                 verbose = FALSE) {
  if (is.null(base_timestamp)) {
    base_timestamp <- as.numeric(Sys.time()) - n_days * 86400
  }
  
  all_interactions <- list()
  preference_snapshots <- list()
  
  for (day in seq_len(n_days)) {
    result <- simulate_day(sim_user, catalogue, day,
                            items_per_day = items_per_day,
                            base_timestamp = base_timestamp)
    
    sim_user <- result$sim_user
    all_interactions <- c(all_interactions, result$interactions)
    
    # store a snapshot of true preferences for evaluation
    effective_prefs <- get_effective_preferences(sim_user, day)
    preference_snapshots[[day]] <- list(
      day = day,
      timestamp = base_timestamp + day * 86400,
      true_prefs = effective_prefs
    )
    
    if (verbose && day %% 10 == 0) {
      message(sprintf("  day %d/%d: %d interactions so far", 
                      day, n_days, length(all_interactions)))
    }
  }
  
  return(list(
    sim_user = sim_user,
    interactions = all_interactions,
    preference_snapshots = preference_snapshots,
    n_days = n_days,
    n_interactions = length(all_interactions)
  ))
}

# create a diverse set of simulated users
# each user has a different preference profile and behaviour pattern
create_sim_user_population <- function(n_users = 50, seed = 42) {
  set.seed(seed)
  
  users <- list()
  
  # user archetypes
  archetypes <- list(
    tech_enthusiast = list(
      base_prefs = c(technology = 0.8, ai = 0.7, machine_learning = 0.6,
                     cybersecurity = 0.3, educational = 0.5,
                     advanced = 0.4, scifi = 0.3),
      stability = 0.85,
      exploration = 0.1,
      noise = 0.15
    ),
    casual_viewer = list(
      base_prefs = c(comedy = 0.7, entertainment = 0.8, drama = 0.5,
                     short_form = 0.6, funny = 0.6, relaxing = 0.4),
      stability = 0.7,
      exploration = 0.2,
      noise = 0.25
    ),
    science_nerd = list(
      base_prefs = c(mathematics = 0.8, physics = 0.7, space = 0.5,
                     documentary = 0.6, educational = 0.7,
                     lecture = 0.4, thoughtful = 0.5, advanced = 0.5),
      stability = 0.9,
      exploration = 0.1,
      noise = 0.12
    ),
    gamer = list(
      base_prefs = c(gaming = 0.9, entertainment = 0.7, exciting = 0.5,
                     comedy = 0.4, technology = 0.3, short_form = 0.5),
      stability = 0.75,
      exploration = 0.15,
      noise = 0.2
    ),
    creative = list(
      base_prefs = c(art = 0.7, music = 0.6, design = 0.5,
                     inspiring = 0.5, tutorial = 0.4, documentary = 0.3,
                     experimental = 0.3),
      stability = 0.8,
      exploration = 0.25,
      noise = 0.18
    ),
    business_pro = list(
      base_prefs = c(business = 0.8, finance = 0.7, economics = 0.5,
                     analysis = 0.5, educational = 0.4, news = 0.3),
      stability = 0.85,
      exploration = 0.1,
      noise = 0.15
    ),
    explorer = list(
      base_prefs = c(documentary = 0.5, travel = 0.6, nature = 0.4,
                     educational = 0.4, inspiring = 0.3),
      stability = 0.5,
      exploration = 0.4,
      noise = 0.3
    ),
    academic = list(
      base_prefs = c(educational = 0.8, lecture = 0.6, mathematics = 0.5,
                     ai = 0.4, philosophy = 0.4, analysis = 0.5,
                     long_form = 0.5, advanced = 0.5),
      stability = 0.9,
      exploration = 0.1,
      noise = 0.1
    )
  )
  
  arch_names <- names(archetypes)
  
  for (i in seq_len(n_users)) {
    # pick an archetype with some randomness
    arch_name <- sample(arch_names, 1)
    arch <- archetypes[[arch_name]]
    
    # add some individual variation to the base preferences
    prefs <- arch$base_prefs
    variation <- rnorm(length(prefs), 0, 0.1)
    prefs <- pmax(0, pmin(1, prefs + variation))
    
    # maybe add 1-2 random extra preferences (everyone's unique)
    registry <- create_code_registry()
    all_dims <- get_all_dimensions(registry)
    extra_dims <- sample(setdiff(all_dims, names(prefs)), sample(0:3, 1))
    if (length(extra_dims) > 0) {
      extra_vals <- runif(length(extra_dims), 0.15, 0.5)
      names(extra_vals) <- extra_dims
      prefs <- c(prefs, extra_vals)
    }
    
    uid <- sprintf("user_%03d", i)
    
    sim_user <- create_sim_user(
      user_id = uid,
      true_preferences = prefs,
      archetype = arch_name,
      preference_stability = arch$stability + rnorm(1, 0, 0.05),
      exploration_tendency = arch$exploration + rnorm(1, 0, 0.03),
      noise_level = arch$noise + rnorm(1, 0, 0.02)
    )
    
    # schedule some preference drifts for random users
    if (runif(1) < 0.4) {
      # drift from current dominant interest to something new
      drift_day <- sample(30:70, 1)
      new_interests <- sample(setdiff(all_dims, names(prefs)), 2)
      new_vals <- runif(2, 0.4, 0.8)
      names(new_vals) <- new_interests
      
      # pick something to drift away from
      old_interests <- sample(names(prefs[prefs > 0.5]), 
                               min(1, sum(prefs > 0.5)))
      old_vals <- setNames(rep(0.3, length(old_interests)), old_interests)
      
      sim_user <- schedule_drift(
        sim_user,
        start_day = drift_day,
        end_day = drift_day + 20,
        from_dims = if (length(old_vals) > 0) old_vals else NULL,
        to_dims = new_vals,
        magnitude = runif(1, 0.3, 0.7)
      )
    }
    
    # add temporary interests for some users
    if (runif(1) < 0.3) {
      temp_day <- sample(20:60, 1)
      temp_dims <- sample(setdiff(all_dims, names(prefs)), 1)
      temp_vals <- runif(1, 0.4, 0.7)
      names(temp_vals) <- temp_dims
      
      sim_user <- add_temporary_interest(
        sim_user,
        start_day = temp_day,
        duration_days = sample(5:15, 1),
        dims = temp_vals,
        intensity = runif(1, 0.3, 0.7)
      )
    }
    
    users[[uid]] <- sim_user
  }
  
  return(users)
}

# run simulation for all users in a population
run_population_simulation <- function(users, catalogue, n_days = 90,
                                       items_per_day = 15,
                                       seed = 42,
                                       verbose = TRUE) {
  set.seed(seed)
  
  base_timestamp <- as.numeric(Sys.time()) - n_days * 86400
  
  all_results <- list()
  
  for (i in seq_along(users)) {
    uid <- names(users)[i]
    sim_user <- users[[uid]]
    
    if (verbose) {
      message(sprintf("simulating user %s (%d/%d) [%s]...", 
                      uid, i, length(users), sim_user$archetype))
    }
    
    result <- run_user_simulation(
      sim_user, catalogue, n_days,
      items_per_day = items_per_day,
      base_timestamp = base_timestamp,
      verbose = FALSE
    )
    
    all_results[[uid]] <- result
  }
  
  # aggregate statistics
  total_interactions <- sum(sapply(all_results, function(r) r$n_interactions))
  
  if (verbose) {
    message(sprintf("\nsimulation complete: %d users, %d days, %d total interactions",
                    length(users), n_days, total_interactions))
  }
  
  return(all_results)
}

# evaluate preference reconstruction
# compare APC-recovered preferences with the simulator's true preferences
evaluate_preference_reconstruction <- function(user_state, sim_result, day) {
  # get the true preferences at this day
  if (day > length(sim_result$preference_snapshots)) {
    day <- length(sim_result$preference_snapshots)
  }
  
  true_prefs <- sim_result$preference_snapshots[[day]]$true_prefs
  
  # get APC-estimated preferences
  estimated_prefs <- get_combined_preferences(user_state)
  
  # normalise both for fair comparison
  true_norm <- normalize_vector(true_prefs)
  est_norm <- normalize_vector(estimated_prefs)
  
  # compute similarity metrics
  cos_sim <- cosine_similarity(true_norm, est_norm)
  
  # also compute correlation
  all_dims <- union(names(true_prefs), names(estimated_prefs))
  true_full <- rep(0, length(all_dims))
  est_full <- rep(0, length(all_dims))
  names(true_full) <- all_dims
  names(est_full) <- all_dims
  true_full[names(true_prefs)] <- true_prefs
  est_full[names(estimated_prefs)] <- estimated_prefs
  
  correlation <- cor(true_full, est_full)
  
  # RMSE
  rmse <- sqrt(mean((true_full - est_full)^2))
  
  # top-K overlap — do the top preferences match?
  true_top5 <- names(sort(true_full, decreasing = TRUE))[1:5]
  est_top5 <- names(sort(est_full, decreasing = TRUE))[1:5]
  top5_overlap <- length(intersect(true_top5, est_top5)) / 5
  
  return(list(
    cosine_similarity = cos_sim,
    correlation = correlation,
    rmse = rmse,
    top5_overlap = top5_overlap,
    day = day
  ))
}
