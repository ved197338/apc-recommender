# Unit Tests — Preference state management

source("R/utils.R")
source("R/codes.R")
source("R/preference_state.R")
source("R/interaction_weights.R")
source("R/decay.R")

tests_run <- 0
tests_passed <- 0

assert <- function(condition, message = "") {
  tests_run <<- tests_run + 1
  if (condition) {
    tests_passed <<- tests_passed + 1
  } else {
    cat(sprintf("  FAIL: %s\n", message))
  }
}

assert_equal <- function(a, b, tol = 1e-6, message = "") {
  tests_run <<- tests_run + 1
  if (is.numeric(a) && is.numeric(b)) {
    if (abs(a - b) < tol) {
      tests_passed <<- tests_passed + 1
    } else {
      cat(sprintf("  FAIL: %s (expected %.6f, got %.6f)\n", message, b, a))
    }
  } else if (identical(a, b)) {
    tests_passed <<- tests_passed + 1
  } else {
    cat(sprintf("  FAIL: %s\n", message))
  }
}

cat("Testing Preference State\n\n")

# Test 1: create user state
cat("test: create_user_state\n")
state <- create_user_state("test_user")
assert(inherits(state, "apc_user_state"), "should have correct class")
assert_equal(state$user_id, "test_user", message = "user_id should match")
assert_equal(state$n_interactions, 0, message = "should start with 0 interactions")
assert(length(state$positive_prefs) == 0, "should start with empty preferences")

# Test 2: single update
cat("test: single preference update\n")
item_code <- create_content_code(ai = 0.8, technology = 0.6, educational = 0.5)

interaction <- create_interaction(
  user_id = "test_user",
  item_id = "item_001",
  interaction_type = "like",
  timestamp = as.numeric(Sys.time()),
  percentage_watched = 0.9
)

state <- update_preference_state(state, interaction, item_code)
assert(state$n_interactions == 1, "interaction count should be 1")
assert(length(state$positive_prefs) > 0, "preferences should be non-empty")
assert("ai" %in% names(state$positive_prefs), "should have ai preference")
assert(state$positive_prefs["ai"] > 0, "ai preference should be positive")

# Test 3: multiple updates accumulate
cat("test: accumulation across updates\n")
ai_before <- state$positive_prefs["ai"]

interaction2 <- create_interaction(
  user_id = "test_user",
  item_id = "item_002",
  interaction_type = "watch",
  timestamp = as.numeric(Sys.time()) + 100,
  duration_watched = 1500,
  item_duration = 1800,
  percentage_watched = 0.83
)
item_code2 <- create_content_code(ai = 0.9, machine_learning = 0.7)

state <- update_preference_state(state, interaction2, item_code2)
assert(state$n_interactions == 2, "should have 2 interactions")
assert(state$positive_prefs["ai"] > ai_before, "ai should increase")
assert("machine_learning" %in% names(state$positive_prefs),
       "should add new dimension")

# Test 4: negative interactions
cat("test: negative interaction handling\n")
neg_interaction <- create_interaction(
  user_id = "test_user",
  item_id = "item_003",
  interaction_type = "dislike",
  timestamp = as.numeric(Sys.time()) + 200,
  percentage_watched = 0.3
)
neg_code <- create_content_code(horror = 0.9, dark = 0.7)

state <- update_preference_state(state, neg_interaction, neg_code)
assert(length(state$negative_prefs) > 0, "should have negative preferences")
assert("horror" %in% names(state$negative_prefs), "should have horror as negative")

# Test 5: combined preferences
cat("test: get_combined_preferences\n")
combined <- get_combined_preferences(state)
assert(length(combined) > 0, "combined should be non-empty")

# negative prefs should reduce combined values
# horror is only in negative, so it should be negative in combined
if ("horror" %in% names(combined)) {
  assert(combined["horror"] < 0, "horror should be negative in combined")
}

# Test 6: preference diversity
cat("test: compute_preference_diversity\n")
diversity <- compute_preference_diversity(state)
assert(diversity >= 0, "diversity should be non-negative")
assert(is.finite(diversity), "diversity should be finite")

# Test 7: top preferences
cat("test: get_top_preferences\n")
top <- get_top_preferences(state, k = 3)
assert(length(top$names) <= 3, "should return at most k items")
assert(length(top$names) == length(top$values), "names and values should match")

# Test 8: state manager
cat("test: state manager operations\n")
mgr <- create_state_manager()
assert(mgr$n_users == 0, "should start empty")

mgr <- set_user_state(mgr, state)
assert(mgr$n_users == 1, "should have 1 user")

retrieved <- mgr$users[["test_user"]]
assert(!is.null(retrieved), "should be able to retrieve user")
assert_equal(retrieved$user_id, "test_user", message = "retrieved user should match")

# Test 9: batch update
cat("test: batch_update_preferences\n")
catalogue <- create_catalogue()
catalogue <- add_item(catalogue, create_item("it1", "Test 1",
                                              create_content_code(ai = 0.8)))
catalogue <- add_item(catalogue, create_item("it2", "Test 2",
                                              create_content_code(comedy = 0.7)))

batch_state <- create_user_state("batch_user")
batch_ints <- list(
  create_interaction("batch_user", "it1", "watch",
                     timestamp = 100, percentage_watched = 0.8),
  create_interaction("batch_user", "it2", "like",
                     timestamp = 200, percentage_watched = 0.9),
  create_interaction("batch_user", "it1", "save",
                     timestamp = 300)
)

batch_state <- batch_update_preferences(batch_state, batch_ints, catalogue)
assert(batch_state$n_interactions == 3, "should process all interactions")
assert("ai" %in% names(batch_state$positive_prefs), "should have ai")
assert("comedy" %in% names(batch_state$positive_prefs), "should have comedy")

# Test 10: history snapshots
cat("test: history snapshots\n")
assert(length(batch_state$history) > 0, "should have history snapshots")

dim_hist <- get_dimension_history(batch_state, "ai")
assert(length(dim_hist$values) > 0, "should have dimension history")
assert(length(dim_hist$timestamps) == length(dim_hist$values),
       "timestamps and values should match")

cat(sprintf("\nResults: %d/%d tests passed\n", tests_passed, tests_run))
