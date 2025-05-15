# Unit Tests — Content code and catalogue operations

# source from project root
source("R/utils.R")
source("R/codes.R")

# simple test framework — nothing fancy, just pass/fail
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

cat("Testing Content Code System\n\n")

# Test 1: registry creation
cat("test: create_code_registry\n")
reg <- create_code_registry()
assert(is.list(reg), "registry should be a list")
assert(length(reg) > 0, "registry should not be empty")
assert("genres" %in% names(reg), "registry should have genres")
assert("topics" %in% names(reg), "registry should have topics")
assert("styles" %in% names(reg), "registry should have styles")
assert("formats" %in% names(reg), "registry should have formats")
assert("complexity" %in% names(reg), "registry should have complexity")
assert("moods" %in% names(reg), "registry should have moods")

# Test 2: get_all_dimensions
cat("test: get_all_dimensions\n")
dims <- get_all_dimensions(reg)
assert(length(dims) > 30, "should have at least 30 dimensions")
assert("scifi" %in% dims, "should include scifi")
assert("ai" %in% dims, "should include ai")
assert("mathematics" %in% dims, "should include mathematics")

# Test 3: get_dimension_category
cat("test: get_dimension_category\n")
assert_equal(get_dimension_category("scifi", reg), "genre",
             message = "scifi should be genre category")
assert_equal(get_dimension_category("ai", reg), "topic",
             message = "ai should be topic category")
assert_equal(get_dimension_category("exciting", reg), "mood",
             message = "exciting should be mood category")

# Test 4: create_content_code
cat("test: create_content_code\n")
code <- create_content_code(scifi = 0.9, action = 0.3, space = 0.8)
assert(length(code) == 3, "code should have 3 dimensions")
assert_equal(code["scifi"], 0.9, message = "scifi should be 0.9")
assert_equal(code["action"], 0.3, message = "action should be 0.3")
assert_equal(code["space"], 0.8, message = "space should be 0.8")

# test clamping
code_clamped <- create_content_code(scifi = 1.5, action = -0.2)
assert(code_clamped["scifi"] <= 1.0, "values should be clamped to max 1")
assert(code_clamped["action"] >= 0.0, "values should be clamped to min 0")

# Test 5: create_item
cat("test: create_item\n")
item <- create_item(
  item_id = "test_001",
  title = "Test Item",
  code_vector = create_content_code(ai = 0.8, educational = 0.6),
  duration = 1800
)
assert(inherits(item, "apc_item"), "item should have class apc_item")
assert_equal(item$id, "test_001", message = "item id should match")
assert_equal(item$duration, 1800, message = "duration should be 1800")
assert(length(item$code) == 2, "code should have 2 dimensions")

# Test 6: catalogue operations
cat("test: catalogue operations\n")
cat_test <- create_catalogue()
assert(inherits(cat_test, "apc_catalogue"), "should have correct class")
assert_equal(cat_test$n_items, 0, message = "should start empty")

# add items
item1 <- create_item("i1", "Item 1", create_content_code(scifi = 0.9, ai = 0.7))
item2 <- create_item("i2", "Item 2", create_content_code(comedy = 0.8, funny = 0.7))
item3 <- create_item("i3", "Item 3", create_content_code(scifi = 0.5, comedy = 0.5))

cat_test <- add_item(cat_test, item1)
cat_test <- add_item(cat_test, item2)
cat_test <- add_item(cat_test, item3)

assert_equal(cat_test$n_items, 3, message = "should have 3 items")
assert(length(cat_test$all_dimensions) >= 4, "should track all dimensions")

# Test 7: code matrix
cat("test: get_code_matrix\n")
mat <- get_code_matrix(cat_test)
assert(nrow(mat) == 3, "matrix should have 3 rows")
assert(ncol(mat) >= 4, "matrix should have at least 4 columns")
assert_equal(mat["i1", "scifi"], 0.9, message = "i1 scifi should be 0.9")
assert_equal(mat["i2", "comedy"], 0.8, message = "i2 comedy should be 0.8")

# Test 8: find_similar_items
cat("test: find_similar_items\n")
similar <- find_similar_items(cat_test, "i1", k = 2)
assert(length(similar$item_ids) == 2, "should return 2 similar items")
# i3 has scifi=0.5 so should be more similar to i1 than i2
assert(similar$item_ids[1] == "i3", "i3 should be most similar to i1")

# Test 9: synthetic catalogue generation
cat("test: generate_synthetic_catalogue\n")
syn_cat <- generate_synthetic_catalogue(n_items = 100, seed = 42)
assert_equal(syn_cat$n_items, 100, message = "should have 100 items")
assert(length(syn_cat$all_dimensions) > 10, "should use many dimensions")

# check that items have reasonable codes
for (i in seq_len(min(10, syn_cat$n_items))) {
  item <- syn_cat$items[[i]]
  assert(length(item$code) > 0, paste("item", i, "should have non-empty code"))
  assert(all(item$code >= 0 & item$code <= 1), 
         paste("item", i, "code values should be in [0,1]"))
}

# Done
cat(sprintf("\nResults: %d/%d tests passed\n", tests_passed, tests_run))
