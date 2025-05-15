# APC Engine — Content taxonomy, item representation, and catalogue management

source("R/utils.R")

# define the content taxonomy
# this is the master list of all possible content dimensions
# organised by category for readability but internally they're all
# just dimensions in a flat vector
#
# the numeric IDs are for reference/display only — internally we use names

# content code registry — maps human-readable names to category metadata
create_code_registry <- function() {
  registry <- list(
# Genres
    genres = list(
      scifi        = list(id = 101, category = "genre", display = "Sci-Fi"),
      action       = list(id = 102, category = "genre", display = "Action"),
      drama        = list(id = 103, category = "genre", display = "Drama"),
      comedy       = list(id = 104, category = "genre", display = "Comedy"),
      horror       = list(id = 105, category = "genre", display = "Horror"),
      thriller     = list(id = 106, category = "genre", display = "Thriller"),
      romance      = list(id = 107, category = "genre", display = "Romance"),
      fantasy      = list(id = 108, category = "genre", display = "Fantasy"),
      mystery      = list(id = 109, category = "genre", display = "Mystery"),
      documentary  = list(id = 110, category = "genre", display = "Documentary"),
      animation    = list(id = 111, category = "genre", display = "Animation"),
      biographical = list(id = 112, category = "genre", display = "Biographical")
    ),
    
# Topics
    topics = list(
      ai                    = list(id = 201, category = "topic", display = "AI"),
      space                 = list(id = 202, category = "topic", display = "Space"),
      mathematics           = list(id = 203, category = "topic", display = "Mathematics"),
      technology            = list(id = 204, category = "topic", display = "Technology"),
      physics               = list(id = 205, category = "topic", display = "Physics"),
      biology               = list(id = 206, category = "topic", display = "Biology"),
      history               = list(id = 207, category = "topic", display = "History"),
      psychology            = list(id = 208, category = "topic", display = "Psychology"),
      philosophy            = list(id = 209, category = "topic", display = "Philosophy"),
      economics             = list(id = 210, category = "topic", display = "Economics"),
      politics              = list(id = 211, category = "topic", display = "Politics"),
      nature                = list(id = 212, category = "topic", display = "Nature"),
      gaming                = list(id = 213, category = "topic", display = "Gaming"),
      music                 = list(id = 214, category = "topic", display = "Music"),
      sports                = list(id = 215, category = "topic", display = "Sports"),
      cooking               = list(id = 216, category = "topic", display = "Cooking"),
      travel                = list(id = 217, category = "topic", display = "Travel"),
      fitness               = list(id = 218, category = "topic", display = "Fitness"),
      business              = list(id = 219, category = "topic", display = "Business"),
      finance               = list(id = 220, category = "topic", display = "Finance"),
      machine_learning      = list(id = 221, category = "topic", display = "Machine Learning"),
      computer_vision       = list(id = 222, category = "topic", display = "Computer Vision"),
      nlp                   = list(id = 223, category = "topic", display = "NLP"),
      robotics              = list(id = 224, category = "topic", display = "Robotics"),
      cybersecurity         = list(id = 225, category = "topic", display = "Cybersecurity"),
      databases             = list(id = 226, category = "topic", display = "Databases"),
      distributed_systems   = list(id = 227, category = "topic", display = "Distributed Systems"),
      neuroscience          = list(id = 228, category = "topic", display = "Neuroscience"),
      computational_neuro   = list(id = 229, category = "topic", display = "Computational Neuroscience"),
      art                   = list(id = 230, category = "topic", display = "Art"),
      design                = list(id = 231, category = "topic", display = "Design"),
      education             = list(id = 232, category = "topic", display = "Education")
    ),
    
# Styles
    styles = list(
      educational   = list(id = 301, category = "style", display = "Educational"),
      entertainment = list(id = 302, category = "style", display = "Entertainment"),
      tutorial      = list(id = 303, category = "style", display = "Tutorial"),
      review        = list(id = 304, category = "style", display = "Review"),
      news          = list(id = 305, category = "style", display = "News"),
      interview     = list(id = 306, category = "style", display = "Interview"),
      lecture       = list(id = 307, category = "style", display = "Lecture"),
      vlog          = list(id = 308, category = "style", display = "Vlog"),
      analysis      = list(id = 309, category = "style", display = "Analysis"),
      debate        = list(id = 310, category = "style", display = "Debate"),
      narrative     = list(id = 311, category = "style", display = "Narrative"),
      experimental  = list(id = 312, category = "style", display = "Experimental")
    ),
    
# Format
    formats = list(
      short_form    = list(id = 401, category = "format", display = "Short Form"),
      long_form     = list(id = 402, category = "format", display = "Long Form"),
      series        = list(id = 403, category = "format", display = "Series"),
      standalone    = list(id = 404, category = "format", display = "Standalone"),
      live          = list(id = 405, category = "format", display = "Live"),
      podcast       = list(id = 406, category = "format", display = "Podcast")
    ),
    
# Complexity
    complexity = list(
      beginner      = list(id = 501, category = "complexity", display = "Beginner"),
      intermediate  = list(id = 502, category = "complexity", display = "Intermediate"),
      advanced      = list(id = 503, category = "complexity", display = "Advanced"),
      expert        = list(id = 504, category = "complexity", display = "Expert")
    ),
    
# Mood
    moods = list(
      exciting      = list(id = 601, category = "mood", display = "Exciting"),
      relaxing      = list(id = 602, category = "mood", display = "Relaxing"),
      intense       = list(id = 603, category = "mood", display = "Intense"),
      thoughtful    = list(id = 604, category = "mood", display = "Thoughtful"),
      funny         = list(id = 605, category = "mood", display = "Funny"),
      inspiring     = list(id = 606, category = "mood", display = "Inspiring"),
      dark          = list(id = 607, category = "mood", display = "Dark"),
      nostalgic     = list(id = 608, category = "mood", display = "Nostalgic"),
      suspenseful   = list(id = 609, category = "mood", display = "Suspenseful")
    )
  )
  
  return(registry)
}

# get all dimension names from the registry
# flattens the hierarchical structure into a single list of names
get_all_dimensions <- function(registry = NULL) {
  if (is.null(registry)) {
    registry <- create_code_registry()
  }
  
  dims <- c()
  for (cat_name in names(registry)) {
    cat_items <- registry[[cat_name]]
    dims <- c(dims, names(cat_items))
  }
  return(dims)
}

# get category for a given dimension name
# useful for hierarchical stuff later
get_dimension_category <- function(dim_name, registry = NULL) {
  if (is.null(registry)) {
    registry <- create_code_registry()
  }
  
  for (cat_name in names(registry)) {
    if (dim_name %in% names(registry[[cat_name]])) {
      return(registry[[cat_name]][[dim_name]]$category)
    }
  }
  return(NA)
}

# create a content code vector for an item
#
# takes named list of dimension values and returns a proper named vector
# validates that all dimensions exist in the registry
#
# example usage:
#   create_content_code(scifi = 0.9, action = 0.3, space = 0.8)
create_content_code <- function(..., registry = NULL) {
  if (is.null(registry)) {
    registry <- create_code_registry()
  }
  
  args <- list(...)
  
  # handle case where a single named list is passed
  if (length(args) == 1 && is.list(args[[1]]) && !is.null(names(args[[1]]))) {
    args <- args[[1]]
  }
  
  all_dims <- get_all_dimensions(registry)
  
  # validate dimension names
  for (name in names(args)) {
    if (!(name %in% all_dims)) {
      warning(sprintf("unknown dimension '%s' — adding anyway but it won't match registry", name))
    }
  }
  
  # build the vector
  code_vec <- unlist(args)
  
  # clamp values to [0, 1] — content codes should be normalised
  code_vec <- pmax(0, pmin(1, code_vec))
  
  return(code_vec)
}

# create a content item with metadata and content code
# this is the full representation of an item in the catalogue
create_item <- function(item_id, title, code_vector, duration = NA, 
                        created_at = NA, metadata = list()) {
  item <- list(
    id         = item_id,
    title      = title,
    code       = code_vector,
    duration   = duration,
    created_at = created_at,
    metadata   = metadata
  )
  class(item) <- "apc_item"
  return(item)
}

# pretty print for items
print.apc_item <- function(x, ...) {
  cat(sprintf("Item: %s (id=%s)\n", x$title, x$id))
  if (!is.na(x$duration)) {
    cat(sprintf("  Duration: %.0f seconds\n", x$duration))
  }
  cat("  Content Code:\n")
  print_vector(x$code, top_n = 8)
}

# create an item catalogue — holds all items with their content codes
create_catalogue <- function() {
  catalogue <- list(
    items = list(),
    all_dimensions = character(0),
    n_items = 0
  )
  class(catalogue) <- "apc_catalogue"
  return(catalogue)
}

# add an item to the catalogue
add_item <- function(catalogue, item) {
  stopifnot(inherits(catalogue, "apc_catalogue"))
  stopifnot(inherits(item, "apc_item"))
  
  catalogue$items[[item$id]] <- item
  catalogue$all_dimensions <- union(catalogue$all_dimensions, names(item$code))
  catalogue$n_items <- length(catalogue$items)
  
  return(catalogue)
}

# get the code matrix for all items in the catalogue
# returns a matrix where rows are items and columns are dimensions
# useful for batch computations
get_code_matrix <- function(catalogue) {
  stopifnot(inherits(catalogue, "apc_catalogue"))
  
  if (catalogue$n_items == 0) {
    return(matrix(0, nrow = 0, ncol = 0))
  }
  
  dims <- catalogue$all_dimensions
  n_items <- catalogue$n_items
  
  mat <- matrix(0, nrow = n_items, ncol = length(dims),
                dimnames = list(names(catalogue$items), dims))
  
  for (i in seq_along(catalogue$items)) {
    item <- catalogue$items[[i]]
    common_dims <- intersect(names(item$code), dims)
    if (length(common_dims) > 0) {
      mat[i, common_dims] <- item$code[common_dims]
    }
  }
  
  return(mat)
}

# generate a synthetic item catalogue for testing
# creates items with realistic-looking content codes
# 
# i spent a while making these somewhat realistic — the combinations
# should make sense (e.g., a scifi space documentary should have high
# values in scifi, space, and documentary)
generate_synthetic_catalogue <- function(n_items = 500, seed = 42) {
  set.seed(seed)
  
  registry <- create_code_registry()
  all_dims <- get_all_dimensions(registry)
  
  catalogue <- create_catalogue()
  
  # define some content archetypes to sample from
  # each archetype has a set of "likely" dimensions with base probabilities
  archetypes <- list(
    scifi_tech = list(
      dims = c(scifi = 0.85, technology = 0.7, space = 0.5, ai = 0.4, 
               exciting = 0.6, advanced = 0.5, long_form = 0.6),
      weight = 0.12
    ),
    ai_education = list(
      dims = c(ai = 0.9, machine_learning = 0.7, technology = 0.6, 
               educational = 0.8, tutorial = 0.5, intermediate = 0.5,
               thoughtful = 0.4),
      weight = 0.10
    ),
    action_thriller = list(
      dims = c(action = 0.9, thriller = 0.6, exciting = 0.8, intense = 0.7,
               entertainment = 0.7, long_form = 0.5),
      weight = 0.10
    ),
    math_science = list(
      dims = c(mathematics = 0.85, physics = 0.5, educational = 0.7,
               lecture = 0.5, advanced = 0.4, thoughtful = 0.6),
      weight = 0.08
    ),
    gaming_fun = list(
      dims = c(gaming = 0.9, entertainment = 0.8, exciting = 0.5,
               funny = 0.4, short_form = 0.5, comedy = 0.3),
      weight = 0.10
    ),
    nature_doc = list(
      dims = c(nature = 0.9, documentary = 0.8, biology = 0.5,
               relaxing = 0.5, inspiring = 0.4, long_form = 0.6,
               educational = 0.4),
      weight = 0.08
    ),
    comedy_entertainment = list(
      dims = c(comedy = 0.9, entertainment = 0.8, funny = 0.9,
               short_form = 0.5, vlog = 0.3),
      weight = 0.08
    ),
    drama_narrative = list(
      dims = c(drama = 0.9, narrative = 0.7, thoughtful = 0.6,
               long_form = 0.6, inspiring = 0.3, psychology = 0.3),
      weight = 0.08
    ),
    business_finance = list(
      dims = c(business = 0.8, finance = 0.7, economics = 0.5,
               analysis = 0.6, educational = 0.4, intermediate = 0.4),
      weight = 0.06
    ),
    music_arts = list(
      dims = c(music = 0.9, art = 0.5, entertainment = 0.6,
               inspiring = 0.5, relaxing = 0.4),
      weight = 0.05
    ),
    history_doc = list(
      dims = c(history = 0.9, documentary = 0.7, educational = 0.5,
               narrative = 0.5, thoughtful = 0.4, long_form = 0.5),
      weight = 0.05
    ),
    fitness_health = list(
      dims = c(fitness = 0.9, tutorial = 0.5, educational = 0.3,
               exciting = 0.3, short_form = 0.4, inspiring = 0.4),
      weight = 0.05
    ),
    cybersecurity_tech = list(
      dims = c(cybersecurity = 0.8, technology = 0.7, tutorial = 0.5,
               advanced = 0.5, analysis = 0.4, intense = 0.3),
      weight = 0.05
    )
  )
  
  # compute archetype sampling probabilities
  arch_names <- names(archetypes)
  arch_weights <- sapply(archetypes, function(a) a$weight)
  arch_probs <- arch_weights / sum(arch_weights)
  
  # some sample titles for each archetype — not trying to be creative here,
  # just need something recognisable for debugging
  title_templates <- list(
    scifi_tech = c("Neural Frontier", "Stellar Code", "Quantum Edge", 
                   "Synthetic Horizon", "Digital Galaxy", "Cyber Nebula",
                   "Void Protocol", "Neon Cosmos", "Binary Stars"),
    ai_education = c("Deep Learning Explained", "Neural Nets 101", 
                     "Understanding Transformers", "ML Fundamentals",
                     "AI Research Overview", "Gradient Descent Tutorial",
                     "Backpropagation Deep Dive"),
    action_thriller = c("Last Stand", "Dark Pursuit", "Rogue Agent",
                        "Silent Strike", "Breach Point", "Double Cross",
                        "Red Zone", "Final Protocol"),
    math_science = c("Group Theory Intro", "Real Analysis Lecture",
                     "Linear Algebra Essentials", "Topology Basics",
                     "Number Theory", "Calculus Revisited",
                     "Probability Deep Dive"),
    gaming_fun = c("Epic Fails Compilation", "Speedrun Challenge",
                   "New Game Review", "Boss Fight Strategy",
                   "Retro Gaming Night", "Indie Gems"),
    nature_doc = c("Ocean Depths", "Amazon Rainforest", "Arctic Wildlife",
                   "Migration Patterns", "Coral Reef Life",
                   "Mountain Ecosystems"),
    comedy_entertainment = c("Stand-up Special", "Comedy Sketches",
                             "Funny Moments", "Prank Compilation",
                             "Roast Battle", "Improv Night"),
    drama_narrative = c("Broken Promises", "The Long Road", "Silent Voices",
                        "Echoes of Yesterday", "Unspoken Truth",
                        "Beyond the Surface"),
    business_finance = c("Market Analysis", "Startup Lessons",
                         "Investment Strategy", "Economy Explained",
                         "Business Case Study", "Financial Planning"),
    music_arts = c("Jazz Sessions", "Classical Masterpieces",
                   "Music Production Tips", "Art Exhibition Tour",
                   "Songwriting Process", "Live Performance"),
    history_doc = c("Ancient Civilizations", "World War Perspectives",
                    "Industrial Revolution", "Medieval Europe",
                    "Cold War Chronicles", "Renaissance Era"),
    fitness_health = c("HIIT Workout", "Yoga Flow", "Strength Training",
                       "Mobility Routine", "Marathon Prep",
                       "Nutrition Guide"),
    cybersecurity_tech = c("Ethical Hacking", "Network Security",
                           "Penetration Testing", "CTF Walkthrough",
                           "Security Audit", "Malware Analysis")
  )
  
  for (i in seq_len(n_items)) {
    # pick an archetype
    arch_idx <- sample(seq_along(arch_names), 1, prob = arch_probs)
    arch_name <- arch_names[arch_idx]
    arch <- archetypes[[arch_name]]
    
    # generate the content code with some noise
    code_vals <- arch$dims
    noise <- rnorm(length(code_vals), mean = 0, sd = 0.15)
    code_vals <- pmax(0, pmin(1, code_vals + noise))
    
    # maybe add 1-3 extra random dimensions with small values
    # this makes items more interesting and creates unexpected combinations
    n_extra <- sample(0:3, 1, prob = c(0.3, 0.4, 0.2, 0.1))
    if (n_extra > 0) {
      extra_dims <- sample(setdiff(all_dims, names(code_vals)), 
                           min(n_extra, length(setdiff(all_dims, names(code_vals)))))
      extra_vals <- runif(length(extra_dims), 0.05, 0.35)
      names(extra_vals) <- extra_dims
      code_vals <- c(code_vals, extra_vals)
    }
    
    # generate title
    titles <- title_templates[[arch_name]]
    title <- paste0(sample(titles, 1), " ", i)
    
    # create the item
    item <- create_item(
      item_id    = sprintf("item_%04d", i),
      title      = title,
      code_vector = create_content_code(as.list(code_vals)),
      duration   = sample(c(120, 300, 600, 900, 1800, 3600, 5400, 7200), 1,
                          prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.05, 0.05)),
      created_at = as.numeric(Sys.time()) - runif(1, 0, 120 * 24 * 3600)
    )
    
    catalogue <- add_item(catalogue, item)
  }
  
  return(catalogue)
}

# compute pairwise similarity matrix for all items
# this can be expensive for large catalogues — mainly for evaluation
compute_item_similarity_matrix <- function(catalogue, method = "cosine") {
  mat <- get_code_matrix(catalogue)
  n <- nrow(mat)
  
  sim_mat <- matrix(0, n, n, dimnames = list(rownames(mat), rownames(mat)))
  
  for (i in seq_len(n)) {
    for (j in i:n) {
      if (method == "cosine") {
        s <- cosine_similarity(mat[i, ], mat[j, ])
      } else if (method == "jaccard") {
        # binarise and use jaccard
        a_dims <- names(which(mat[i, ] > 0.1))
        b_dims <- names(which(mat[j, ] > 0.1))
        s <- jaccard_similarity(a_dims, b_dims)
      } else {
        stop(sprintf("unknown similarity method: %s", method))
      }
      sim_mat[i, j] <- s
      sim_mat[j, i] <- s
    }
  }
  
  return(sim_mat)
}

# find nearest neighbours for a given item
find_similar_items <- function(catalogue, item_id, k = 10, method = "cosine") {
  target <- catalogue$items[[item_id]]
  if (is.null(target)) stop(sprintf("item %s not found in catalogue", item_id))
  
  sims <- sapply(catalogue$items, function(item) {
    if (item$id == item_id) return(-1)  # exclude self
    cosine_similarity(target$code, item$code)
  })
  
  sorted_idx <- order(sims, decreasing = TRUE)
  top_idx <- sorted_idx[1:min(k, length(sorted_idx))]
  
  return(list(
    item_ids     = names(sims)[top_idx],
    similarities = sims[top_idx]
  ))
}
