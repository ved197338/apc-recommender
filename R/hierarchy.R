# APC Engine — Hierarchical category structure and signal propagation

#

###############################################################################

source("R/utils.R")

# define the content hierarchy
#
# this is a tree structure where each node can have children.
# the propagation_up value controls how much signal flows from child to parent.
# the propagation_down value controls parent to child flow.
#
# i deliberated over this structure for a while. the main question was
# whether to use a strict tree or allow DAGs (directed acyclic graphs).
# went with a tree for simplicity — DAGs create ambiguity about which
# path to propagate through.
create_default_hierarchy <- function() {
  hierarchy <- list(
    # TECHNOLOGY branch
    technology = list(
      children = c("ai", "cybersecurity", "databases", "distributed_systems",
                    "robotics"),
      propagation_up = 0.3,
      propagation_down = 0.15
    ),
    ai = list(
      parent = "technology",
      children = c("machine_learning", "computer_vision", "nlp"),
      propagation_up = 0.35,
      propagation_down = 0.2
    ),
    machine_learning = list(
      parent = "ai",
      children = character(0),
      propagation_up = 0.4,
      propagation_down = 0.0
    ),
    computer_vision = list(
      parent = "ai",
      children = character(0),
      propagation_up = 0.4,
      propagation_down = 0.0
    ),
    nlp = list(
      parent = "ai",
      children = character(0),
      propagation_up = 0.4,
      propagation_down = 0.0
    ),
    cybersecurity = list(
      parent = "technology",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    databases = list(
      parent = "technology",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    distributed_systems = list(
      parent = "technology",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    robotics = list(
      parent = "technology",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    
    # SCIENCE branch
    science = list(
      children = c("physics", "biology", "mathematics", "neuroscience",
                    "computational_neuro"),
      propagation_up = 0.25,
      propagation_down = 0.15
    ),
    physics = list(
      parent = "science",
      children = c("space"),
      propagation_up = 0.3,
      propagation_down = 0.2
    ),
    space = list(
      parent = "physics",
      children = character(0),
      propagation_up = 0.35,
      propagation_down = 0.0
    ),
    biology = list(
      parent = "science",
      children = c("nature"),
      propagation_up = 0.3,
      propagation_down = 0.2
    ),
    nature = list(
      parent = "biology",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    mathematics = list(
      parent = "science",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    neuroscience = list(
      parent = "science",
      children = c("computational_neuro"),
      propagation_up = 0.3,
      propagation_down = 0.2
    ),
    computational_neuro = list(
      parent = "neuroscience",
      children = character(0),
      propagation_up = 0.35,
      propagation_down = 0.0
    ),
    
    # CREATIVE / ENTERTAINMENT branch
    entertainment_root = list(
      children = c("gaming", "music", "art", "sports", "comedy",
                    "action", "drama", "thriller", "horror", "romance",
                    "fantasy", "mystery", "scifi", "animation"),
      propagation_up = 0.2,
      propagation_down = 0.1
    ),
    gaming = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.25,
      propagation_down = 0.0
    ),
    music = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    art = list(
      parent = "entertainment_root",
      children = c("design"),
      propagation_up = 0.25,
      propagation_down = 0.15
    ),
    design = list(
      parent = "art",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    sports = list(
      parent = "entertainment_root",
      children = c("fitness"),
      propagation_up = 0.25,
      propagation_down = 0.2
    ),
    fitness = list(
      parent = "sports",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    
    # genre connections — these are flat under entertainment
    comedy = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    action = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    drama = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    thriller = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    horror = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    romance = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    fantasy = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    mystery = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    scifi = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    animation = list(
      parent = "entertainment_root",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    
    # BUSINESS / SOCIAL branch
    business_root = list(
      children = c("business", "finance", "economics", "politics"),
      propagation_up = 0.25,
      propagation_down = 0.15
    ),
    business = list(
      parent = "business_root",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    finance = list(
      parent = "business_root",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    economics = list(
      parent = "business_root",
      children = character(0),
      propagation_up = 0.3,
      propagation_down = 0.0
    ),
    politics = list(
      parent = "business_root",
      children = character(0),
      propagation_up = 0.25,
      propagation_down = 0.0
    ),
    
    # HUMAN INTEREST
    human_interest = list(
      children = c("psychology", "philosophy", "history", "education",
                    "travel", "cooking", "biographical"),
      propagation_up = 0.2,
      propagation_down = 0.1
    ),
    psychology = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.25,
      propagation_down = 0.0
    ),
    philosophy = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.25,
      propagation_down = 0.0
    ),
    history = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.25,
      propagation_down = 0.0
    ),
    education = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    travel = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    cooking = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    ),
    biographical = list(
      parent = "human_interest",
      children = character(0),
      propagation_up = 0.2,
      propagation_down = 0.0
    )
  )
  
  class(hierarchy) <- "apc_hierarchy"
  return(hierarchy)
}

# get ancestors of a node (path to root)
get_ancestors <- function(node_name, hierarchy) {
  ancestors <- character(0)
  current <- node_name
  
  # safety counter to avoid infinite loops if the hierarchy is malformed
  max_depth <- 20
  depth <- 0
  
  while (depth < max_depth) {
    if (!(current %in% names(hierarchy))) break
    node <- hierarchy[[current]]
    if (is.null(node$parent)) break
    
    ancestors <- c(ancestors, node$parent)
    current <- node$parent
    depth <- depth + 1
  }
  
  return(ancestors)
}

# get all descendants of a node (recursively)
get_descendants <- function(node_name, hierarchy) {
  if (!(node_name %in% names(hierarchy))) return(character(0))
  
  node <- hierarchy[[node_name]]
  children <- node$children
  
  if (length(children) == 0) return(character(0))
  
  descendants <- children
  for (child in children) {
    descendants <- c(descendants, get_descendants(child, hierarchy))
  }
  
  return(descendants)
}

# get the depth of a node in the hierarchy (root = 0)
get_node_depth <- function(node_name, hierarchy) {
  return(length(get_ancestors(node_name, hierarchy)))
}

# propagate preferences through the hierarchy
#
# this is the key function. given a preference vector with values at
# specific nodes, propagate signal up to parents and down to children
# based on the propagation weights.
#
# the propagation is multiplicative and diminishes at each level.
# so if you have:
#   machine_learning: 0.9
#   ML → AI propagation_up: 0.4
#   AI → technology propagation_up: 0.3
#
# then:
#   AI gets: 0.9 * 0.4 = 0.36
#   technology gets: 0.36 * 0.3 = 0.108
#
# this feels right intuitively — someone who loves ML definitely has
# some interest in AI, but isn't necessarily into all of technology.
propagate_preferences <- function(preference_vector, hierarchy,
                                   direction = "both",
                                   max_depth = 5) {
  propagated <- preference_vector
  
  if (direction %in% c("up", "both")) {
    propagated <- propagate_up(propagated, hierarchy, max_depth)
  }
  
  if (direction %in% c("down", "both")) {
    propagated <- propagate_down(propagated, hierarchy, max_depth)
  }
  
  return(propagated)
}

# upward propagation — from specific to general
propagate_up <- function(preference_vector, hierarchy, max_depth = 5) {
  result <- preference_vector
  
  # for each dimension that has a value, propagate up to ancestors
  for (dim_name in names(preference_vector)) {
    if (!(dim_name %in% names(hierarchy))) next
    
    value <- preference_vector[dim_name]
    if (abs(value) < 1e-10) next
    
    # walk up the hierarchy
    current <- dim_name
    current_value <- value
    depth <- 0
    
    while (depth < max_depth) {
      node <- hierarchy[[current]]
      if (is.null(node) || is.null(node$parent)) break
      
      parent <- node$parent
      prop_weight <- node$propagation_up
      
      # propagated value
      propagated_value <- current_value * prop_weight
      
      if (abs(propagated_value) < 1e-6) break
      
      # add to parent (don't override, accumulate)
      if (parent %in% names(result)) {
        result[parent] <- result[parent] + propagated_value
      } else {
        result[parent] <- propagated_value
      }
      
      current <- parent
      current_value <- propagated_value
      depth <- depth + 1
    }
  }
  
  return(result)
}

# downward propagation — from general to specific
propagate_down <- function(preference_vector, hierarchy, max_depth = 3) {
  result <- preference_vector
  
  for (dim_name in names(preference_vector)) {
    if (!(dim_name %in% names(hierarchy))) next
    
    value <- preference_vector[dim_name]
    if (abs(value) < 1e-10) next
    
    node <- hierarchy[[dim_name]]
    if (length(node$children) == 0) next
    
    # propagate to each child
    prop_weight <- node$propagation_down
    propagated_value <- value * prop_weight
    
    if (abs(propagated_value) < 1e-6) next
    
    for (child in node$children) {
      if (child %in% names(result)) {
        result[child] <- result[child] + propagated_value
      } else {
        result[child] <- propagated_value
      }
      
      # recursively propagate deeper (with reduced max_depth)
      if (max_depth > 1 && child %in% names(hierarchy)) {
        child_node <- hierarchy[[child]]
        if (length(child_node$children) > 0) {
          child_vec <- c()
          child_vec[child] <- propagated_value
          deeper <- propagate_down(child_vec, hierarchy, max_depth - 1)
          
          for (deep_name in names(deeper)) {
            if (deep_name == child) next  # skip the one we already added
            if (deep_name %in% names(result)) {
              result[deep_name] <- result[deep_name] + deeper[deep_name]
            } else {
              result[deep_name] <- deeper[deep_name]
            }
          }
        }
      }
    }
  }
  
  return(result)
}

# compute hierarchical similarity between two vectors
# this considers the hierarchy — if user likes AI and item is ML,
# they're more similar than AI and cooking, even though AI and ML
# might not have the same exact dimension name.
compute_hierarchical_similarity <- function(vec_a, vec_b, hierarchy) {
  # propagate both vectors through the hierarchy
  a_prop <- propagate_preferences(vec_a, hierarchy, direction = "both")
  b_prop <- propagate_preferences(vec_b, hierarchy, direction = "both")
  
  return(cosine_similarity(a_prop, b_prop))
}

# get the hierarchical distance between two nodes
# distance = number of edges in the shortest path through the tree
get_hierarchical_distance <- function(node_a, node_b, hierarchy) {
  if (node_a == node_b) return(0)
  
  # get ancestors of both
  ancestors_a <- c(node_a, get_ancestors(node_a, hierarchy))
  ancestors_b <- c(node_b, get_ancestors(node_b, hierarchy))
  
  # find lowest common ancestor
  lca <- intersect(ancestors_a, ancestors_b)
  
  if (length(lca) == 0) {
    # no common ancestor — maximum distance
    return(length(ancestors_a) + length(ancestors_b) + 2)
  }
  
  # distance is depth_a_to_lca + depth_b_to_lca
  lca_node <- lca[1]  # first (lowest) common ancestor
  dist_a <- match(lca_node, ancestors_a) - 1
  dist_b <- match(lca_node, ancestors_b) - 1
  
  return(dist_a + dist_b)
}

# find dimensions that are "adjacent" to a given set of dimensions
# in the hierarchy. useful for exploration.
#
# "adjacent" means siblings, parents, or children in the hierarchy
find_adjacent_dimensions <- function(dimensions, hierarchy, 
                                      max_distance = 2) {
  adjacent <- character(0)
  
  for (dim_name in dimensions) {
    if (!(dim_name %in% names(hierarchy))) next
    node <- hierarchy[[dim_name]]
    
    # add parent
    if (!is.null(node$parent)) {
      adjacent <- c(adjacent, node$parent)
      
      # add siblings (other children of parent)
      parent_node <- hierarchy[[node$parent]]
      if (!is.null(parent_node)) {
        siblings <- setdiff(parent_node$children, dim_name)
        adjacent <- c(adjacent, siblings)
      }
    }
    
    # add children
    adjacent <- c(adjacent, node$children)
    
    # if max_distance > 1, go one level deeper
    if (max_distance > 1) {
      for (child in node$children) {
        if (child %in% names(hierarchy)) {
          adjacent <- c(adjacent, hierarchy[[child]]$children)
        }
      }
    }
  }
  
  # remove the input dimensions themselves and duplicates
  adjacent <- setdiff(unique(adjacent), dimensions)
  
  return(adjacent)
}

# visualise the hierarchy as an indented tree
# helpful for debugging and documentation
print_hierarchy <- function(hierarchy, root_nodes = NULL, indent = 0) {
  if (is.null(root_nodes)) {
    # find root nodes (nodes without parents)
    root_nodes <- names(hierarchy)[sapply(hierarchy, function(n) {
      is.null(n$parent)
    })]
  }
  
  for (root in root_nodes) {
    if (!(root %in% names(hierarchy))) next
    
    node <- hierarchy[[root]]
    prefix <- paste(rep("  ", indent), collapse = "")
    
    cat(sprintf("%s├── %s", prefix, root))
    if (!is.null(node$propagation_up)) {
      cat(sprintf(" [up=%.2f, down=%.2f]", 
                  node$propagation_up, node$propagation_down))
    }
    cat("\n")
    
    if (length(node$children) > 0) {
      print_hierarchy(hierarchy, node$children, indent + 1)
    }
  }
}
