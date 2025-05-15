# APC — Accumulated Preference Code Recommendation Engine

An advanced recommendation system built in R that models user preferences as a continuously evolving, multi-dimensional numerical state rather than relying solely on conventional collaborative filtering or static content-based similarity.

**Author:** Vedanth
**Created:** 2025

## Overview

APC (Accumulated Preference Codes) addresses the fundamental problem of how to represent, update, and utilise a user's changing tastes over time. 

In conventional recommender systems, interaction history is often either aggregated statically (making it slow to respond to new interests) or modeled as a black-box sequence (making it difficult to inspect *why* an item is recommended). 

APC takes a different approach:
1. Every item is represented as a multidimensional vector of "content codes" (e.g., `SciFi=0.9`, `Space=0.8`, `Educational=0.6`).
2. Users have a corresponding "preference state" vector.
3. Every interaction (watch, like, skip, etc.) modifies the user's preference state, weighted by the interaction type and engagement level.
4. Older preferences decay exponentially, but *different dimensions decay at different rates* (e.g., "moods" decay faster than "genres").
5. The system computes preference **momentum** (the derivative of preference strength over time) to detect emerging interests before they become dominant.

## Core Features

- **Temporal Preference Decay:** Exponential decay with per-dimension and adaptive lambda values. 
- **Preference Momentum:** Detects rising, falling, and stable interests by computing rolling velocities over preference snapshots.
- **Compound Codes:** Detects statistically significant combinations (e.g., `AI × Mathematics`) using Mutual Information, Lift, or Chi-squared tests to prevent combinatorial explosion.
- **Hierarchical Propagation:** Content codes sit in a hierarchy (e.g., `Machine Learning -> AI -> Technology`). Signal propagates up and down the tree.
- **Controlled Exploration:** Prevents recommendation bubbles using UCB, Epsilon-greedy, or Hierarchically-adjacent exploration strategies.
- **Explicit Negative Modelling:** Differentiates between "unseen" and "actively disliked" by tracking repeated skips and low-completion events, applying targeted suppression.

## Project Structure

The project is structured like a pure R research package, avoiding complex external dependencies where possible to keep the algorithm transparent.

```
apc/
├── R/
│   ├── codes.R                 # Content taxonomy and catalogue
│   ├── utils.R                 # Maths and vector utilities
│   ├── interaction_weights.R   # Signal extraction from behaviour
│   ├── decay.R                 # Temporal decay logic
│   ├── preference_state.R      # User state and update functions
│   ├── momentum.R              # Velocity/trend detection
│   ├── compound_codes.R        # Joint probability/MI detection
│   ├── hierarchy.R             # Tree structures and propagation
│   ├── scoring.R               # APC scoring function and coefficient learning
│   ├── exploration.R           # UCB and hierarchical exploration
│   ├── negative_preferences.R  # Explicit dislike modelling
│   ├── simulator.R             # Synthetic user behaviour engine
│   └── evaluation.R            # NDCG, ILD, MRR, Adaptation metrics
├── experiments/
│   ├── ablation_study.R        # Component importance evaluation
│   ├── baseline_comparison.R   # APC vs MF, CF, Popularity
│   ├── compound_code_test.R    # MI vs Lift vs Chi2
│   ├── decay_test.R            # Uniform vs adaptive decay
│   ├── exploration_test.R      # Relevance vs diversity tradeoff
│   └── preference_drift.R      # Adaptation speed measurement
├── tests/
│   ├── test_codes.R            # Unit tests
│   └── test_preference_state.R # Unit tests
├── data/                       # (Data directory for real datasets)
└── results/                    # (Generated plots and CSVs go here)
```

## Running the Experiments

To evaluate the algorithm, we use a synthetic user simulator (`R/simulator.R`) which allows us to know the *true* hidden preferences of users and measure how well the algorithm reconstructs them.

All experiments are completely reproducible. Simply run them from the project root:

```bash
# Measure how fast APC adapts to a sudden change in user tastes
Rscript experiments/preference_drift.R

# Compare APC against Collaborative Filtering and Matrix Factorisation
Rscript experiments/baseline_comparison.R

# Evaluate which APC components actually contribute to NDCG
Rscript experiments/ablation_study.R
```

Results (PNG plots and CSVs) will be saved to the `results/` directory.

## Research Question Answered

The primary experiment (`preference_drift.R`) evaluates: *"Can an accumulated, temporally adaptive representation of user preferences respond to changing interests faster than conventional static recommendation representations while maintaining recommendation relevance?"*

Yes. By explicitly modelling **momentum** and applying **per-dimension decay**, APC detects shifts in user interests significantly faster than static content-based accumulation or traditional matrix factorisation, which suffer from "historical drag" (where years of old data dilute new signals).

## Dependencies

Base R handles the vast majority of operations.
No major external recommender packages are used — the algorithms are built from scratch to maintain transparency. 
