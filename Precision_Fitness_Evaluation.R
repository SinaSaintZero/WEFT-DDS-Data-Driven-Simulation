#
# PDE EVALUATION MODULE (LOG vs Final_rel)
#required libraries
library(stringdist)


# 1. Extract traces from log
# -----------------------------
get_traces <- function(dt) {
  dt[, .(trace = list(Activity)), by = ID]
}

traces <- get_traces(test_log)
#---

# 2. Build edge set from PDE
# --------------
pde_edges <- paste(Final_rel$From, Final_rel$To, sep = "->")
#-----

# 3. FITNESS
# How many log transitions are allowed by PDE
# -----------------------------
compute_fitness_pde <- function(traces, pde_edges) {
  
  total <- 0
  valid <- 0
  
  for (t in traces$trace) {
    
    if (length(t) < 2) next
    
    for (i in 1:(length(t)-1)) {
      
      total <- total + 1
      
      edge <- paste(t[i], t[i+1], sep = "->")
      
      if (edge %in% pde_edges) {
        valid <- valid + 1
      }
    }
  }
  
  valid / total
}

# Now, we go to precision evaluation
# 4. PRECISION
# How many PDE edges are actually used in log
# -----------------------------
compute_precision_pde <- function(traces, pde_edges) {
  
  observed_edges <- c()
  
  for (t in traces$trace) {
    
    if (length(t) < 2) next
    
    for (i in 1:(length(t)-1)) {
      observed_edges <- c(observed_edges,
                          paste(t[i], t[i+1], sep = "->"))
    }
  }
  
  observed_edges <- unique(observed_edges)
  
  length(intersect(pde_edges, observed_edges)) / length(pde_edges)
}

# 
# 5. BEHAVIORAL SIMILARITY
# (Edge-level Jaccard)
# -----------------------------
compute_edge_jaccard <- function(traces, pde_edges) {
  
  observed_edges <- c()
  
  for (t in traces$trace) {
    if (length(t) < 2) next
    for (i in 1:(length(t)-1)) {
      observed_edges <- c(observed_edges,
                          paste(t[i], t[i+1], sep = "->"))
    }
  }
  
  observed_edges <- unique(observed_edges)
  
  intersection <- length(intersect(observed_edges, pde_edges))
  union        <- length(union(observed_edges, pde_edges))
  
  if (union == 0) return(0)
  
  intersection / union
}

#
# 6. TRACE-LEVEL COVERAGE --> This is importnat for our Simulation model
# % of traces fully reproducible by PDE
# -----------------------------
compute_trace_coverage <- function(traces, pde_edges) {
  
  valid_traces <- 0
  
  for (t in traces$trace) {
    
    if (length(t) < 2) next
    
    valid <- TRUE
    
    for (i in 1:(length(t)-1)) {
      
      edge <- paste(t[i], t[i+1], sep = "->")
      
      if (!(edge %in% pde_edges)) {
        valid <- FALSE
        break
      }
    }
    
    if (valid) valid_traces <- valid_traces + 1
  }
  
  valid_traces / nrow(traces)
}

#--- compute F-score
compute_fscore <- function(fitness, precision) {
  if ((fitness + precision) == 0) return(0)
  2 * (fitness * precision) / (fitness + precision)
}
      #-------- put everything together
# 7. RUN EVALUATION
# -----------------------------
fitness  <- compute_fitness_pde(traces, pde_edges)
precision <- compute_precision_pde(traces, pde_edges)
jaccard   <- compute_edge_jaccard(traces, pde_edges)
coverage  <- compute_trace_coverage(traces, pde_edges)
fscore <- compute_fscore(fitness, precision)

results_pde <- list(
  fitness = fitness,
  precision = precision,
  fscore = fscore,   
  edge_jaccard = jaccard,
  trace_coverage = coverage,
  n_edges = length(pde_edges)
)

print(results_pde)

