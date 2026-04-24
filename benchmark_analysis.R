###############################################################
# BENCHMARK MODULE: WEFTSIM vs BASELINE DISCOVERY METHODS
# Self-contained version
# No external WEFTSIM function required
###############################################################

library(data.table)

#--- pm4py
###############################################################
# 0. PYTHON / PM4PY SETUP (INDUCTIVE MINER)
###############################################################

library(reticulate)
py_config()
reticulate::install_python(version = "3.10")
Sys.setenv(RETICULATE_PYTHON = 
             "C:/Users/snamakia/AppData/Local/r-reticulate/python/3.10/python.exe");
reticulate::py_install(c("pm4py", "pandas"))
pm4py <- import("pm4py")
reticulate::install_python()
reticulate::virtualenv_create("pm4py_env")
reticulate::virtualenv_install("pm4py_env", c("pm4py", "pandas"))
# Load PM4Py safely
pm4py_available <- FALSE

tryCatch({
  pm4py <- reticulate::import("pm4py")
  pandas <- reticulate::import("pandas")
  pm4py_available <- TRUE
  message("PM4Py successfully loaded.")
}, error = function(e) {
  message("PM4Py still not available.")
})
###############################################################
# BENCHMARK MODULE: WEFTSIM vs BASELINE DISCOVERY METHODS
###############################################################

library(data.table)
library(reticulate)

###############################################################
# 0. PM4PY SETUP
###############################################################

pm4py_available <- FALSE

tryCatch({
  pm4py <- import("pm4py")
  pandas <- import("pandas")
  pm4py_available <- TRUE
  message("PM4Py successfully loaded.")
}, error = function(e) {
  message("PM4Py not available. Inductive Miner will be skipped.")
})

if (pm4py_available) {
  py_run_string("
import time
import pandas as pd
import pm4py

def run_inductive_from_dataframe(df):
    df = df.copy()

    df = df.rename(columns={
        'case_id': 'case:concept:name',
        'activity': 'concept:name',
        'timestamp': 'time:timestamp'
    })

    df['case:concept:name'] = df['case:concept:name'].astype(str)
    df['concept:name'] = df['concept:name'].astype(str)
    df['time:timestamp'] = pd.to_datetime(df['time:timestamp'])

    log = pm4py.convert_to_event_log(df)

    start = time.time()

    tree = pm4py.discover_process_tree_inductive(log)
    net, im, fm = pm4py.convert_to_petri_net(tree)

    runtime = time.time() - start

    fitness_result = pm4py.fitness_token_based_replay(log, net, im, fm)

    if isinstance(fitness_result, dict):
        fitness = fitness_result.get('log_fitness', None)
        if fitness is None:
            fitness = fitness_result.get('average_trace_fitness', None)
    else:
        fitness = fitness_result

    precision = pm4py.precision_token_based_replay(log, net, im, fm)

    activities = len([t for t in net.transitions if t.label is not None])
    edges = len(net.arcs)

    return {
        'fitness': float(fitness),
        'precision': float(precision),
        'runtime': float(runtime),
        'activities': int(activities),
        'edges': int(edges)
    }
")
}

###############################################################
# 1. LOG PREPARATION
###############################################################

prepare_log <- function(file_path) {
  
  log <- fread(file_path, encoding = "UTF-8")
  
  message("\nDetected columns:")
  print(names(log))
  
  case_candidates <- names(log)[grep("case.*concept.*name|case_id|case.id|case", names(log), ignore.case = TRUE)]
  case_col <- case_candidates[1]
  
  activity_candidates <- names(log)[grep("^activity$|concept:name|concept.*name|task|event|action", names(log), ignore.case = TRUE)]
  activity_candidates <- activity_candidates[!activity_candidates %in% case_col]
  activity_col <- activity_candidates[1]
  
  timestamp_candidates <- names(log)[grep("timestamp|time|date", names(log), ignore.case = TRUE)]
  timestamp_col <- timestamp_candidates[1]
  
  if (is.na(case_col)) stop("Case column not found.")
  if (is.na(activity_col)) stop("Activity column not found.")
  
  setnames(log, case_col, "case_id")
  setnames(log, activity_col, "activity")
  
  if (is.na(timestamp_col)) {
    message("No timestamp column found -> using original event order.")
    log[, timestamp := seq_len(.N)]
    log[, timestamp_order := seq_len(.N)]
  } else {
    setnames(log, timestamp_col, "timestamp")
    
    ts <- log$timestamp
    
    if (is.numeric(ts) || is.integer(ts)) {
      log[, timestamp_order := as.numeric(ts)]
    } else {
      ts_char <- as.character(ts)
      
      is_duration <- grepl("^\\d+:\\d+(\\.\\d+)?$", ts_char)
      
      if (sum(is_duration, na.rm = TRUE) > 0.8 * length(ts_char)) {
        
        convert_to_seconds <- function(x) {
          out <- rep(NA_real_, length(x))
          ok <- grepl("^\\d+:\\d+(\\.\\d+)?$", x)
          parts <- strsplit(x[ok], ":", fixed = TRUE)
          
          out[ok] <- sapply(parts, function(p) {
            as.numeric(p[1]) * 60 + as.numeric(p[2])
          })
          
          out
        }
        
        log[, timestamp_order := convert_to_seconds(ts_char)]
        
      } else {
        
        numeric_time <- suppressWarnings(as.numeric(ts_char))
        
        if (all(is.na(numeric_time))) {
          message("Timestamp not usable -> using original event order.")
          log[, timestamp_order := seq_len(.N)]
        } else {
          log[, timestamp_order := numeric_time]
          log[is.na(timestamp_order), timestamp_order := seq_len(.N)]
        }
      }
    }
  }
  
  log[, original_order := seq_len(.N)]
  
  log[, case_id := as.character(case_id)]
  log[, activity := as.character(activity)]
  
  setorder(log, case_id, timestamp_order, original_order)
  
  return(log[, .(case_id, activity, timestamp, timestamp_order, original_order)])
}

###############################################################
# 2. DIRECTLY-FOLLOWS GRAPH
###############################################################

extract_log_edges <- function(log) {
  
  x <- copy(log)
  setorder(x, case_id, timestamp_order, original_order)
  
  x[, next_activity := shift(activity, type = "lead"), by = case_id]
  
  edges <- x[!is.na(next_activity),
             .N,
             by = .(From = activity, To = next_activity)]
  
  setorder(edges, -N)
  return(edges)
}

###############################################################
# 3. TRACE EXTRACTION
###############################################################

get_traces <- function(log) {
  x <- copy(log)
  setorder(x, case_id, timestamp_order, original_order)
  x[, .(trace = list(activity)), by = case_id]
}

###############################################################
# 4. WEFTSIM EDGE DISCOVERY
###############################################################

discover_WEFTSIM_edges <- function(log) {
  
  dfg <- extract_log_edges(log)
  
  if (nrow(dfg) == 0) {
    return(data.table(From = character(), To = character(), N = integer()))
  }
  
  dfg[, MovingRange := abs(N - shift(N, fill = N[1]))]
  
  MR_average <- mean(dfg$MovingRange, na.rm = TRUE)
  x_average <- mean(dfg$N, na.rm = TRUE)
  
  UCL_edge <- x_average + 3 * (MR_average / 2.534)
  LCL_edge <- x_average - 3 * (MR_average / 2.534)
  
  stable_edges <- dfg[N > ceiling(LCL_edge) & N < ceiling(UCL_edge)]
  hot_edges <- dfg[N >= ceiling(UCL_edge)]
  
  edge_stability <- unique(rbindlist(list(stable_edges, hot_edges), fill = TRUE))
  
  activities <- unique(c(dfg$From, dfg$To))
  
  activity_stats <- rbindlist(lapply(activities, function(a) {
    outgoing <- dfg[From == a, N]
    if (length(outgoing) == 0) outgoing <- 0
    
    data.table(
      activity = a,
      size = length(outgoing),
      mean_freq = mean(outgoing, na.rm = TRUE),
      sd_freq = sd(outgoing, na.rm = TRUE)
    )
  }), fill = TRUE)
  
  activity_stats[is.na(sd_freq), sd_freq := 0]
  
  grand_average <- weighted.mean(
    activity_stats$mean_freq,
    activity_stats$size,
    na.rm = TRUE
  )
  
  avg_size <- mean(activity_stats$size[activity_stats$size > 0], na.rm = TRUE)
  if (is.na(avg_size) || avg_size <= 0) avg_size <- 1
  
  c4 <- ((4 * avg_size) - 1) / ((4 * avg_size) - 3)
  n_bar <- max(1, ceiling(nrow(dfg) / length(activities)))
  A3 <- 3 / (c4 * sqrt(n_bar))
  sigma_hat <- mean(activity_stats$sd_freq, na.rm = TRUE)
  
  UCL_activity <- ceiling(grand_average + A3 * c4 * sigma_hat)
  LCL_activity <- ceiling(grand_average - A3 * c4 * sigma_hat)
  
  stable_activities <- activity_stats[
    mean_freq >= LCL_activity | mean_freq >= UCL_activity,
    activity
  ]
  
  final_edges <- edge_stability[
    From %in% stable_activities & To %in% stable_activities,
    .(From, To, N)
  ]
  
  if (nrow(final_edges) == 0) {
    final_edges <- edge_stability[, .(From, To, N)]
  }
  
  if (nrow(final_edges) == 0) {
    final_edges <- dfg[, .(From, To, N)]
  }
  
  return(unique(final_edges))
}

###############################################################
# 5. BASELINE METHODS
###############################################################

discover_frequency_DFG <- function(log, percentile = 0.80) {
  
  dfg <- extract_log_edges(log)
  if (nrow(dfg) == 0) return(dfg)
  
  threshold <- as.numeric(quantile(dfg$N, percentile, na.rm = TRUE))
  dfg[N >= threshold, .(From, To, N)]
}

discover_heuristics_miner_like <- function(log, dependency_threshold = 0.50) {
  
  dfg <- extract_log_edges(log)
  if (nrow(dfg) == 0) return(dfg)
  
  reverse_dfg <- copy(dfg)
  setnames(reverse_dfg, c("From", "To", "N"), c("To", "From", "N_reverse"))
  
  dep <- merge(
    dfg,
    reverse_dfg,
    by = c("From", "To"),
    all.x = TRUE
  )
  
  dep[is.na(N_reverse), N_reverse := 0]
  dep[, dependency := (N - N_reverse) / (N + N_reverse + 1)]
  
  result <- dep[dependency >= dependency_threshold, .(From, To, N)]
  
  if (nrow(result) == 0) {
    result <- discover_frequency_DFG(log, percentile = 0.80)
  }
  
  return(unique(result))
}

discover_alpha_miner_like <- function(log) {
  
  dfg <- extract_log_edges(log)
  if (nrow(dfg) == 0) return(dfg)
  
  reverse_dfg <- copy(dfg)
  setnames(reverse_dfg, c("From", "To", "N"), c("To", "From", "N_reverse"))
  
  rel <- merge(
    dfg,
    reverse_dfg,
    by = c("From", "To"),
    all.x = TRUE
  )
  
  rel[is.na(N_reverse), N_reverse := 0]
  
  result <- rel[N_reverse == 0, .(From, To, N)]
  
  if (nrow(result) == 0) {
    result <- discover_frequency_DFG(log, percentile = 0.80)
  }
  
  return(unique(result))
}

###############################################################
# 6. INDUCTIVE MINER
###############################################################

run_inductive_miner <- function(log) {
  
  if (!pm4py_available) return(NULL)
  
  x <- copy(log)
  setorder(x, case_id, timestamp_order, original_order)
  
  x[, event_index := seq_len(.N), by = case_id]
  x[, safe_time := as.POSIXct(event_index, origin = "1970-01-01", tz = "UTC")]
  
  df <- data.frame(
    case_id = x$case_id,
    activity = x$activity,
    timestamp = format(x$safe_time, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  
  result <- tryCatch({
    py$run_inductive_from_dataframe(df)
  }, error = function(e) {
    message("Inductive Miner failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(result)) return(NULL)
  
  list(
    fitness = as.numeric(result$fitness),
    precision = as.numeric(result$precision),
    runtime = as.numeric(result$runtime),
    activities = as.integer(result$activities),
    edges = as.integer(result$edges)
  )
}

evaluate_inductive_miner <- function(result, dataset_name) {
  
  if (is.null(result)) return(NULL)
  
  fitness <- result$fitness
  precision <- result$precision
  
  fscore <- ifelse(
    is.na(fitness) | is.na(precision) | (fitness + precision == 0),
    NA,
    2 * fitness * precision / (fitness + precision)
  )
  
  data.table(
    Dataset = dataset_name,
    Method = "Inductive Miner",
    Fitness = round(fitness, 4),
    Precision = round(precision, 4),
    F_score = round(fscore, 4),
    Edge_Jaccard = NA_real_,
    Trace_Coverage = NA_real_,
    Activities = result$activities,
    Edges = result$edges,
    Runtime_sec = round(result$runtime, 4)
  )
}

###############################################################
# 7. METRICS
###############################################################

edge_vector <- function(edge_df) {
  unique(paste(edge_df$From, edge_df$To, sep = "->"))
}

observed_edge_vector <- function(traces) {
  unique(unlist(lapply(traces$trace, function(t) {
    if (length(t) < 2) return(NULL)
    paste(head(t, -1), tail(t, -1), sep = "->")
  })))
}

compute_fitness <- function(traces, model_edges) {
  
  total <- 0
  valid <- 0
  
  for (t in traces$trace) {
    if (length(t) < 2) next
    
    trace_edges <- paste(head(t, -1), tail(t, -1), sep = "->")
    total <- total + length(trace_edges)
    valid <- valid + sum(trace_edges %in% model_edges)
  }
  
  if (total == 0) return(0)
  valid / total
}

compute_precision <- function(traces, model_edges) {
  
  obs_edges <- observed_edge_vector(traces)
  
  if (length(model_edges) == 0) return(0)
  length(intersect(model_edges, obs_edges)) / length(model_edges)
}

compute_edge_jaccard <- function(traces, model_edges) {
  
  obs_edges <- observed_edge_vector(traces)
  u <- union(obs_edges, model_edges)
  
  if (length(u) == 0) return(0)
  length(intersect(obs_edges, model_edges)) / length(u)
}

compute_trace_coverage <- function(traces, model_edges) {
  
  valid_traces <- 0
  total_traces <- 0
  
  for (t in traces$trace) {
    if (length(t) < 2) next
    
    total_traces <- total_traces + 1
    trace_edges <- paste(head(t, -1), tail(t, -1), sep = "->")
    
    if (all(trace_edges %in% model_edges)) {
      valid_traces <- valid_traces + 1
    }
  }
  
  if (total_traces == 0) return(0)
  valid_traces / total_traces
}

compute_fscore <- function(fitness, precision) {
  if ((fitness + precision) == 0) return(0)
  2 * fitness * precision / (fitness + precision)
}

###############################################################
# 8. MODEL EVALUATION
###############################################################

evaluate_model <- function(log, model_edges_df, method, dataset, runtime_sec) {
  
  traces <- get_traces(log)
  model_edges <- edge_vector(model_edges_df)
  
  fitness <- compute_fitness(traces, model_edges)
  precision <- compute_precision(traces, model_edges)
  fscore <- compute_fscore(fitness, precision)
  jaccard <- compute_edge_jaccard(traces, model_edges)
  coverage <- compute_trace_coverage(traces, model_edges)
  
  data.table(
    Dataset = dataset,
    Method = method,
    Fitness = round(fitness, 4),
    Precision = round(precision, 4),
    F_score = round(fscore, 4),
    Edge_Jaccard = round(jaccard, 4),
    Trace_Coverage = round(coverage, 4),
    Activities = length(unique(c(model_edges_df$From, model_edges_df$To))),
    Edges = nrow(model_edges_df),
    Runtime_sec = round(runtime_sec, 4)
  )
}

###############################################################
# 9. RUN BENCHMARK
###############################################################

run_benchmark <- function(log, dataset_name) {
  
  results <- list()
  
  start <- Sys.time()
  weftsim_edges <- discover_WEFTSIM_edges(log)
  runtime <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  results[[length(results) + 1]] <- evaluate_model(log, weftsim_edges, "WEFTSIM", dataset_name, runtime)
  
  start <- Sys.time()
  heur_edges <- discover_heuristics_miner_like(log)
  runtime <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  results[[length(results) + 1]] <- evaluate_model(log, heur_edges, "Heuristics Miner-like", dataset_name, runtime)
  
  start <- Sys.time()
  freq_edges <- discover_frequency_DFG(log)
  runtime <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  results[[length(results) + 1]] <- evaluate_model(log, freq_edges, "Frequency DFG", dataset_name, runtime)
  
  start <- Sys.time()
  alpha_edges <- discover_alpha_miner_like(log)
  runtime <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  results[[length(results) + 1]] <- evaluate_model(log, alpha_edges, "Alpha Miner-like", dataset_name, runtime)
  
  if (pm4py_available) {
    message(">>> Running Inductive Miner <<<")
    im_result <- run_inductive_miner(log)
    
    if (!is.null(im_result)) {
      results[[length(results) + 1]] <- evaluate_inductive_miner(im_result, dataset_name)
    }
  }
  
  rbindlist(results, fill = TRUE)
}

###############################################################
# 10. RUN ALL DATASETS
###############################################################

all_results <- list()

log1 <- prepare_log("C:/Users/snamakia/OneDrive/Research-I6545/Sina/Research/Publication Incubators/Data-driven simulaiton/Development/1. Clean Folder/BPI dataset evaluation/BPI 2012 evaluation/Data/BPI2012_event_log.csv")
all_results[[1]] <- run_benchmark(log1, "BPI 2012")

log2 <- prepare_log("C:/Users/snamakia/OneDrive/Research-I6545/Sina/Research/Publication Incubators/Data-driven simulaiton/Development/1. Clean Folder/BPI dataset evaluation/datasets BPI/BPI2015_event_log.csv")
all_results[[2]] <- run_benchmark(log2, "BPI 2015")

log3 <- prepare_log("C:/Users/snamakia/OneDrive/Research-I6545/Sina/Research/Publication Incubators/Data-driven simulaiton/Development/1. Clean Folder/BPI dataset evaluation/datasets BPI/BPI Challenge 2017.csv")
all_results[[3]] <- run_benchmark(log3, "BPI 2017")

log4 <- prepare_log("C:/Users/snamakia/OneDrive/Research-I6545/Sina/Research/Publication Incubators/Data-driven simulaiton/Development/1. Clean Folder/BPI dataset evaluation/datasets BPI/BPI2019_event_log.csv")
all_results[[4]] <- run_benchmark(log4, "BPI 2019")

final_results <- rbindlist(all_results, fill = TRUE)

print(final_results)

write.csv(final_results, "benchmark_results_WEFTSIM_vs_baselines.csv", row.names = FALSE)
