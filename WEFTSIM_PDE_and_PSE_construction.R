# Stable Heuristic Miner -> Trace-driven Sim (simmer)
# Improved, robust, generic version that preserves your original logic
# Author: Sina NAMAKI ARAGHI

# ---------------------------
# Libraries
# ---------------------------
install.packages(c("bupaR", "eventdataR", "edeaR", "petrinetR", "processmapR", "processcheckR", "xesreadR"))
library(bupaR)
library(processcheckR)
library(processmapR)
library(petrinetR)
library(edeaR)
library(tidyverse)
library(lubridate)
required_pkgs <- c(
  "DiagrammeR","dplyr","tidyr","data.table","simmer","simmer.plot",
  "lubridate","ggplot2","stringr","gridExtra","fitdistrplus","purrr"
)
installed <- rownames(installed.packages())
for (p in required_pkgs) if (!p %in% installed) install.packages(p, repos = "https://cloud.r-project.org")
invisible(lapply(required_pkgs, library, character.only = TRUE))

# define small helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------
# 0) Load and prepare event log
# ---------------------------
# Choose a file or provide a path
fn <- file.choose()
EventLog <- read.csv2(fn, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))

DF_EventLog <- data.frame(EventLog, stringsAsFactors = FALSE)
names(DF_EventLog) <- make.names(names(DF_EventLog))  # ensure safe column names

# Expected columns by convention: Case.ID, Activity, Resource, Start.Timestamp, Complete.Timestamp, Span
# We'll detect presence and adapt.
if (!"Case.ID" %in% names(DF_EventLog)) {
  stop("The input event log must contain a column named like 'Case.ID' (case identifier).")
}
if (!"Activity" %in% names(DF_EventLog)) {
  # try to recover common variants
  alt.act <- grep("activity|task|event", names(DF_EventLog), value = TRUE, ignore.case = TRUE)
  if (length(alt.act) >= 1) names(DF_EventLog)[names(DF_EventLog) == alt.act[1]] <- "Activity"
  else stop("No Activity column found.")
}
if (!"Resource" %in% names(DF_EventLog)) {
  # add Resource column if missing
  DF_EventLog$Resource <- NA_character_
}

# ---------------------------
# helpers: parse durations/times robustly
# ---------------------------
parse_span_to_minutes <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  empty <- is.na(x) | x == ""
  # mm:ss and hh:mm:ss etc using lubridate
  suppressWarnings({
    # try mm:ss or mm:ss.s
    ms_parsed <- lubridate::ms(x[!empty])
    valid_idx <- !is.na(ms_parsed)
    out[!empty][valid_idx] <- as.numeric(ms_parsed[valid_idx], units = "mins")
  })
  suppressWarnings({
    # try hh:mm or hh:mm:ss (hm will parse hh:mm and hms will parse hh:mm:ss)
    hm_parsed <- lubridate::hm(x[!empty])
    valid_idx <- !is.na(hm_parsed)
    out[!empty][valid_idx] <- as.numeric(hm_parsed[valid_idx], units = "mins")
  })
  # if still NA try numeric (minutes)
  numeric_idx <- !is.na(as.numeric(suppressWarnings(as.numeric(x))))
  out[numeric_idx] <- as.numeric(x[numeric_idx])
  out
}

parse_hms_safe <- function(x) {
  # try to parse times or datetimes to POSIXct; allow HH:MM:SS or full datetime
  out <- rep(NA, length(x))
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi) || xi == "") next
    # try many formats
    parsed <- NA
    parsed <- suppressWarnings(tryCatch(hms::as_hms(xi), error = function(e) NA))
    if (!is.na(parsed)) {
      # convert to POSIXct today + time (used for differences)
      out[i] <- as.POSIXct(sprintf("%s %s", Sys.Date(), as.character(parsed)), tz = "")
      next
    }
    parsed2 <- suppressWarnings(tryCatch(lubridate::ymd_hms(xi), error = function(e) NA))
    if (!is.na(parsed2)) { out[i] <- parsed2; next }
    parsed3 <- suppressWarnings(tryCatch(lubridate::ymd_hm(xi), error = function(e) NA))
    if (!is.na(parsed3)) { out[i] <- parsed3; next }
    parsed4 <- suppressWarnings(tryCatch(lubridate::ymd(xi), error = function(e) NA))
    if (!is.na(parsed4)) { out[i] <- parsed4; next }
    # finally try parse_date_time with flexible orders
    parsed5 <- suppressWarnings(tryCatch(lubridate::parse_date_time(xi, orders = c("HMS","HM","Ymd HMS","Ymd HM","Ymd")), error = function(e) NA))
    if (!is.na(parsed5)) { out[i] <- parsed5; next }
    out[i] <- NA
  }
  as.POSIXct(out, origin = "1970-01-01")
}

# create parsed columns
DF_EventLog <- DF_EventLog %>%
  mutate(
    Span_min = parse_span_to_minutes(Span),
    Start_hms = parse_hms_safe(Start.Timestamp),
    End_hms = parse_hms_safe(Complete.Timestamp),
    Dur_min_ts = as.numeric(difftime(End_hms, Start_hms, units = "mins"))
  )

# Order events inside case preserving original order if possible
DF_EventLog <- DF_EventLog %>%
  group_by(Case.ID) %>%
  mutate(.row_order = row_number()) %>%
  arrange(Case.ID, .row_order) %>%
  ungroup()

# Prefer timestamp duration when available
DF_EventLog <- DF_EventLog %>%
  mutate(Activity_Dur_min = coalesce(Dur_min_ts, Span_min))

# sanitize Activity and Resource types
DF_EventLog$Activity <- as.character(DF_EventLog$Activity)
DF_EventLog$Resource <- as.character(DF_EventLog$Resource)

# ---------------------------
# 1) Build direct-follows and dependency matrix (your miner)
# ---------------------------
dt <- data.table(
  ID = DF_EventLog$Case.ID,
  Activity = DF_EventLog$Activity,
  Resource = DF_EventLog$Resource
)

shift <- data.table::shift
consecutive_id <- dt[, .(first.act = Activity, second.act = shift(Activity, type = "lead")), by = ID][!is.na(second.act)]
Activity_Direct_Relation <- consecutive_id[, .N, by = .(first.act, second.act)]
setnames(Activity_Direct_Relation, c("first.act","second.act","N"))

All_Activities <- unique(dt$Activity)
n_activities <- length(All_Activities)
new_classes <- as.character(All_Activities)

# Build dependency matrix (square)
DependencyMatrix <- data.table(matrix(0, nrow = n_activities, ncol = n_activities))
setnames(DependencyMatrix, new_classes)
DependencyMatrix[, new_classes := new_classes]
setkey(DependencyMatrix, new_classes)

# populate counts
for (i in seq_len(nrow(Activity_Direct_Relation))) {
  f <- as.character(Activity_Direct_Relation$first.act[i])
  s <- as.character(Activity_Direct_Relation$second.act[i])
  DependencyMatrix[f, (s) := Activity_Direct_Relation$N[i]]
}

# ---------------------------
# 2) Stability analysis on edges (your logic)
# ---------------------------
df2 <- DependencyMatrix %>%
  gather(col, val, -new_classes) %>%
  arrange(new_classes, desc(val)) %>%
  filter(val != 0) %>%
  mutate(MovingRange = abs(val - lag(val, default = first(val))))

# avoid NA if only single row
if (nrow(df2) == 0) stop("No direct-follow relations found in the log.")

MR_average <- mean(df2$MovingRange, na.rm = TRUE)
x_average <- mean(df2$val, na.rm = TRUE)
# Use same formula you had
UCL_indiv <- x_average + 3 * (MR_average / 2.534)
LCL_indiv <- x_average - 3 * (MR_average / 2.534)

ProcessActivities_EdgeStability <- subset(df2, df2$val > ceiling(LCL_indiv) & df2$val < ceiling(UCL_indiv))
colnames(ProcessActivities_EdgeStability) <- c("From", "To", "val", "MovingRange")
Hot_ProcessActivities_EdgeStability <- subset(df2, df2$val > ceiling(UCL_indiv))
colnames(Hot_ProcessActivities_EdgeStability) <- c("From", "To", "val", "MovingRange")

AllRelations_edgeStability <- bind_rows(ProcessActivities_EdgeStability, Hot_ProcessActivities_EdgeStability)
AllRelations_edgeStability[is.na(AllRelations_edgeStability)] <- 0
stable_edge_set <- AllRelations_edgeStability %>%
  dplyr::select(From, To) %>%
  distinct()

# ---------------------------
# 3) Stability analysis on activities (your logic)
# ---------------------------
m_number_of_samples <- length(DependencyMatrix$new_classes)
n_number_of_observations <- length(df2$val)
n_bar <- ifelse(m_number_of_samples > 0, ceiling(n_number_of_observations / m_number_of_samples), 1)

# Convert dependency to numeric DF
df_numeric_of_dependency <- data.frame(DependencyMatrix, header = TRUE, stringsAsFactors = FALSE)
# remove the appended "new_classes" column to get numeric matrix of counts
depMatrix_without_colnames <- subset(DependencyMatrix, select = DependencyMatrix$new_classes)
# Replace "0" strings if any with NA (your code tried this)
depMatrix_without_colnames[depMatrix_without_colnames == "0"] <- NA
# compute size (outgoing count) and mean of non-zero counts per activity (row)
Average_of_dependency_values <- data.frame(
  new_classes = df_numeric_of_dependency[, "new_classes"],
  size = rowSums(df_numeric_of_dependency[, names(df_numeric_of_dependency) != "new_classes"] > 0, na.rm = TRUE) - 1,
  mean = rowMeans(depMatrix_without_colnames, na.rm = TRUE),
  stringsAsFactors = FALSE
)
Average_of_dependency_values$mean[is.na(Average_of_dependency_values$mean)] <- 0

# replace zeros with NA in numeric df for sd calc
df_numeric_of_dependency[df_numeric_of_dependency == 0] <- NA
standard_deviation_of_Activities <- apply(df_numeric_of_dependency[, names(df_numeric_of_dependency) != "new_classes"], 1, sd, na.rm = TRUE)
standard_deviation_of_Activities[is.na(standard_deviation_of_Activities)] <- 0
matrix_of_standard_deviations <- as.matrix(standard_deviation_of_Activities)
Powered_standard_deviations <- as.double(matrix_of_standard_deviations * matrix_of_standard_deviations)

n_i_x_i <- Average_of_dependency_values$size * Average_of_dependency_values$mean
Average_of_dependency_values[["n_i.x_i"]] <- n_i_x_i
Average_of_dependency_values[["powered_std"]] <- Powered_standard_deviations

n_i_1_powerestd <- (Average_of_dependency_values$size - 1) * Average_of_dependency_values$powered_std
Average_of_dependency_values[["n_i_substract_1_powerestd"]] <- n_i_1_powerestd

Grand_Average <- ifelse(sum(Average_of_dependency_values$size, na.rm = TRUE) > 0,
                        sum(Average_of_dependency_values$n_i.x_i, na.rm = TRUE) / sum(Average_of_dependency_values$size, na.rm = TRUE),
                        0)
avg_size <- mean(Average_of_dependency_values$size[Average_of_dependency_values$size > 0], na.rm = TRUE)
if (is.na(avg_size) || avg_size <= 0) avg_size <- 1
C4n_population <- ((4 * (avg_size) - 1) / (4 * (avg_size) - 3))
A3n <- 3 / (C4n_population * sqrt(n_bar))
sigma_hat <- mean(matrix_of_standard_deviations, na.rm = TRUE)
UCL <- ceiling(Grand_Average + (A3n * C4n_population * sigma_hat))
LCL <- ceiling(Grand_Average - (A3n * C4n_population * sigma_hat))

instable_activities <- Average_of_dependency_values %>% mutate(Below_LCL = mean < LCL)
hot_zones <- instable_activities %>% mutate(Hot_Zones = mean > UCL)
stable_Activities <- hot_zones %>% mutate(Stable_zones = LCL <= mean & mean <= UCL)

Stable_activities <- data.table(stable_Activities$new_classes, stable_Activities$Stable_zones)
Hot_activities <- data.table(stable_Activities$new_classes, stable_Activities$Hot_Zones)

All_Activities_stable <- union(
  Stable_activities$V1[Stable_activities$V2 == TRUE],
  Hot_activities$V1[Hot_activities$V2 == TRUE]
)

Direct_relation_stable <- dplyr::filter(Activity_Direct_Relation, Activity_Direct_Relation$first.act %in% All_Activities_stable)
Final_rel <- dplyr::filter(Direct_relation_stable, Direct_relation_stable$second.act %in% All_Activities_stable)

# ---------------------------
# 4) Extract frequent traces and filter by stable edges
# ---------------------------
historic_traces <- DF_EventLog %>%
  group_by(Case.ID) %>%
  arrange(.row_order) %>%
  summarise(trace = str_c(Activity, collapse = " -> "), .groups = "drop")

trace_freq <- historic_traces %>% count(trace, sort = TRUE) %>% rename(freq = n)

trace_transitions <- function(trace_str) {
  acts <- str_split(trace_str, " -> ")[[1]]
  if (length(acts) < 2) return(data.frame(From = character(), To = character(), stringsAsFactors = FALSE))
  data.frame(From = head(acts, -1), To = tail(acts, -1), stringsAsFactors = FALSE)
}

stable_edge_keys <- stable_edge_set %>% mutate(key = paste(From, To, sep = "||")) %>% pull(key)

is_trace_stable <- function(trace_str, keys = stable_edge_keys) {
  tr_df <- trace_transitions(trace_str)
  if (nrow(tr_df) == 0) return(FALSE)
  keys_tr <- paste(tr_df$From, tr_df$To, sep = "||")
  all(keys_tr %in% keys)
}

valid_traces <- trace_freq %>%
  mutate(is_stable = vapply(trace, is_trace_stable, logical(1))) %>%
  filter(is_stable)

if (nrow(valid_traces) == 0) {
  # fallback to activity-stable relations (Final_rel)
  fallback_edges <- Final_rel %>%
    select(first.act, second.act) %>%
    rename(From = first.act, To = second.act) %>%
    distinct() %>%
    mutate(key = paste(From, To, sep = "||")) %>%
    pull(key)
  is_trace_fallback <- function(trace_str) {
    tr_df <- trace_transitions(trace_str)
    if (nrow(tr_df) == 0) return(FALSE)
    keys_tr <- paste(tr_df$From, tr_df$To, sep = "||")
    all(keys_tr %in% fallback_edges)
  }
  valid_traces <- trace_freq %>%
    mutate(is_stable = vapply(trace, is_trace_fallback, logical(1))) %>%
    filter(is_stable)
}

K <- min(5, nrow(valid_traces))
top_traces <- if (nrow(valid_traces) > 0) valid_traces %>% slice(1:K) else trace_freq %>% slice(1:min(5,n()))

# ---------------------------
# 5) Fit distributions per activity and per transition
# ---------------------------
activity_samples <- DF_EventLog %>%
  dplyr::select(Activity, Activity_Dur_min) %>%
  filter(!is.na(Activity_Dur_min), Activity_Dur_min > 0)

# transitions: compute delay between end current and start next (in minutes)
transition_samples <- DF_EventLog %>%
  group_by(Case.ID) %>%
  arrange(.row_order) %>%
  mutate(next_activity = lead(Activity),
         next_start_hms = lead(Start_hms),
         trans_delay_min = as.numeric(difftime(next_start_hms, End_hms, units = "mins"))) %>%
  ungroup() %>%
  filter(!is.na(next_activity), !is.na(trans_delay_min)) %>%
  dplyr::select(From = Activity, To = next_activity, Delay_min = trans_delay_min) %>%
  filter(Delay_min >= 0)  # ignore negatives due to noisy timestamps

# helper: choose between lognormal and gamma by AIC and return a sampler function
fit_duration <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 5) {
    # fallback: empirical lognormal from moments
    m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
    if (!is.finite(m) || m <= 0) m <- 1
    if (!is.finite(s) || s <= 0) s <- max(0.1 * m, 0.1)
    meanlog <- log(m^2 / sqrt(s^2 + m^2))
    sdlog <- sqrt(log(1 + (s^2 / m^2)))
    return(function() rlnorm(1, meanlog = meanlog, sdlog = sdlog))
  }
  ln_fit <- tryCatch(fitdist(x, "lnorm"), error = function(e) NULL)
  gm_fit <- tryCatch(fitdist(x, "gamma"), error = function(e) NULL)
  best <- NULL
  if (!is.null(ln_fit) && !is.null(gm_fit)) {
    best <- if (ln_fit$aic <= gm_fit$aic) ln_fit else gm_fit
  } else {
    best <- ln_fit %||% gm_fit
  }
  if (is.null(best)) {
    m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
    if (!is.finite(m) || m <= 0) m <- 1
    if (!is.finite(s) || s <= 0) s <- max(0.1 * m, 0.1)
    meanlog <- log(m^2 / sqrt(s^2 + m^2))
    sdlog <- sqrt(log(1 + (s^2 / m^2)))
    return(function() rlnorm(1, meanlog = meanlog, sdlog = sdlog))
  }
  if (best$distname == "lnorm") {
    ml <- best$estimate[["meanlog"]]; sl <- best$estimate[["sdlog"]]
    return(function() rlnorm(1, meanlog = ml, sdlog = sl))
  } else if (best$distname == "gamma") {
    sh <- best$estimate[["shape"]]; rt <- best$estimate[["rate"]]
    return(function() rgamma(1, shape = sh, rate = rt))
  } else {
    m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
    meanlog <- log(m^2 / sqrt(s^2 + m^2))
    sdlog <- sqrt(log(1 + (s^2 / m^2)))
    return(function() rlnorm(1, meanlog = meanlog, sdlog = sdlog))
  }
}

# Build samplers
activity_samplers <- activity_samples %>%
  group_by(Activity) %>%
  summarise(sampler = list(fit_duration(Activity_Dur_min)), .groups = "drop")

transition_samplers <- transition_samples %>%
  group_by(From, To) %>%
  summarise(sampler = list(fit_duration(Delay_min)), .groups = "drop")

get_activity_sampler <- function(a) {
  row <- activity_samplers[activity_samplers$Activity == a, ]
  if (nrow(row) == 0) return(function() rlnorm(1, meanlog = log(3), sdlog = 0.25))
  row$sampler[[1]]
}
get_transition_sampler <- function(f, t) {
  row <- transition_samplers %>% filter(From == f, To == t)
  if (nrow(row) == 0) return(function() 0)
  row$sampler[[1]]
}

# resource mapping (Activity -> Resource)
activity_resource_map <- DF_EventLog %>%
  distinct(Activity, Resource) %>%
  arrange(Activity)

# ---------------------------
# 6) Build trace-driven trajectories with service and handover delays (simmer)
# ---------------------------
env <- simmer::simmer("Stable Trace Simulation")

unique_resources <- unique(na.omit(DF_EventLog$Resource))
default_capacity <- 2
# add resources (if Resource is NA for some activities we skip)
for (res in unique_resources) {
  # make resource name safe
  res_name <- as.character(res)
  env <- env %>% add_resource(res_name, capacity = default_capacity)
}

# construct trajectories for top stable traces
trajectories <- list()
for (i in seq_len(nrow(top_traces))) {
  t_str <- top_traces$trace[i]
  acts <- str_split(t_str, " -> ")[[1]]
  tr <- trajectory(paste0("Trace_", i))
  for (j in seq_along(acts)) {
    a <- acts[j]
    res <- activity_resource_map %>% filter(Activity == a) %>% pull(Resource)
    act_samp <- get_activity_sampler(a)
    # if resource exists and not NA, seize, timeout via sampler, release
    if (length(res) > 0 && !is.na(res) && res %in% unique_resources) {
      tr <- tr %>%
        seize(as.character(res)) %>%
        timeout(function() { as.numeric(act_samp()) }) %>%
        release(as.character(res))
    } else {
      # no resource known -> just execute activity duration
      tr <- tr %>% timeout(function() { as.numeric(act_samp()) })
    }
    # handover delay to next activity
    if (j < length(acts)) {
      next_a <- acts[j+1]
      trans_samp <- get_transition_sampler(a, next_a)
      tr <- tr %>% timeout(function() { as.numeric(trans_samp()) })
    }
  }
  trajectories[[i]] <- tr
}

# Add weighted generators according to historic trace frequency
total_freq <- sum(top_traces$freq)
base_rate <- 0.05  # cases per minute baseline
if (total_freq == 0) total_freq <- 1

for (i in seq_len(nrow(top_traces))) {
  weight <- top_traces$freq[i] / total_freq
  env <- env %>%
    add_generator(
      name_prefix = paste0("case_T", i, "_"),
      trajectory = trajectories[[i]],
      distribution = function() rexp(1, rate = base_rate * weight)
    )
}

# ---------------------------
# 7) Run simulation and reconstruct simulated event log
# ---------------------------
sim_time <- 10000 # minutes; customizable
env %>% run(until = sim_time)

simmer_arr <- get_mon_arrivals(env, per_resource = TRUE)

# Map resource -> activity via event log (in case multiple activities share resource names)
simulation_activities_map <- if (nrow(activity_resource_map) > 0) {
  setNames(activity_resource_map$Activity, activity_resource_map$Resource)
} else character(0)

start_time <- lubridate::now()

# reconstruct event log: resource names may repeat for different activities;
# we attempt to map resource -> activity using the mapping; if ambiguous we keep NA
simulation_eventlog <- data.frame(
  case_id = simmer_arr$name,
  resource = simmer_arr$resource,
  timestamp_start = as_datetime(start_time + duration(simmer_arr$start_time, units = "minutes")),
  timestamp_end   = as_datetime(start_time + duration(simmer_arr$end_time,   units = "minutes")),
  duration_min = simmer_arr$activity_time,
  stringsAsFactors = FALSE
)


# Map resource -> activity where possible, else keep NA
simulation_eventlog$activity <- simulation_activities_map[as.character(simulation_eventlog$resource)]
# If mapping produced NA, attempt to infer using generator prefix (Trace_i) -> top_traces list
na_idx <- which(is.na(simulation_eventlog$activity))
if (length(na_idx) > 0) {
  # fall back: try to parse case_id prefix to find which trace it belongs to and assign sequential activities
  # (best-effort; conservative)
  simulation_eventlog$activity[na_idx] <- NA_character_
}

# remove rows without activity (if any)
simulation_eventlog <- simulation_eventlog %>% filter(!is.na(activity))

# ---------------------------
# 8) Rebuild simulated traces and validation
# ---------------------------
sim_traces <- simulation_eventlog %>%
  group_by(case_id) %>%
  arrange(timestamp_start) %>%
  summarise(
    trace = str_c(activity, collapse = " -> "),
    case_duration_min = sum(duration_min, na.rm = TRUE),
    .groups = "drop"
  )

sim_trace_freq <- sim_traces %>% count(trace, sort = TRUE) %>% rename(freq_sim = n)

compare_traces <- trace_freq %>% left_join(sim_trace_freq, by = "trace") %>% mutate(freq_sim = replace_na(freq_sim, 0))

# Historic case durations
historic_case_durations <- DF_EventLog %>%
  group_by(Case.ID) %>%
  summarise(case_duration_min = sum(Activity_Dur_min, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(case_duration_min))

# Plots: historic vs simulated trace frequencies (top 10) & durations
p1 <- compare_traces %>%
  slice(1:min(10, n())) %>%
  tidyr::gather(origin, freq, freq, freq_sim) %>%
  ggplot(aes(x = reorder(trace, freq), y = freq, fill = origin)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Historic vs Simulated Trace Frequencies (Top 10)", x = "Trace", y = "Count") +
  theme_minimal()

p2 <- bind_rows(
  historic_case_durations %>% mutate(origin = "historic"),
  sim_traces %>% mutate(origin = "simulated") %>% dplyr::select(case_duration_min, origin)
) %>%
  ggplot(aes(x = origin, y = case_duration_min, fill = origin)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Case Duration Distribution: Historic vs Simulated", x = "", y = "Duration (min)") +
  theme_minimal()

gridExtra::grid.arrange(p1, p2, nrow = 2)

# Resource diagnostics (if resources exist)
if (length(unique_resources) > 0) {
  try(plot(get_mon_resources(env), metric = "usage") + ggtitle("Resource Usage Over Time"))
  try(plot(get_mon_resources(env), metric = "utilization") + ggtitle("Resource Utilization Over Time"))
}

# Save outputs
write.csv(simulation_eventlog, "simulation_results_traceAnalyses.csv", row.names = FALSE)

# ---------------------------
# 9) Coverage metrics (what you asked: "at which percentage activities in simulation model are covered by historic log within traces")
# ---------------------------
# Historic direct-follow set
historic_dfg <- Activity_Direct_Relation %>% transmute(From = first.act, To = second.act, N = N)

# Simulated transitions
sim_transitions <- simulation_eventlog %>%
  group_by(case_id) %>%
  arrange(timestamp_start) %>%
  mutate(next_activity = lead(activity)) %>%
  ungroup() %>%
  filter(!is.na(next_activity)) %>%
  count(activity, next_activity, name = "N_sim") %>%
  transmute(From = activity, To = next_activity, N_sim = N_sim)

# Activity-level coverage: for each activity in simulation, proportion of its simulated outgoing transitions that exist in historic log (weighted)
sim_agg <- sim_transitions %>%
  left_join(historic_dfg %>% transmute(From, To, historic = 1), by = c("From", "To")) %>%
  mutate(historic = ifelse(is.na(historic), 0, historic))

activity_coverage <- sim_agg %>%
  group_by(From) %>%
  summarise(
    total_sim_transitions = sum(N_sim, na.rm = TRUE),
    covered_sim_transition_occurrences = sum(N_sim * historic, na.rm = TRUE),
    coverage_pct = ifelse(total_sim_transitions > 0, 100 * covered_sim_transition_occurrences / total_sim_transitions, NA_real_),
    .groups = "drop"
  )

# Overall (weighted) activity transition coverage
overall_weighted_transition_coverage <- mean(activity_coverage$coverage_pct, na.rm = TRUE)

# Trace-level coverage
sim_trace_set <- unique(sim_traces$trace)
hist_trace_set <- unique(trace_freq$trace)
sim_traces_matching_hist_pct <- ifelse(length(sim_traces$trace) > 0, 100 * sum(sim_traces$trace %in% hist_trace_set) / nrow(sim_traces), NA_real_)
historic_traces_covered_by_sim_pct <- ifelse(length(hist_trace_set) > 0, 100 * sum(hist_trace_set %in% sim_trace_set) / length(hist_trace_set), NA_real_)


# Print coverage summary
cat("\n--- Coverage Summary ---\n")
cat(sprintf("Simulated case events: %d\n", nrow(simulation_eventlog)))
cat(sprintf("Number of unique simulated traces: %d\n", length(sim_traces$trace)))
cat(sprintf("Historic traces covered by simulation: %.2f%%\n", historic_traces_covered_by_sim_pct))
cat(sprintf("Average activity-level (weighted) transition coverage: %.2f%%\n", overall_weighted_transition_coverage))
cat("\nActivity-level coverage (per activity):\n")
print(activity_coverage %>% arrange(desc(coverage_pct)))

# Save the coverage table
write.csv(activity_coverage, "activity_transition_coverage.csv", row.names = FALSE)

# ---------------------------
# End: results are:
# - simulation_results_traceAnalyses.csv  (simulated event log)
# - activity_transition_coverage.csv     (coverage per activity)
# - plus plots on screen
# ---------------------------
cat("\nOutputs saved:\n- simulation_results_traceAnalyses.csv\n- activity_transition_coverage.csv\n\n")



#---- Evaluating conformance checking


