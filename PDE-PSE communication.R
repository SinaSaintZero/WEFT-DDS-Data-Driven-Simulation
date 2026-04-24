# ===== FINAL WORKING VERSION (UPDATED) 
# Stable Heuristic Miner v2 – Fully Robust + Hot edges colored red
# Hot edges = red, Stable edges = black, BOTH thick (hot thicker)
# Handles empty edges, sparse logs, and DiagrammeR constraints
#==> Also, there is the added PSE construction
# Author: Sina Namaki Araghi (updated)

# -------------------- Libraries --------------------
library(DiagrammeR)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)



# -------------------- Load Event Log --------------------
EventLog <- read.csv(file.choose(), header = TRUE)

DF_EventLog <- data.frame(EventLog)

# Expect: CaseID | Activity | Resource
setDT(DF_EventLog)
dt <- data.table(
  ID = DF_EventLog[[1]],
  Activity = as.character(DF_EventLog[[2]]), # we change this based on column names
  Resource = if (ncol(DF_EventLog) >= 3) DF_EventLog[[3]] else NA # we change the column number based on th ename
)

set.seed(42)

all_ids <- unique(dt$ID)
train_ids <- sample(all_ids, size = 0.7 * length(all_ids))

train_log <- dt[ID %in% train_ids]
test_log  <- dt[!ID %in% train_ids]

# -------------------- Directly-Follows Relations --------------------
shift <- data.table::shift
consecutive_id <- train_log[, .(
  From = Activity,
  To = shift(Activity, type = "lead")
), by = ID][!is.na(To)]

Activity_Direct_Relation <- consecutive_id[, .N, by = .(From, To)]

# -------------------- Dependency Matrix --------------------
All_Activities <- unique(train_log$Activity)
n <- length(All_Activities)

DependencyMatrix <- matrix(
  0, nrow = n, ncol = n,
  dimnames = list(All_Activities, All_Activities)
)

for (i in seq_len(nrow(Activity_Direct_Relation))) {
  DependencyMatrix[
    Activity_Direct_Relation$From[i],
    Activity_Direct_Relation$To[i]
  ] <- Activity_Direct_Relation$N[i]
}

# -------------------- Edge Stability (MR Chart – safe) --------------------
df2 <- as.data.frame(DependencyMatrix) %>%
  tibble::rownames_to_column("From") %>%
  pivot_longer(-From, names_to = "To", values_to = "val") %>%
  filter(val > 0) %>%
  arrange(From, desc(val)) %>%
  group_by(From) %>%
  mutate(MovingRange = abs(val - lag(val, default = first(val)))) %>%
  ungroup()

if (nrow(df2) == 0) stop("No directly-follows relations found in the event log.")

MR_average <- mean(df2$MovingRange, na.rm = TRUE)
x_average  <- mean(df2$val, na.rm = TRUE)

UCL_indiv <- x_average + 3 * (MR_average / 2.534)
LCL_indiv <- max(0, x_average - 3 * (MR_average / 2.534))

StableEdges <- df2 %>% filter(val >= LCL_indiv, val <= UCL_indiv)
HotEdges    <- df2 %>% filter(val > UCL_indiv)

# -------------------- Activity Stability --------------------
row_means <- rowMeans(DependencyMatrix, na.rm = TRUE)
row_sd    <- apply(DependencyMatrix, 1, sd, na.rm = TRUE)

Grand_Average <- mean(row_means, na.rm = TRUE)
sigma_hat <- sd(row_means, na.rm = TRUE)

UCL_act <- Grand_Average + 3 * sigma_hat
LCL_act <- max(0, Grand_Average - 3 * sigma_hat)

ActivityStability <- data.frame(
  Activity = All_Activities,
  mean = row_means,
  Stable = row_means >= LCL_act & row_means <= UCL_act,
  Hot = row_means > UCL_act
)

Stable_activities <- ActivityStability %>% filter(Stable)
Hot_activities    <- ActivityStability %>% filter(Hot)

Selected_Activities <- union(
  Stable_activities$Activity,
  Hot_activities$Activity
)

# -------------------- Final Relations (SAFE) --------------------
Final_rel <- Activity_Direct_Relation %>%
  filter(From %in% Selected_Activities,
         To   %in% Selected_Activities)

# -------------------- Nodes --------------------
node_stable <- create_node_df(
  n = nrow(Stable_activities),
  type = "Activity",
  label = Stable_activities$Activity,
  style = "filled",
  penwidth = 3,
  color = "black",
  shape = "box",
  fixedsize = FALSE,
  fontsize = 18
)

node_hot <- if (nrow(Hot_activities) > 0) {
  create_node_df(
    n = nrow(Hot_activities),
    type = "Activity",
    label = Hot_activities$Activity,
    style = "filled",
    penwidth = 4,
    color = "red",
    shape = "box",
    fixedsize = FALSE,
    fontsize = 18
  )
} else NULL

nodes_processModel <- combine_ndfs(node_hot, node_stable)

# -------------------- EDGE COLORING + THICK STABLE EDGES --------------------
# Tag Final_rel edges as Hot vs Stable using the HotEdges list
Final_rel <- Final_rel %>%
  dplyr::left_join(
    HotEdges %>% dplyr::select(From, To) %>% dplyr::mutate(edge_type = "hot"),
    by = c("From", "To")
  ) %>%
  dplyr::mutate(
    edge_type  = ifelse(is.na(edge_type), "stable", edge_type),
    edge_color = ifelse(edge_type == "hot", "red", "black"),
    # Keep stable edges thick too; make hot edges even thicker
    edge_width = ifelse(edge_type == "hot", 5, 4)
  )

# -------------------- Edges (DiagrammeR compatible) --------------------
matched_from <- match(Final_rel$From, nodes_processModel$label)
matched_to   <- match(Final_rel$To,   nodes_processModel$label)
valid_edges  <- !is.na(matched_from) & !is.na(matched_to)

if (any(valid_edges)) {
  edges_df <- create_edge_df(
    from     = matched_from[valid_edges],
    to       = matched_to[valid_edges],
    color    = Final_rel$edge_color[valid_edges],
    penwidth = Final_rel$edge_width[valid_edges],
    rel      = Final_rel$edge_type[valid_edges]
  )
} else {
  edges_df <- NULL
  message("No valid edges after filtering – nodes-only model created.")
}

# -------------------- Graph --------------------
ProcessModelStableMiner2 <- create_graph(
  nodes_df = nodes_processModel,
  edges_df = edges_df,
  directed = TRUE
) %>%
  add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
  add_global_graph_attrs(attr = "rankdir", value = "LR", attr_type = "graph") %>%
  add_global_graph_attrs(attr = "overlap", value = "false", attr_type = "graph") %>%
  
  # ---- SPACING ----
add_global_graph_attrs(attr = "nodesep", value = 0.8, attr_type = "graph") %>%
  add_global_graph_attrs(attr = "ranksep", value = 1, attr_type = "graph") %>%
  add_global_graph_attrs(attr = "pad", value = "0.5", attr_type = "graph") %>%
  
  # ---- EDGES ----
add_global_graph_attrs(attr = "splines", value = "curved", attr_type = "graph") %>%
  add_global_graph_attrs(attr = "arrowsize", value = 1.6, attr_type = "edge") %>%
  add_global_graph_attrs(attr = "penwidth", value = 3, attr_type = "edge") %>%
  
  # ---- NODES ----
add_global_graph_attrs(attr = "penwidth", value = 3, attr_type = "node") %>%
  add_global_graph_attrs(attr = "width", value = 1.6, attr_type = "node") %>%
  add_global_graph_attrs(attr = "height", value = 0.8, attr_type = "node") %>%
  add_global_graph_attrs(attr = "fontsize", value = 40, attr_type = "node") %>%
  
  add_global_graph_attrs(attr = "center", value = "true", attr_type = "graph")

render_graph(ProcessModelStableMiner2)




# ===================== END process discovery =====================



#-------- The PSE Simulation engine receives all edges ------
#--- Getting PDE outputs
automatically_discovered_process_workflow <- All_edges %>% group_by(id)%>%
  filter(!is.na(id))
# Example edges_Stable_mining_model data
edges_Stable_mining_model <- data.frame(
  id = automatically_discovered_process_workflow$id,    # Automatically take the id from All_edges
  from = automatically_discovered_process_workflow$from, # From column from All_edges
  to = automatically_discovered_process_workflow$to,     # To column from All_edges
  rel = rep("related", nrow(automatically_discovered_process_workflow)),
  color = rep("black", nrow(automatically_discovered_process_workflow)), # Example: set all to "black"
  label = automatically_discovered_process_workflow$label,
  fontsize = rep(16, nrow(automatically_discovered_process_workflow))
)         

# get resources
activity_resource_mapping <- nodes_processModel %>%
  dplyr::select(id, type, label) %>%
  inner_join(
    dt %>% distinct(Activity, Resources),
    by= c("label" = "Activity")
    
  )

# Creating the trajectory in PSE
# Define a workflow trajectory extracted from the Stable Heuristic Miner
workflow <- trajectory("Workflow")

for (i in 1:nrow(automatically_discovered_process_workflow)) {
  from <- automatically_discovered_process_workflow$from[i]
  to <- automatically_discovered_process_workflow$to[i]
  
  # Get the resources for the activities
  resource_from <- activity_resource_mapping %>% filter(id == from) %>% pull(Resources)
  resource_to <- activity_resource_mapping %>% filter(id == to) %>% pull(Resources)
  
  # Define the sequence for each edge -> mixed distribution
  if (length(resource_from) > 0 & length(resource_to) > 0) {
    workflow <- workflow %>%
      seize(resource_from) %>%
      timeout(function() rnorm(1, mean = 4, sd = 1)) %>%  # Normal for first task
      release(resource_from) %>%
      seize(resource_to) %>%
      timeout(function() rexp(1, rate = 1/3)) %>%        # Exponential for second task
      release(resource_to)
    
  }
}

# Create the simulation environment
env <- simmer("Factory Simulation")

# **User-defined capacities**
# Define the capacities for each machine
machine_capacities <- c(
  "Machine 6 - Turning & Milling" = 2,
  "Machine 8 - Turning & Milling" = 2, # at 3 we decreased the waiting time and arrival time this is the bottleneck based on all changes I made on other machines
  "Quality Check 1" = 2,# at 2 we start to smooth the process
  "Machine 7- Laser Marking" = 2,
  "Machine 11 - Grinding" = 2,
  "Machine 1 - Lapping" = 2,
  "Machine 3 - Round Grinding" = 2,
  "Packing" = 2,
  "Machine 9 - Turning & Milling" = 2,
  "Machine 10 - Grinding" = 2,
  "Machine 4 - Turning & Milling" = 2,
  "Machine 2 - Round Grinding" = 2,
  "Machine 12 - Grinding" = 2,
  "Machine 27 - Grinding" =2,
  "Machine 15 - Turning" = 2
  
)

# Add resources with user-defined capacities
for (resource in names(machine_capacities)) {
  capacity <- machine_capacities[resource]
  env <- env %>% add_resource(resource, capacity = capacity)
}

# Add trajectory and generate arrivals (Poisson process with moderate rate)
arrival_rate <- 0.05  # Average inter-arrival time = 10 minutes
env <- env %>%
  add_generator("product", workflow, function() rexp(1, rate = arrival_rate))

# Run the simulation
env %>% run(until = 1000)

# Prepare results of the simulation
simmer_results <- get_mon_arrivals(env, per_resource = TRUE)

# Convert simulation time into actual time
start_time <- now()
simulation_activities_map <- setNames(activity_resource_mapping$label, activity_resource_mapping$Resources)

simulation_eventlog <- data.frame(
  case_id = simmer_results$name,
  activity = simulation_activities_map[simmer_results$resource],
  resource = simmer_results$resource,
  timestamp_start = as_datetime(start_time + seconds(simmer_results$start_time)),
  timestamp_end = as_datetime(start_time + seconds(simmer_results$end_time))
)


#---> add the duration of tasks and processes
trace_analyses<- simulation_eventlog %>%
  group_by(case_id)%>%
  arrange(timestamp_start)%>%
  summarise(
    trace_process = str_c(activity, collapse = " -> "),
    process_start = min(timestamp_start),
    process_end = max(timestamp_end),
    process_duration = as.numeric(difftime(max(timestamp_end),min(timestamp_start), units = 'hours'))
  )


# Step 2: Identify unique traces and assign types
unique_traces <- trace_analyses %>%
  distinct(trace_process) %>%
  mutate(trace_type = row_number())

trace_analyses <- trace_analyses %>%
  left_join(unique_traces, by = "trace_process")

# Step 3: Calculate average duration for each trace type
average_trace_durations <- trace_analyses %>%
  group_by(trace_type) %>%
  summarise(avg_process_duration = mean(process_duration))

# Step 4: Merge average durations back to the main table
trace_analyses <- trace_analyses %>%
  left_join(average_trace_durations, by = "trace_type")

# Calculate average process duration for each trace type
average_trace_durations <- trace_analyses %>%
  group_by(trace_type) %>%
  summarise(avg_process_duration = mean(process_duration, na.rm = TRUE))

# Add the average durations back to the simulation_eventlog table
trace_analyses <- trace_analyses %>%
  left_join(average_trace_durations, by = "trace_type")

# If you want to add this to unique_traces as well
unique_traces <- unique_traces %>%
  left_join(average_trace_durations, by = "trace_type")

# -- analyzing the duration of each trace by graph
ggplot(trace_analyses, aes(x = process_duration)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Process throughput time", x = "Process throughput (hours)", y = "Frequency") +
  theme_minimal()

avg_durations <- trace_analyses %>%
  group_by(trace_type) %>%
  summarize(avg_duration = mean(process_duration))

ggplot(avg_durations, aes(x = factor(trace_type), y = avg_duration)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Process Duration by Trace Type", x = "Trace Type", y = "Average Duration (hours)") +
  theme_minimal()

ggplot(trace_analyses, aes(x = factor(trace_type), y = process_duration)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Process Duration by Trace Type", x = "Trace Type", y = "Process Duration (hours)") +
  theme_minimal()

ggplot(trace_analyses, aes(x = process_start, y = process_end, color = factor(trace_type))) +
  geom_point(size = 2) +
  geom_segment(aes(xend = process_start, yend = process_end), alpha = 0.5) +
  labs(title = "Process Start and End Times by Trace Type", x = "Start Time", y = "End Time", color = "Trace Type") +
  theme_minimal()

case_counts <- trace_analyses %>%
  group_by(trace_type) %>%
  summarize(case_count = n())

ggplot(case_counts, aes(x = factor(trace_type), y = case_count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Count of Cases by Trace Type", x = "Trace Type", y = "Number of Cases") +
  theme_minimal()

ggplot(trace_analyses, aes(x = factor(trace_type), y = process_duration, fill = process_duration)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(title = "Heatmap of Process Durations by Trace Type", x = "Trace Type", y = "Process Duration (minutes)") +
  theme_minimal()

ggplot(trace_analyses, aes(x = process_start, y = process_duration, color = factor(trace_type))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Start Times vs. Process Durations", x = "Start Time", y = "Process Duration (minutes)", color = "Trace Type") +
  theme_minimal()


# Resource Utilization Plots
# Resource usage over time
plot(get_mon_resources(env), metric = "usage") +
  ggtitle("Resource Usage Over Time")

# Resource utilization over time
plot(get_mon_resources(env), metric = "utilization") +
  ggtitle("Resource Utilization Over Time")

# Check monitored arrival data
arrivals_data <- get_mon_arrivals(env)
#-- count the number of produced products----
nrow(arrivals_data)
mean(arrivals_data$activity_time) #-- average production time

# Plot Flow Time of Arrivals
if (nrow(arrivals_data) > 0) {
  plot(arrivals_data, metric = "flow_time") +
    ggtitle("Flow Time of Arrivals")
} else {
  print("No arrivals data available.")
}

# Plot Waiting Time of Arrivals
if (nrow(arrivals_data) > 0) {
  plot(arrivals_data, metric = "waiting_time") +
    ggtitle("Waiting Times of Arrivals")
} else {
  print("No arrivals data available.")
}



write.csv(simulation_eventlog, "simulation_results_clampProduction.csv", row.names = FALSE)

