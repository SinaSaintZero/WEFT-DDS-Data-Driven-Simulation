# 
#---> Code Chunk to evaluate the system runTime

# -------------------- Libraries --------------------
library(DiagrammeR)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(simmer)
library(stringr)
library(ggplot2)

# -------------------- Load Event Log -------------
EventLog <- read.csv(file.choose(), header = TRUE)
DF_EventLog <- data.frame(EventLog)

setDT(DF_EventLog)

dt <- data.table(
  ID = DF_EventLog[[1]],
  Activity = as.character(DF_EventLog[[2]]),
  Resource = if (ncol(DF_EventLog) >= 3) DF_EventLog[[3]] else NA
)

set.seed(42)

all_ids <- unique(dt$ID)
train_ids <- sample(all_ids, size = 0.7 * length(all_ids))

train_log <- dt[ID %in% train_ids]

# ----------------Directly-Follows ---------
shift <- data.table::shift
consecutive_id <- train_log[, .(
  From = Activity,
  To = shift(Activity, type = "lead")
), by = ID][!is.na(To)]

Activity_Direct_Relation <- consecutive_id[, .N, by = .(From, To)]

# ------------------ Dependency Matrix ---------------
All_Activities <- unique(train_log$Activity)
n <- length(All_Activities)

DependencyMatrix <- matrix(0, nrow = n, ncol = n,
                           dimnames = list(All_Activities, All_Activities))

for (i in seq_len(nrow(Activity_Direct_Relation))) {
  DependencyMatrix[
    Activity_Direct_Relation$From[i],
    Activity_Direct_Relation$To[i]
  ] <- Activity_Direct_Relation$N[i]
}

# -------------------- Stability --------------------
df2 <- as.data.frame(DependencyMatrix) %>%
  tibble::rownames_to_column("From") %>%
  pivot_longer(-From, names_to = "To", values_to = "val") %>%
  filter(val > 0)

MR_average <- mean(abs(diff(df2$val)), na.rm = TRUE)
x_average  <- mean(df2$val, na.rm = TRUE)

UCL_indiv <- x_average + 3 * (MR_average / 2.534)
LCL_indiv <- max(0, x_average - 3 * (MR_average / 2.534))

StableEdges <- df2 %>% filter(val <= UCL_indiv)
HotEdges    <- df2 %>% filter(val > UCL_indiv)

# ----------------- Graph -----------
nodes_processModel <- create_node_df(
  n = length(All_Activities),
  label = All_Activities
)

edges_df <- create_edge_df(
  from = match(Activity_Direct_Relation$From, nodes_processModel$label),
  to   = match(Activity_Direct_Relation$To, nodes_processModel$label)
)

ProcessModelStableMiner2 <- create_graph(
  nodes_df = nodes_processModel,
  edges_df = edges_df,
  directed = TRUE
)

render_graph(ProcessModelStableMiner2)

      # ======> SIMULATION PART 

# =------ SIMULATION + RUNTIME EVALUATION -----

library(simmer)
library(dplyr)

# -------- Ensure All_edges exists --------
All_edges <- Activity_Direct_Relation %>%
  mutate(id = row_number(), label = paste(From, "->", To))

# -------- Machine capacities --------
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
  #----Depending on the case study we can modify the resource usage

)

# RUNTIME EXPERIMENT 

times <- numeric(10)  # 10 runs

for (run in 1:10) {
  
  timing <- system.time({
    
    # -------- Workflow extraction --------
    automatically_discovered_process_workflow <- All_edges %>%
      filter(!is.na(id))
    
    # -------- Deterministic resource mapping --------
    resources_list <- names(machine_capacities)
    
    workflow <- simmer::trajectory("Workflow")
    
    for (i in seq_len(nrow(automatically_discovered_process_workflow))) {
      
      # deterministic assignment (no randomness)
      resource_from <- resources_list[(i %% length(resources_list)) + 1]
      resource_to   <- resources_list[((i + 1) %% length(resources_list)) + 1]
      
      workflow <- workflow %>%
        simmer::seize(resource_from) %>%
        simmer::timeout(function() stats::rnorm(1, mean = 4, sd = 1)) %>%
        simmer::release(resource_from) %>%
        simmer::seize(resource_to) %>%
        simmer::timeout(function() stats::rexp(1, rate = 1/3)) %>%
        simmer::release(resource_to)
    }
    
    # -------- Environment creation --------
    env <- simmer::simmer("Factory Simulation")
    
    for (resource in names(machine_capacities)) {
      env <- env %>%
        simmer::add_resource(resource, capacity = machine_capacities[resource])
    }
    
    # -------- Generator --------
    arrival_rate <- 0.05
    
    env <- env %>%
      simmer::add_generator(
        "product",
        workflow,
        function() stats::rexp(1, rate = arrival_rate)
      )
    
  })
  
  times[run] <- timing["elapsed"]
}

# Results of evaluation of the simulationRunTime

mean_time <- mean(times)
sd_time   <- sd(times)

cat("Simulation environment design time:\n")
cat("Mean:", round(mean_time, 4), "seconds\n")
cat("Std Dev:", round(sd_time, 4), "seconds\n")
sessionInfo()
Sys.info()
parallel::detectCores()
