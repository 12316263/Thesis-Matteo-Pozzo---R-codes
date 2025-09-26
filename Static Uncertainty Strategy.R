# STATIC UNCERTAINTY STRATEGY

rm(list = ls())         # Clear all objects from workspace: guarantees a clean environment.
graphics.off()          # Close any open plotting devices: avoids drawing over previous plots.

# -------------------------------------
# Experimental Grid parameters
# -------------------------------------
## --- Retailer ---
lead_time_retailer_vals <- 1:3                           # Vector of retailer inbound lead times (days) to test.
setup_retailer_vals     <- 45:55                         # Vector of retailer setup costs to test.
hold_retailer_vals      <- seq(0.1, 0.4, by = 0.1)       # Vector of retailer holding costs to test.
backorder_retailer_vals <- 2:5                           # Vector of retailer backorder penalty costs to test.

Experimental_grid_table_retailer <- expand.grid(         # Build the Cartesian product of retailer parameters.
  lead_time_retailer = lead_time_retailer_vals,          # Column: retailer lead time.
  setup_retailer     = setup_retailer_vals,              # Column: retailer setup.
  hold_retailer      = hold_retailer_vals,               # Column: retailer holding.
  backorder_retailer = backorder_retailer_vals,          # Column: retailer backorder.
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE       # Keep it tidy (no attributes; character not factors).
)

## --- Manufacturer ---
lead_time_manufacturer_vals <- 3:5                       # Vector of manufacturer inbound lead times (days) to test.
setup_manufacturer_vals     <- 190:210                   # Vector of manufacturer setup costs to test.
hold_manufacturer_vals      <- c(0.3, 0.5, 0.7)          # Vector of manufacturer holding costs to test.
backorder_manufacturer_vals <- 1:3                       # Vector of manufacturer backorder penalty costs to test.

Experimental_grid_table_manufacturer <- expand.grid(     # Build the Cartesian product of manufacturer parameters.
  lead_time_manufacturer = lead_time_manufacturer_vals,  # Column: manufacturer lead time.
  setup_manufacturer     = setup_manufacturer_vals,      # Column: manufacturer setup.
  hold_manufacturer      = hold_manufacturer_vals,       # Column: manufacturer holding.
  backorder_manufacturer = backorder_manufacturer_vals,  # Column: manufacturer backorder.
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE       # Keep it tidy.
)

# ============================================================
# Core static strategy (two-echelon) wrapped into functions
# ============================================================

set.seed(123)                        # Fix RNG seed so all scenarios see the same 365-day demand (fair comparison).
T_cost      <- 365                   # Performance window: number of days we accrue costs and evaluate KPIs.
demand_mean <- 100                   # Planning mean demand (used by DP proxy and lot-sizing).
demand_sd   <- 20                    # Planning demand standard deviation (used to generate realized demand).

demand_first365 <-                    # Realized retailer demand for the first 365 days (non-negative integers).
  pmax(round(rnorm(T_cost, mean = demand_mean, sd = demand_sd)), 0)

clamp <- function(x, lo, hi) {        # Utility: clip value x into closed interval [lo, hi].
  max(lo, min(hi, x))
}

# ------------------------------------------------------------
# Shortest-path dynamic program with "arrival-day semantics"
# DP nodes are days; an arc (i -> t) means: place at day i, arrives at i+L, covers days (i+L)..(t-1).
# Cost proxy uses holding on mean demand only to keep DP tractable and stationary.
# ------------------------------------------------------------
shortest_path_days <- function(T_len, setup, holding, lead_time, daily_mean) {
  cost <- rep(Inf, T_len + 1)        # DP value array for nodes 0..T_len (1-based index stores node t-1).
  cost[1] <- 0                       # Base: cost to reach node 0 is zero.
  pred <- rep(NA_integer_, T_len + 1)# To backtrack optimal predecessors.
  
  for (t in 2:(T_len + 1)) {         # For each sink node t (representing end of day t-1)…
    best <- Inf                      # Track best incoming arc cost.
    arg  <- NA_integer_              # Track best predecessor node index.
    for (i in 1:(t - 1)) {           # Consider all predecessors i < t.
      start <- i + lead_time         # Arrival day if we place at i.
      if (start >= t) next           # Must cover at least one day: if arrival is after/equal to t, skip.
      n_days <- t - start            # Number of covered days (from start to t-1 inclusive).
      
      # Holding-cost proxy: mean inventory carried for 1..n_days days.
      # Sum of 1..n = n(n+1)/2; multiply by daily_mean and holding cost/unit/day.
      h_cost <- holding * daily_mean * (n_days * (n_days + 1) / 2)
      
      v <- cost[i] + setup + h_cost  # Total cost reaching node t via node i and arc (i -> t).
      if (v < best) {                # Keep the best incoming arc.
        best <- v
        arg  <- i
      }
    }
    cost[t] <- best                  # Store optimal cost to reach node t.
    pred[t] <- arg                   # Store predecessor that achieves it.
  }
  
  # Backtrack optimal order placements: follow predecessors from terminal node.
  order_days <- c()                  # Will store placement (order) days (before adding lead time).
  cur <- T_len + 1                   # Start from terminal node (end of horizon).
  while (!is.na(pred[cur])) {        # Walk back until we hit the origin (NA).
    order_days <- c(pred[cur], order_days)  # Prepend predecessor day.
    cur <- pred[cur]                 # Move to predecessor.
  }
  repl_days <- order_days + lead_time # Convert placement days to ARRIVAL days (shift by lead time).
  
  list(repl_days = repl_days)        # Return only arrival schedule (quantities planned elsewhere).
}

# ------------------------------------------------------------
# Static lot sizing with exact mass balance up to T_cost
# Each cycle k gets Q_k = mean * (#days in cycle within the first 365 days).
# The last lot is adjusted so total lots == 365 * mean (mass-balance).
# ------------------------------------------------------------
plan_quantities_static <- function(repl_days, T_cost, daily_mean) {
  K <- length(repl_days)             # Number of arrivals/cycles.
  q <- numeric(K)                    # Lot sizes to be received at each arrival.
  n_days_vec <- integer(K)           # Cycle lengths (in days) within the cost window.
  
  for (k in seq_len(K)) {            # For each cycle…
    start <- repl_days[k]            # Cycle starts at its arrival day.
    end   <- if (k < K) min(repl_days[k + 1] - 1, T_cost) else T_cost  # End at day before next arrival or 365.
    n_days <- max(0, end - start + 1)# Cycle length (0 if arrival after T_cost).
    n_days_vec[k] <- n_days          # Save cycle length.
    q[k] <- n_days * daily_mean      # Fixed lot equals expected demand over the cycle (no safety).
  }
  
  target_total <- T_cost * daily_mean # Expected demand over 365 days.
  delta <- target_total - sum(q)      # Correction to match target total exactly.
  if (K >= 1) q[K] <- max(0, q[K] + delta)  # Adjust the last lot (non-negative clamp).
  
  list(q = q, n_days = n_days_vec)    # Return lots and cycle lengths.
}

# ------------------------------------------------------------
# Inventory simulation (single echelon)
# - demand length may be 365 + L to allow last arrivals to land (tail zeros).
# - costs accrue only for t <= T_cost.
# - repl_days are ARRIVAL days; placement is logged at (arrival - lead_time).
# ------------------------------------------------------------
simulate_inventory <- function(demand, repl_days, quantities,
                               setup_cost, holding_cost, backorder_cost, lead_time,
                               T_cost, initial_inventory = 150) {
  n_full <- length(demand)           # Simulation horizon length (365 + echelon lead time).
  inv <- numeric(n_full)             # End-of-day on-hand inventory time series.
  orders <- numeric(n_full)          # Placement time series (for upstream demand).
  stock <- initial_inventory         # On-hand inventory at start.
  backorder <- 0                     # Unmet demand carried over to next day.
  r <- 1                             # Index of next planned arrival in repl_days.
  c_setup <- 0; c_hold <- 0; c_back <- 0  # Cost accumulators.
  
  for (t in 1:n_full) {              # Simulate day by day.
    if (r <= length(repl_days) && t == repl_days[r]) {  # If a lot arrives today…
      qty <- quantities[r]           # Fixed planned quantity for this cycle.
      if (qty > 0) {                 # Ignore zero quantities.
        stock <- stock + qty         # Receive into on-hand stock.
        if (t <= T_cost) c_setup <- c_setup + setup_cost   # Count setup if arrival inside cost window.
        place_day <- clamp(repl_days[r] - lead_time, 1, n_full)  # Placement day = arrival - lead time (clamped).
        orders[place_day] <- orders[place_day] + qty      # Log the placement amount.
      }
      r <- r + 1                     # Move to next arrival.
    }
    
    need <- demand[t] + backorder    # Today's demand plus any backlog must be satisfied.
    ship <- min(need, stock)         # Ship as much as possible (limited by stock).
    stock <- stock - ship            # Reduce stock by shipped amount.
    backorder <- need - ship         # Any unmet demand becomes backlog.
    
    inv[t] <- stock                  # Record end-of-day inventory.
    
    if (t <= T_cost) {               # Accrue costs only within the 365-day window.
      c_hold <- c_hold + stock * holding_cost
      c_back <- c_back + backorder * backorder_cost
    }
  }
  
  list(                               # Return trajectory and KPI totals.
    inventory = inv,
    orders = orders,
    setup_cost = c_setup,
    holding_cost = c_hold,
    backorder_cost = c_back,
    total_cost = c_setup + c_hold + c_back,
    avg_cost = (c_setup + c_hold + c_back) / T_cost
  )
}

# ------------------------------------------------------------
# Scenario runner: executes the full two-echelon simulation for one row of parameters
# ------------------------------------------------------------
run_scenario <- function(params) {
  # --- Unpack scenario parameters from a one-row data.frame/list ---
  Lr <- params$lead_time_retailer
  Sr <- params$setup_retailer
  Hr <- params$hold_retailer
  Br <- params$backorder_retailer
  Lm <- params$lead_time_manufacturer
  Sm <- params$setup_manufacturer
  Hm <- params$hold_manufacturer
  Bm <- params$backorder_manufacturer
  
  # --- Echelon-specific horizons (allow last arrivals to land) ---
  T_full_retailer     <- T_cost + Lr         # Retailer horizon = 365 + Lr.
  T_full_manufacturer <- T_cost + Lm         # Manufacturer horizon = 365 + Lm.
  
  # --- Build demand vector for retailer (365 realized + Lr tail zeros) ---
  demand_vec <- c(demand_first365, rep(0, Lr))
  
  # --- RETAILER planning: arrival days via DP, lot sizing, and simulation ---
  retailer_days <- shortest_path_days(       # Compute optimal arrival schedule for retailer.
    T_len      = T_full_retailer,
    setup      = Sr,
    holding    = Hr,
    lead_time  = Lr,
    daily_mean = demand_mean
  )
  retailer_plan <- plan_quantities_static(   # Fixed lots for each cycle (mass-balanced to 365*mean).
    repl_days  = retailer_days$repl_days,
    T_cost     = T_cost,
    daily_mean = demand_mean
  )
  retailer_initial <- 150                    # Initial on-hand inventory (consistent baseline).
  retailer_sim <- simulate_inventory(        # Simulate retailer inventory & costs.
    demand         = demand_vec,
    repl_days      = retailer_days$repl_days,
    quantities     = retailer_plan$q,
    setup_cost     = Sr,
    holding_cost   = Hr,
    backorder_cost = Br,
    lead_time      = Lr,
    T_cost         = T_cost,
    initial_inventory = retailer_initial
  )
  
  # --- Manufacturer demand is retailer placements that occur within 365 days ---
  manufacturer_demand <- numeric(T_full_manufacturer)          # Initialize manufacturer demand timeline.
  retailer_place_days <- pmax(1, retailer_days$repl_days - Lr) # Placement = arrival - Lr (clamped to ≥ 1).
  for (k in seq_along(retailer_place_days)) {                  # Map each retailer placement…
    place_day <- retailer_place_days[k]
    if (place_day <= T_cost) {                                 # Only placements in the 365-day window count.
      manufacturer_demand[place_day] <-                       # Add a spike equal to the corresponding lot.
        manufacturer_demand[place_day] + retailer_plan$q[k]
    }
  }
  
  # --- Manufacturer planning: arrival days via DP using a mean proxy over its demand timeline ---
  manu_days <- shortest_path_days(
    T_len      = T_full_manufacturer,
    setup      = Sm,
    holding    = Hm,
    lead_time  = Lm,
    daily_mean = if (sum(manufacturer_demand) > 0)
      mean(manufacturer_demand[1:T_cost]) else 0
  )
  
  # --- Manufacturer lots: sum deterministic spikes inside each cycle (only up to day 365) ---
  manu_q <- numeric(length(manu_days$repl_days))               # Allocate lot vector.
  for (k in seq_along(manu_days$repl_days)) {
    start <- manu_days$repl_days[k]                            # Cycle start (arrival day).
    end   <- if (k < length(manu_days$repl_days))              # Cycle end (day before next arrival)…
      manu_days$repl_days[k + 1] - 1 else T_full_manufacturer
    start_c <- max(start, 1)                                   # Clamp lower bound inside horizon.
    end_c   <- min(end, T_cost)                                # Count demand only inside 365-day window.
    manu_q[k] <- if (start_c <= end_c)                         # Sum spikes in the cycle window.
      sum(manufacturer_demand[start_c:end_c]) else 0
  }
  
  # --- Manufacturer initial stock: cover potential demand inside its own lead time (avoid day-1 backlog) ---
  manufacturer_initial <- max(150,                              # Baseline initial inventory.
                              sum(manufacturer_demand[1:min(T_cost, Lm)]))
  
  # --- Simulate manufacturer echelon ---
  manufacturer_sim <- simulate_inventory(
    demand         = manufacturer_demand,
    repl_days      = manu_days$repl_days,
    quantities     = manu_q,
    setup_cost     = Sm,
    holding_cost   = Hm,
    backorder_cost = Bm,
    lead_time      = Lm,
    T_cost         = T_cost,
    initial_inventory = manufacturer_initial
  )
  
  # --- Collect KPIs for this scenario in a single-row data.frame ---
  data.frame(
    lead_time_retailer = Lr,
    setup_retailer     = Sr,
    hold_retailer      = Hr,
    backorder_retailer = Br,
    lead_time_manufacturer = Lm,
    setup_manufacturer     = Sm,
    hold_manufacturer      = Hm,
    backorder_manufacturer = Bm,
    retailer_setups     = retailer_sim$setup_cost,
    retailer_holding    = retailer_sim$holding_cost,
    retailer_backorder  = retailer_sim$backorder_cost,
    retailer_total      = retailer_sim$total_cost,
    retailer_avg_daily  = retailer_sim$avg_cost,
    manufacturer_setups    = manufacturer_sim$setup_cost,
    manufacturer_holding   = manufacturer_sim$holding_cost,
    manufacturer_backorder = manufacturer_sim$backorder_cost,
    manufacturer_total     = manufacturer_sim$total_cost,
    manufacturer_avg_daily = manufacturer_sim$avg_cost,
    sc_total           = retailer_sim$total_cost + manufacturer_sim$total_cost,  # System total.
    sc_avg_daily       = retailer_sim$avg_cost + manufacturer_sim$avg_cost,      # System daily avg.
    retailer_cycles    = length(retailer_days$repl_days),                        # Number of retailer cycles.
    manufacturer_cycles= length(manu_days$repl_days),                            # Number of manufacturer cycles.
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Build the combined grid and evaluate all/some scenarios
# ------------------------------------------------------------
Grid_full <- merge(Experimental_grid_table_retailer,   # Merge (Cartesian product) retailer × manufacturer grids.
                   Experimental_grid_table_manufacturer,
                   by = NULL)

cat("Full Cartesian grid size:", nrow(Grid_full), "scenarios\n")  # Informative print of grid size.

MAX_SCEN <- 1500                         # Runtime guardrail: set to Inf to run all; otherwise sample this many.
if (!is.infinite(MAX_SCEN) && nrow(Grid_full) > MAX_SCEN) {
  set.seed(123)                          # Seed for reproducible sampling of scenarios.
  pick <- sample.int(nrow(Grid_full), MAX_SCEN)  # Randomly choose scenario indices.
  Grid_eval <- Grid_full[pick, , drop = FALSE]   # Subset grid to sampled scenarios.
  cat("Sampling", nrow(Grid_eval),
      "scenarios for evaluation (set MAX_SCEN <- Inf to run all)\n")
} else {
  Grid_eval <- Grid_full                 # Evaluate the full grid when MAX_SCEN is Inf or grid is small.
}

# Evaluate scenarios with a simple text progress bar
n_scen <- nrow(Grid_eval)                # Number of scenarios to evaluate.
pb <- txtProgressBar(min = 0, max = n_scen, style = 3)  # Create progress bar.
Results_list <- vector("list", n_scen)   # Preallocate list to collect per-scenario results.

for (i in seq_len(n_scen)) {             # Loop over all selected scenarios.
  Results_list[[i]] <- run_scenario(Grid_eval[i, ])  # Run the two-echelon simulation.
  if (i %% 5 == 0 || i == n_scen) setTxtProgressBar(pb, i)  # Update progress every 5 scenarios or at end.
}
close(pb)                                # Close the progress bar.

Results <- do.call(rbind, Results_list)  # Row-bind all per-scenario data.frames into one results table.

# Sort results by system total cost (ascending) and show top 10
Results_sorted <- Results[order(Results$sc_total), ]  # Order scenarios by best (lowest) total system cost.
cat("\n===== TOP 10 SCENARIOS (by Total Supply Chain Cost over 365 days) =====\n")
print(head(Results_sorted, 10), row.names = FALSE)    # Print the 10 best scenarios.

# Persist full results to CSV (for later analysis/reporting)
outfile <- "experimental_grid_results.csv"            # Output file name.
write.csv(Results, outfile, row.names = FALSE)        # Save every evaluated scenario.
cat("\nSaved full results to:", normalizePath(outfile), "\n")  # Confirm path to saved file.

#--------------------------------------------
#--------------------------------------------
# ANALYZED PLOTS
#--------------------------------------------
#--------------------------------------------

# ------------------------------------------------------------
# Retailer: Average Daily Cost vs Lead Time
# ------------------------------------------------------------
avg_daily_retailer <- tapply(Results$retailer_avg_daily, Results$lead_time_retailer, mean)

plot(as.numeric(names(avg_daily_retailer)), avg_daily_retailer, type = "b", lwd = 2, pch = 19, col = "blue",
     xlab = "Retailer Lead Time (days)",
     ylab = "Average Daily Cost",
     main = "Static strat. - Retailer: Avg Daily Cost vs Lead Time",
     ylim = range(avg_daily_retailer))

# ------------------------------------------------------------
# Manufacturer: Average Daily Cost vs Lead Time
# ------------------------------------------------------------
avg_daily_manufacturer <- tapply(Results$manufacturer_avg_daily, Results$lead_time_manufacturer, mean)

plot(as.numeric(names(avg_daily_manufacturer)), avg_daily_manufacturer, type = "b", lwd = 2, pch = 19, col = "red",
     xlab = "Manufacturer Lead Time (days)",
     ylab = "Average Daily Cost",
     main = "Static strat. - Manufacturer: Avg Daily Cost vs Lead Time",
     ylim = range(avg_daily_manufacturer))

# ------------------------------------------------------------
# Retailer: Cost Components Distribution
# ------------------------------------------------------------
T_perf <- 365

setup_cost_retailer   <- Results$retailer_setups
holding_cost_retailer <- Results$retailer_holding
backorder_cost_retailer <- Results$retailer_backorder

# Per day costs
cost_SH_r <- (setup_cost_retailer + holding_cost_retailer) / T_perf
cost_BO_r <- backorder_cost_retailer / T_perf

boxplot(list(`Setup+Holding` = cost_SH_r,
             Backorder       = cost_BO_r),
        main = "Static Strat. - Retailer: Cost Component Distribution",
        ylab = "Cost per Day",
        col  = c("lightgreen","pink"),
        las  = 1)

# ------------------------------------------------------------
# Manufacturer: Cost Components per Day (Boxplots)
# ------------------------------------------------------------
T_perf <- 365

# Manufacturer costs per day
cost_SH_m <- (Results$manufacturer_setups + Results$manufacturer_holding) / T_perf
cost_BO_m <- Results$manufacturer_backorder / T_perf

# Boxplot
boxplot(list(`Setup+Holding` = cost_SH_m,
             Backorder       = cost_BO_m),
        main = "Static Strat. - Manufacturer: Cost Component Distribution",
        ylab = "Cost per Day",
        col  = c("lightgreen","pink"),
        las  = 1)

# ------------------------------------------------------------
# Retailer: Avg Daily SC Cost vs Setup Cost
# ------------------------------------------------------------
avg_daily_by_Sr <- tapply(Results$sc_avg_daily, Results$setup_retailer, mean)

plot(as.numeric(names(avg_daily_by_Sr)), avg_daily_by_Sr, type = "b", lwd = 2, pch = 19, col = "blue",
     xlab = "Setup Cost per Order",
     ylab = "Average Daily SC Cost",
     main = "Static Strat. - Retailer: Avg Daily Cost vs Setup Cost")

# ------------------------------------------------------------
# Manufacturer: Avg Daily SC Cost vs Setup Cost
# ------------------------------------------------------------
avg_daily_by_Sm <- tapply(Results$sc_avg_daily, Results$setup_manufacturer, mean)

plot(as.numeric(names(avg_daily_by_Sm)), avg_daily_by_Sm, type = "b", lwd = 2, pch = 17, col = "red",
     xlab = "Setup Cost per Order",
     ylab = "Average Daily SC Cost",
     main = "Static Strat. - Manufacturer: Avg Daily Cost vs Setup Cost")

# ------------------------------------------------------------
# Average Inventory Comparison: Retailer vs Manufacturer
# ------------------------------------------------------------

# Compute average inventory over 365 days for each scenario
avg_inv_retailer    <- Results$retailer_holding / Results$hold_retailer   # reverse-engineer average stock
avg_inv_manufacturer <- Results$manufacturer_holding / Results$hold_manufacturer

# Combine into a data.frame for plotting
avg_inv_df <- data.frame(
  Retailer = avg_inv_retailer,
  Manufacturer = avg_inv_manufacturer
)

# Side-by-side boxplot
boxplot(avg_inv_df,
        main = "Static Strat. - Average Inventory: Retailer vs Manufacturer",
        ylab = "Average Stock (units)",
        col = c("blue", "red"))

# ------------------------------------------------------------
# Average order size
# ------------------------------------------------------------

# Approximate average order sizes per scenario
retailer_order_sizes    <- Results$retailer_total / Results$retailer_cycles
manufacturer_order_sizes <- Results$manufacturer_total / Results$manufacturer_cycles

# Plot histograms side by side
par(mfrow = c(1, 2))

hist(retailer_order_sizes,
     breaks = 30, col = "blue", border = "white",
     main = "Static Strat. - Retailer: Order Size Distribution",
     xlab = "Order Size (units)", ylab = "Frequency")

hist(manufacturer_order_sizes,
     breaks = 30, col = "red", border = "white",
     main = "Static Strat. - Manufacturer: Order Size Distribution",
     xlab = "Order Size (units)", ylab = "Frequency")

par(mfrow = c(1, 1))

# ------------------------------------------------------------
# Dot plot: Avg backorder costs (Static strategy)
# ------------------------------------------------------------
T_perf <- 365

# --- Retailer aggregated value ---
avg_backorder_retailer <- mean(Results$retailer_backorder / T_perf, na.rm = TRUE)

# --- Manufacturer aggregated value ---
avg_backorder_manufacturer <- mean(Results$manufacturer_backorder / T_perf, na.rm = TRUE)

# --- Plot both as dots ---
plot(1:2, c(avg_backorder_retailer, avg_backorder_manufacturer),
     pch = 19, col = c("blue", "red"), cex = 3,
     xaxt = "n", xlab = "",
     ylab = "Avg Backorder Cost per Day",
     main = "Static Strat. - Avg Backorder Costs")
axis(1, at = 1:2, labels = c("Retailer", "Manufacturer"))

# --- Add numeric values next to both dots ---
text(x = 1, y = avg_backorder_retailer,
     labels = round(avg_backorder_retailer, 0),
     pos = 4, cex = 1, col = "black")

text(x = 2, y = avg_backorder_manufacturer,
     labels = round(avg_backorder_manufacturer, 0),
     pos = 4, cex = 1, col = "black")

# ------------------------------------------------------------
# Line Plot: Average Backorders vs Lead Time
# ------------------------------------------------------------

# --- Retailer ---
avg_backorder_retailer <- aggregate(retailer_backorder / T_cost ~ lead_time_retailer,
                                    data = Results, mean)

plot(avg_backorder_retailer$lead_time_retailer,
     avg_backorder_retailer$`retailer_backorder/T_cost`,
     type = "b", pch = 19, col = "blue", lwd = 2,
     xlab = "Lead Time (days)",
     ylab = "Average Backorders per Day",
     main = "Static Strat. - Retailer: Avg Backorders vs Lead Time")
grid(nx = NA, ny = NULL)

# --- Manufacturer ---
avg_backorder_manufacturer <- aggregate(manufacturer_backorder / T_cost ~ lead_time_manufacturer,
                                        data = Results, mean)

plot(avg_backorder_manufacturer$lead_time_manufacturer,
     avg_backorder_manufacturer$`manufacturer_backorder/T_cost`,
     type = "b", pch = 17, col = "red", lwd = 2,
     xlab = "Lead Time (days)",
     ylab = "Average Backorders per Day",
     main = "Static Strat. - Manufacturer: Avg Backorders vs Lead Time")
grid(nx = NA, ny = NULL)

# -------------------------------
# Total costs over 1 year (365 days)
# -------------------------------

days_year <- 365

# Average daily cost = mean of scenario averages
avg_daily_cost_retailer     <- mean(Results$retailer_avg_daily, na.rm = TRUE)
avg_daily_cost_manufacturer <- mean(Results$manufacturer_avg_daily, na.rm = TRUE)

# Scale to 365 days
total_year_retailer      <- avg_daily_cost_retailer * days_year
total_year_manufacturer  <- avg_daily_cost_manufacturer * days_year
total_year_supply_chain  <- total_year_retailer + total_year_manufacturer

# Print nicely
cat("Retailer Cost over 1 year:", round(total_year_retailer, 2), "\n")
cat("Manufacturer Cost over 1 year:", round(total_year_manufacturer, 2), "\n")
cat("Supply Chain Cost over 1 year:", round(total_year_supply_chain, 2), "\n")
