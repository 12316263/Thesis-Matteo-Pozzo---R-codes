# DYNAMIC UNCERTAINTY STRATEGY

rm(list = ls())                 # Remove all objects from the current R session to avoid interference.
graphics.off()                  # Close any open plotting devices (clears previous plots).

set.seed(123)                   # Fix random seed so all stochastic results are reproducible.

# -------------------------------------
# Experimental Grid parameters
# -------------------------------------
## --- Retailer ---
s_vals_retailer         <- seq(100, 200, by = 10)   # Retailer candidate reorder points s: 100,110,...,200.
S_vals_retailer         <- seq(130, 230, by = 10)   # Retailer candidate order-up-to levels S: 130,140,...,230.
lead_time_retailer_vals <- 1:3                      # Retailer lead times (days) to test: 1,2,3.
setup_retailer_vals     <- 45:55                    # Retailer setup (fixed order) costs: 45..55.
hold_retailer_vals      <- seq(0.1, 0.4, by = 0.1)  # Retailer holding costs per unit/day: 0.1,0.2,0.3,0.4.
backorder_retailer_vals <- 2:5                      # Retailer backorder costs per unit/day: 2..5.

Experimental_grid_table_retailer <- expand.grid(    # Build the full Cartesian product of all retailer factors.
  s_retailer              = s_vals_retailer,        # Column for s values.
  S_retailer              = S_vals_retailer,        # Column for S values.
  lead_time_retailer      = lead_time_retailer_vals,# Column for lead time values.
  setup_retailer          = setup_retailer_vals,    # Column for setup costs.
  hold_retailer           = hold_retailer_vals,     # Column for holding costs.
  backorder_retailer      = backorder_retailer_vals,# Column for backorder costs.
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE  # Cleaner data frame; keep strings as strings.
)

# Keep only feasible pairs
Experimental_grid_table_retailer <- subset(         # Filter grid rows to only those where S > s.
  Experimental_grid_table_retailer,
  S_retailer > s_retailer
)

## --- Manufacturer ---
s_vals_manufacturer         <- seq(1000, 1300, by = 20) # Manufacturer candidate s: 1000,1020,...,1300.
S_vals_manufacturer         <- seq(1300, 1400, by = 20) # Manufacturer candidate S: 1300,1320,...,1400.
lead_time_manufacturer_vals <- 3:5                      # Manufacturer lead times (days): 3,4,5.
setup_manufacturer_vals     <- 190:210                  # Manufacturer setup costs: 190..210.
hold_manufacturer_vals      <- c(0.3, 0.5, 0.7)         # Manufacturer holding costs per unit/day.
backorder_manufacturer_vals <- 1:3                      # Manufacturer backorder costs per unit/day.

Experimental_grid_table_manufacturer <- expand.grid(   # Build the full Cartesian product for manufacturer.
  s_manufacturer              = s_vals_manufacturer,   # Column for s values.
  S_manufacturer              = S_vals_manufacturer,   # Column for S values.
  lead_time_manufacturer      = lead_time_manufacturer_vals, # Column for lead times.
  setup_manufacturer          = setup_manufacturer_vals,     # Column for setup costs.
  hold_manufacturer           = hold_manufacturer_vals,      # Column for holding costs.
  backorder_manufacturer      = backorder_manufacturer_vals, # Column for backorder costs.
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE     # Cleaner data frame; keep strings as strings.
)

# Keep only feasible pairs
Experimental_grid_table_manufacturer <- subset(        # Filter to feasible manufacturer pairs where S > s.
  Experimental_grid_table_manufacturer,
  S_manufacturer > s_manufacturer
)

# -------------------------------
# Generate stochastic demand (500 days)
# -------------------------------
num_days <- 500                                        # Simulation horizon length in days.
demand_mean <- 100                                     # Mean of daily end-customer demand.
demand_sd   <- 20                                      # Standard deviation of daily demand.
alpha <- 0.95                                          # Target service quantile (used for demand scaling).
q_alpha <- qnorm(alpha, mean = demand_mean, sd = demand_sd)  # Compute the Normal(alpha) quantile.
safety_factor <- q_alpha / demand_mean                 # Scale so mean*factor ≈ q_alpha (adds conservatism).

demand <- pmax(round(rnorm(num_days, mean = demand_mean, sd = demand_sd)), 0) # Simulate integer demand, truncate <0.
demand <- round(demand * safety_factor)                # Inflate realized demand by safety factor.

# Helper
inv_position <- function(on_hand, pipeline_vec, backlog) { # Compute inventory position at an echelon.
  on_hand + if (length(pipeline_vec)) sum(pipeline_vec) else 0 - backlog     # IP = on-hand + pipeline − backlog.
}

# -------------------------------
# Optimization function for retailer (s, S) — cost-only (inventory position trigger)
# -------------------------------
simulate_retailer_cost <- function(                    # Retailer simulator that returns total cost only.
  s, S, demand,                                     # (s,S) parameters and demand vector.
  initial_inventory = 150,                           # Initial on-hand inventory units.
  lead_time = 2,                                     # Replenishment lead time (days).
  setup_cost = 50,                                   # Fixed setup cost per order.
  holding_cost = 0.5,                                # Holding cost per unit per day.
  backorder_cost = 2                                 # Backorder penalty per unit per day.
) {
  stopifnot(S > s)                                     # Enforce feasibility: S must be greater than s.
  inventory <- initial_inventory                       # State: on-hand inventory.
  pipeline  <- if (lead_time > 0) rep(0, lead_time) else numeric(0)  # Pipeline vector (length = lead_time) or empty.
  backorder <- 0                                       # State: backlog (unmet demand carried forward).
  total_setup_cost <- 0                                # Accumulator: setup cost.
  total_holding_cost <- 0                              # Accumulator: holding cost.
  total_backorder_cost <- 0                            # Accumulator: backorder cost.
  
  for (t in seq_along(demand)) {                       # Iterate day by day across the horizon.
    # Receive
    if (length(pipeline)) {                            # If lead time > 0, receive the oldest pipeline slot.
      inventory <- inventory + pipeline[1]             # Add arrivals to on-hand.
      pipeline <- c(pipeline[-1], 0)                   # Shift pipeline left by one day; free last slot.
    }
    # Serve demand + backlog
    effective_demand <- demand[t] + backorder          # Total to satisfy today = new demand + backlog.
    shipped <- min(effective_demand, inventory)        # Ship what you can (bounded by on-hand).
    inventory <- inventory - shipped                   # Update on-hand after shipping.
    backorder <- effective_demand - shipped            # Any unmet demand remains as backlog.
    # Costs
    total_holding_cost   <- total_holding_cost   + inventory * holding_cost # Holding cost for end-of-day on-hand.
    total_backorder_cost <- total_backorder_cost + backorder  * backorder_cost # Backorder penalty for remaining backlog.
    # Reorder using inventory position (on-hand + pipeline − backlog)
    inv_pos <- inv_position(inventory, pipeline, backorder)  # Compute inventory position after serving.
    if (inv_pos <= s) {                              # If position hits/below s, place an order up to S.
      order_qty <- S - inv_pos                       # Order size to bring position to S.
      if (order_qty > 0) {                           # Only proceed if strictly positive.
        if (length(pipeline)) pipeline[length(pipeline)] <- pipeline[length(pipeline)] + order_qty else inventory <- inventory + order_qty # Schedule receipt at LT or immediate if LT=0.
        total_setup_cost <- total_setup_cost + setup_cost # Pay setup cost for this order.
      }
    }
  }
  total_setup_cost + total_holding_cost + total_backorder_cost  # Return total cost over the horizon.
}

optimize_ss <- function(                               # Brute-force grid search for retailer (s,S).
  demand,                                            # Demand vector used for evaluation.
  s_values = seq(100, 200, by = 10),                 # Candidate s values to test.
  S_values = seq(130, 230, by = 10)                  # Candidate S values to test.
) {
  results <- data.frame()                              # Table to gather (s,S,Cost) rows.
  for (s in s_values) {                                # Loop across s candidates.
    for (S in S_values) {                              # Loop across S candidates.
      if (S > s) {                                     # Only feasible combinations S > s.
        cost <- simulate_retailer_cost(s, S, demand)   # Simulate total cost for (s,S).
        results <- rbind(results, data.frame(s = s, S = S, Cost = cost))  # Append outcome.
      }
    }
  }
  best <- results[which.min(results$Cost), ]           # Extract row with minimum total cost.
  print(paste("Best s (retailer):", best$s, "| Best S:", best$S, "| Cost:", round(best$Cost, 2))) # Report best.
  best                                                  # Return best row.
}

best_ss   <- optimize_ss(demand)                        # Run optimizer to find retailer’s best (s,S) on the coarse grid.
retailer_s <- best_ss$s                                 # Save optimal s for later use.
retailer_S <- best_ss$S                                 # Save optimal S for later use.

# -------------------------------
# Simulate retailer with optimal (s, S) — full traces
# -------------------------------
simulate_retailer_full <- function(                     # Retailer simulator that returns traces + cost breakdown.
  s, S, demand,                                      # (s,S) and demand vector.
  initial_inventory = 150,                           # Initial on-hand units.
  lead_time = 2,                                     # Lead time in days.
  setup_cost = 50,                                   # Setup (fixed) cost per order.
  holding_cost = 0.5,                                # Holding cost per unit per day.
  backorder_cost = 2                                 # Backorder cost per unit per day.
) {
  stopifnot(S > s)                                     # Sanity check on feasibility.
  inventory <- initial_inventory                       # Initialize on-hand.
  pipeline  <- if (lead_time > 0) rep(0, lead_time) else numeric(0) # Initialize pipeline vector (or empty).
  backorder <- 0                                       # Initialize backlog.
  total_setup_cost <- 0                                # Reset cost accumulators.
  total_holding_cost <- 0
  total_backorder_cost <- 0
  order_vector    <- numeric(length(demand))           # To record orders placed each day.
  inventory_trace <- numeric(length(demand))           # To record end-of-day on-hand each day.
  
  for (t in seq_along(demand)) {                       # Simulate over all days.
    if (length(pipeline)) {                            # Receive pipeline arrivals for today.
      inventory <- inventory + pipeline[1]             # Add to on-hand.
      pipeline <- c(pipeline[-1], 0)                   # Shift pipeline forward.
    }
    effective_demand <- demand[t] + backorder          # Demand to satisfy = new + backlog.
    shipped <- min(effective_demand, inventory)        # Ship what’s possible.
    inventory <- inventory - shipped                   # Update on-hand.
    backorder <- effective_demand - shipped            # Update backlog.
    
    total_holding_cost   <- total_holding_cost   + inventory * holding_cost   # Accumulate holding cost.
    total_backorder_cost <- total_backorder_cost + backorder  * backorder_cost# Accumulate backorder cost.
    
    inv_pos <- inv_position(inventory, pipeline, backorder) # Compute inventory position.
    if (inv_pos <= s) {                            # If position is at/below s, place order to raise to S.
      order_qty <- S - inv_pos                     # Compute order size.
      if (order_qty > 0) {                         # Only place positive orders.
        if (length(pipeline)) pipeline[length(pipeline)] <- pipeline[length(pipeline)] + order_qty else inventory <- inventory + order_qty # Schedule or immediate receipt.
        order_vector[t] <- order_qty               # Log the order placed today.
        total_setup_cost <- total_setup_cost + setup_cost # Add setup cost component.
      }
    }
    inventory_trace[t] <- inventory                # Record end-of-day on-hand.
  }
  
  total_cost <- total_setup_cost + total_holding_cost + total_backorder_cost # Total cost across horizon.
  avg_cost_per_day <- total_cost / length(demand)                            # Average daily cost metric.
  
  list(                                            # Return a list of outputs for later analysis.
    total_cost = total_cost,                       # Total cost across the horizon.
    avg_cost   = avg_cost_per_day,                 # Average cost per day.
    setup_cost = total_setup_cost,                 # Setup cost component.
    holding_cost = total_holding_cost,             # Holding cost component.
    backorder_cost = total_backorder_cost,         # Backorder cost component.
    inventory  = inventory_trace,                  # Time series of inventory.
    orders     = order_vector                      # Time series of orders placed.
  )
}

retailer_result <- simulate_retailer_full(retailer_s, retailer_S, demand) # Run retailer with its optimal (s,S).

# --------------
# Experimental grid retailer simulation (cost-only)
# --------------
n <- nrow(Experimental_grid_table_retailer)        # Total number of retailer scenarios to evaluate.
cost_collector <- numeric(n)                       # Pre-allocate vector to collect total costs.

for (u in 1:n) {                                   # Loop over each retailer scenario u = 1..n.
  cost_collector[u] <- simulate_retailer_cost(     # Simulate cost-only to keep it fast for the big grid.
    s               = Experimental_grid_table_retailer$s_retailer[u],      # s of this scenario.
    S               = Experimental_grid_table_retailer$S_retailer[u],      # S of this scenario.
    demand          = demand,                                              # Same demand for comparability.
    initial_inventory = 150,                                              # Initial on-hand.
    lead_time       = Experimental_grid_table_retailer$lead_time_retailer[u], # Lead time for this scenario.
    setup_cost      = Experimental_grid_table_retailer$setup_retailer[u],     # Setup cost for this scenario.
    holding_cost    = Experimental_grid_table_retailer$hold_retailer[u],      # Holding cost for this scenario.
    backorder_cost  = Experimental_grid_table_retailer$backorder_retailer[u]  # Backorder cost for this scenario.
  )
}

Experimental_grid_table_retailer$total_retailer_cost <- cost_collector        # Attach total cost to grid table.
Experimental_grid_table_retailer$avg_cost_per_day   <-                       # Compute and attach avg cost/day.
  Experimental_grid_table_retailer$total_retailer_cost / num_days
sum_demand <- sum(demand)                                                     # Total units demanded across horizon.
Experimental_grid_table_retailer$avg_cost_per_unit  <-                       # Compute avg cost per unit demanded.
  Experimental_grid_table_retailer$total_retailer_cost / max(1, sum_demand)  # Guard against divide-by-zero.

# -------------------------------
# FULL retailer simulations with summaries (traces summarized)
# -------------------------------
pb <- txtProgressBar(min = 0, max = n, style = 3)   # Create a console progress bar for user feedback.

retailer_full_summary <- data.frame(                # Preallocate a summary table (one row per scenario).
  row_id              = integer(n),                 # Grid row index.
  s                   = numeric(n),                 # s used.
  S                   = numeric(n),                 # S used.
  lead_time           = integer(n),                 # Lead time used.
  setup               = numeric(n),                 # Setup cost used.
  holding             = numeric(n),                 # Holding cost used.
  backorder           = numeric(n),                 # Backorder cost used.
  total_cost          = numeric(n),                 # Total cost from full simulation.
  avg_cost_per_day    = numeric(n),                 # Average daily cost.
  avg_cost_per_unit   = numeric(n),                 # Total cost divided by total demand.
  setup_per_day       = numeric(n),                 # Setup cost per day.
  holding_per_day     = numeric(n),                 # Holding cost per day.
  backorder_per_day   = numeric(n),                 # Backorder cost per day.
  mean_inventory      = numeric(n),                 # Mean end-of-day on-hand.
  max_inventory       = numeric(n),                 # Maximum end-of-day on-hand.
  orders_count        = integer(n),                 # Number of days with positive orders.
  order_frequency     = numeric(n),                 # Orders per day.
  mean_order_size     = numeric(n),                 # Average positive order size.
  sd_order_size       = numeric(n),                 # Std dev of positive order sizes.
  stringsAsFactors = FALSE                          # Keep character columns as strings (legacy safety).
)

for (i in seq_len(n)) {                             # Loop over each retailer scenario for full simulation.
  p <- Experimental_grid_table_retailer[i, ]       # Extract the i-th parameter combination.
  sim <- simulate_retailer_full(                   # Run detailed simulation with traces.
    s                = p$s_retailer,               # Scenario s.
    S                = p$S_retailer,               # Scenario S.
    demand           = demand,                     # Same demand series.
    initial_inventory= 150,                        # Initial on-hand.
    lead_time        = p$lead_time_retailer,       # Scenario lead time.
    setup_cost       = p$setup_retailer,           # Scenario setup cost.
    holding_cost     = p$hold_retailer,            # Scenario holding cost.
    backorder_cost   = p$backorder_retailer        # Scenario backorder cost.
  )
  
  inv <- sim$inventory                              # Convenience: daily inventory time series.
  ord <- sim$orders                                 # Convenience: daily order time series.
  pos <- ord > 0                                    # Logical vector (TRUE where an order was placed).
  ord_pos <- ord[pos]                               # Vector of strictly positive order quantities.
  
  retailer_full_summary$row_id[i]            <- i                   # Record row index.
  retailer_full_summary$s[i]                 <- p$s_retailer        # Record s.
  retailer_full_summary$S[i]                 <- p$S_retailer        # Record S.
  retailer_full_summary$lead_time[i]         <- p$lead_time_retailer# Record lead time.
  retailer_full_summary$setup[i]             <- p$setup_retailer     # Record setup cost.
  retailer_full_summary$holding[i]           <- p$hold_retailer      # Record holding cost.
  retailer_full_summary$backorder[i]         <- p$backorder_retailer # Record backorder cost.
  
  retailer_full_summary$total_cost[i]        <- sim$total_cost       # Store total cost.
  retailer_full_summary$avg_cost_per_day[i]  <- sim$avg_cost         # Store average daily cost.
  retailer_full_summary$avg_cost_per_unit[i] <- sim$total_cost / max(1, sum_demand) # Cost per demanded unit.
  
  retailer_full_summary$setup_per_day[i]     <- sim$setup_cost   / num_days   # Setup cost per day.
  retailer_full_summary$holding_per_day[i]   <- sim$holding_cost / num_days   # Holding cost per day.
  retailer_full_summary$backorder_per_day[i] <- sim$backorder_cost / num_days # Backorder cost per day.
  
  retailer_full_summary$mean_inventory[i]    <- mean(inv)                  # Average end-of-day inventory.
  retailer_full_summary$max_inventory[i]     <- max(inv, na.rm = TRUE)     # Max end-of-day inventory.
  retailer_full_summary$orders_count[i]      <- sum(pos)                    # Number of positive-order days.
  retailer_full_summary$order_frequency[i]   <- sum(pos) / num_days         # Orders per day.
  retailer_full_summary$mean_order_size[i]   <- if (length(ord_pos)) mean(ord_pos) else 0 # Mean positive order.
  retailer_full_summary$sd_order_size[i]     <- if (length(ord_pos)) sd(ord_pos)   else 0 # SD positive order.
  
  setTxtProgressBar(pb, i)                    # Update the progress bar.
}
close(pb)                                      # Close the progress bar.

# Attach headline metrics back to the original grid
Experimental_grid_table_retailer$total_retailer_cost <- retailer_full_summary$total_cost      # Copy totals back.
Experimental_grid_table_retailer$avg_cost_per_day    <- retailer_full_summary$avg_cost_per_day# Copy avg/day back.
Experimental_grid_table_retailer$avg_cost_per_unit   <- retailer_full_summary$avg_cost_per_unit# Copy avg/unit back.

# -------------------------------
# Get manufacturer demand from retailer orders
# -------------------------------
manufacturer_demand <- retailer_result$orders        # Upstream demand for manufacturer is retailer’s order stream.

# -------------------------------
# (Optional heuristic) Set manufacturer (s, S) via high service
# -------------------------------
alpha_manufacturer <- 0.9999                         # Very high target service level for manufacturer.
q_alpha_m <- qnorm(alpha_manufacturer, mean = mean(manufacturer_demand), sd = sd(manufacturer_demand)) # Quantile of retailer orders.
manufacturer_lead_time <- 5                          # Manufacturer lead time (days) for heuristic sizing.
manufacturer_S <- ceiling(manufacturer_lead_time * q_alpha_m)  # Heuristic S ≈ LT * high quantile of demand.
manufacturer_s <- round(manufacturer_S * 0.8)        # Heuristic s set to 80% of S (conservative).

# -------------------------------
# Simulate manufacturer (inventory position) — function + full grid evaluation
# -------------------------------
simulate_manufacturer_full <- function(               # Manufacturer simulator (returns traces + cost breakdown).
  s, S, demand,                                    # (s,S) and demand vector.
  initial_inventory = 500,                         # Initial on-hand units at manufacturer.
  lead_time = 5,                                   # Manufacturer lead time (days).
  setup_cost = 200,                                # Setup cost per order at manufacturer.
  holding_cost = 0.3,                              # Holding cost per unit per day.
  backorder_cost = 1.5                             # Backorder cost per unit per day.
) {
  stopifnot(S > s)                                   # Enforce feasibility: S > s.
  inventory <- initial_inventory                     # Initialize state: on-hand.
  pipeline  <- if (lead_time > 0) rep(0, lead_time) else numeric(0)  # Initialize pipeline vector or empty.
  backorder <- 0                                     # Initialize backlog.
  total_setup_cost <- 0                              # Reset cost accumulators.
  total_holding_cost <- 0
  total_backorder_cost <- 0
  order_vector    <- numeric(length(demand))         # Record daily orders placed by manufacturer.
  inventory_trace <- numeric(length(demand))         # Record end-of-day on-hand inventory.
  
  for (t in seq_along(demand)) {                     # Iterate over all days.
    if (length(pipeline)) {                          # Receive arrivals scheduled for today.
      inventory <- inventory + pipeline[1]           # Add arrivals to on-hand.
      pipeline <- c(pipeline[-1], 0)                 # Shift pipeline vector left by one day.
    }
    effective_demand <- demand[t] + backorder        # Demand to satisfy = new demand + backlog.
    shipped <- min(effective_demand, inventory)      # Ship what is feasible given on-hand.
    inventory <- inventory - shipped                 # Update on-hand after shipping.
    backorder <- effective_demand - shipped          # Remaining unmet demand becomes backlog.
    
    total_holding_cost   <- total_holding_cost   + inventory * holding_cost   # Accumulate holding cost.
    total_backorder_cost <- total_backorder_cost + backorder  * backorder_cost# Accumulate backorder cost.
    
    inv_pos <- inv_position(inventory, pipeline, backorder) # Compute inventory position.
    if (inv_pos <= s) {                           # If position at/below s, place an order.
      order_qty <- S - inv_pos                    # Order up to S on inventory position basis.
      if (order_qty > 0) {                        # Only place positive orders.
        if (length(pipeline)) pipeline[length(pipeline)] <- pipeline[length(pipeline)] + order_qty else inventory <- inventory + order_qty # Schedule receipt at LT or immediate if LT=0.
        order_vector[t] <- order_qty              # Log today’s order size.
        total_setup_cost <- total_setup_cost + setup_cost # Add setup cost.
      }
    }
    inventory_trace[t] <- inventory               # Record end-of-day on-hand.
  }
  
  total_cost <- total_setup_cost + total_holding_cost + total_backorder_cost # Sum of all cost components.
  avg_cost_per_day <- total_cost / length(demand)                            # Average cost per day.
  
  list(                                            # Return detailed outputs for analysis.
    total_cost = total_cost,                       # Total cost across the horizon.
    avg_cost   = avg_cost_per_day,                 # Average daily cost.
    setup_cost = total_setup_cost,                 # Setup component.
    holding_cost = total_holding_cost,             # Holding component.
    backorder_cost = total_backorder_cost,         # Backorder component.
    inventory  = inventory_trace,                  # Inventory time series.
    orders     = order_vector                      # Orders time series.
  )
}

# MANUFACTURER — Full grid summaries
n_m <- nrow(Experimental_grid_table_manufacturer)   # Number of manufacturer scenarios to evaluate.
sum_demand_m <- sum(manufacturer_demand)            # Total units demanded by manufacturer across horizon.
pb_m <- txtProgressBar(min = 0, max = n_m, style = 3) # Progress bar for manufacturer loop.

manufacturer_full_summary <- data.frame(            # Preallocate manufacturer summary table.
  row_id              = integer(n_m),               # Grid row index.
  s                   = numeric(n_m),               # s used.
  S                   = numeric(n_m),               # S used.
  lead_time           = integer(n_m),               # Lead time used.
  setup               = numeric(n_m),               # Setup cost used.
  holding             = numeric(n_m),               # Holding cost used.
  backorder           = numeric(n_m),               # Backorder cost used.
  total_cost          = numeric(n_m),               # Total cost across horizon.
  avg_cost_per_day    = numeric(n_m),               # Average daily cost.
  avg_cost_per_unit   = numeric(n_m),               # Total cost / total manufacturer demand.
  setup_per_day       = numeric(n_m),               # Setup cost per day.
  holding_per_day     = numeric(n_m),               # Holding cost per day.
  backorder_per_day   = numeric(n_m),               # Backorder cost per day.
  mean_inventory      = numeric(n_m),               # Mean end-of-day on-hand.
  max_inventory       = numeric(n_m),               # Max end-of-day on-hand.
  orders_count        = integer(n_m),               # Number of positive-order days.
  order_frequency     = numeric(n_m),               # Orders per day.
  mean_order_size     = numeric(n_m),               # Mean positive order size.
  sd_order_size       = numeric(n_m),               # Std dev of positive order sizes.
  stringsAsFactors = FALSE                          # Keep strings as strings.
)

for (i in seq_len(n_m)) {                           # Loop over each manufacturer scenario.
  p <- Experimental_grid_table_manufacturer[i, ]    # Extract the i-th parameter set.
  sim <- simulate_manufacturer_full(                # Run full manufacturer simulation.
    s               = p$s_manufacturer,             # Scenario s.
    S               = p$S_manufacturer,             # Scenario S.
    demand          = manufacturer_demand,          # Demand stream from retailer orders.
    initial_inventory = 500,                        # Initial on-hand at manufacturer.
    lead_time       = p$lead_time_manufacturer,     # Scenario lead time.
    setup_cost      = p$setup_manufacturer,         # Scenario setup cost.
    holding_cost    = p$hold_manufacturer,          # Scenario holding cost.
    backorder_cost  = p$backorder_manufacturer      # Scenario backorder cost.
  )
  
  inv <- sim$inventory                              # Inventory time series.
  ord <- sim$orders                                 # Orders time series.
  pos <- ord > 0                                    # TRUE on days with a positive order.
  ord_pos <- ord[pos]                               # Vector of strictly positive order sizes.
  
  manufacturer_full_summary$row_id[i]    <- i                           # Save grid row index.
  manufacturer_full_summary$s[i]         <- p$s_manufacturer             # Save s.
  manufacturer_full_summary$S[i]         <- p$S_manufacturer             # Save S.
  manufacturer_full_summary$lead_time[i] <- p$lead_time_manufacturer     # Save lead time.
  manufacturer_full_summary$setup[i]     <- p$setup_manufacturer         # Save setup cost.
  manufacturer_full_summary$holding[i]   <- p$hold_manufacturer          # Save holding cost.
  manufacturer_full_summary$backorder[i] <- p$backorder_manufacturer     # Save backorder cost.
  
  manufacturer_full_summary$total_cost[i]        <- sim$total_cost       # Total cost for this scenario.
  manufacturer_full_summary$avg_cost_per_day[i]  <- sim$avg_cost         # Average cost per day.
  manufacturer_full_summary$avg_cost_per_unit[i] <- sim$total_cost / max(1, sum_demand_m) # Cost per demanded unit.
  
  manufacturer_full_summary$setup_per_day[i]     <- sim$setup_cost   / num_days  # Setup/day.
  manufacturer_full_summary$holding_per_day[i]   <- sim$holding_cost / num_days  # Holding/day.
  manufacturer_full_summary$backorder_per_day[i] <- sim$backorder_cost / num_days# Backorder/day.
  
  manufacturer_full_summary$mean_inventory[i]  <- mean(inv)                 # Mean inventory.
  manufacturer_full_summary$max_inventory[i]   <- max(inv, na.rm = TRUE)    # Max inventory.
  manufacturer_full_summary$orders_count[i]    <- sum(pos)                  # Count of order days.
  manufacturer_full_summary$order_frequency[i] <- sum(pos) / num_days       # Orders per day.
  manufacturer_full_summary$mean_order_size[i] <- if (length(ord_pos)) mean(ord_pos) else 0 # Mean order size.
  manufacturer_full_summary$sd_order_size[i]   <- if (length(ord_pos)) sd(ord_pos)   else 0 # SD order size.
  
  setTxtProgressBar(pb_m, i)                      # Update progress bar.
}
close(pb_m)                                       # Close the progress bar.

Experimental_grid_table_manufacturer$total_manufacturer_cost <- manufacturer_full_summary$total_cost        # Attach totals to grid.
Experimental_grid_table_manufacturer$avg_cost_per_day       <- manufacturer_full_summary$avg_cost_per_day  # Attach avg/day to grid.
Experimental_grid_table_manufacturer$avg_cost_per_unit      <- manufacturer_full_summary$avg_cost_per_unit # Attach avg/unit to grid.

# -------------------------------
# Headline: best scenarios
# -------------------------------
best_R_idx <- which.min(retailer_full_summary$total_cost)     # Index of retailer scenario with lowest total cost.
best_Row   <- retailer_full_summary[best_R_idx, ]             # Extract that retailer scenario summary row.
cat("\n--- Best Retailer Scenario (FULL grid) ---\n"); print(best_Row)  # Print best retailer scenario.

best_M_idx <- which.min(manufacturer_full_summary$total_cost) # Index of manufacturer scenario with lowest total cost.
best_Mow   <- manufacturer_full_summary[best_M_idx, ]         # Extract that manufacturer scenario summary row.
cat("\n--- Best Manufacturer Scenario (FULL grid) ---\n"); print(best_Mow) # Print best manufacturer scenario.

# Keep key tables in workspace
retailer_results_full         <- Experimental_grid_table_retailer        # Save final retailer grid with metrics.
manufacturer_results_full     <- Experimental_grid_table_manufacturer    # Save final manufacturer grid with metrics.
retailer_full_summary_tbl     <- retailer_full_summary                   # Save retailer summaries table.
manufacturer_full_summary_tbl <- manufacturer_full_summary               # Save manufacturer summaries table.


#--------------------------------------------
#--------------------------------------------
# ANALYZED PLOTS
#--------------------------------------------
#--------------------------------------------

# -------------------------------
# Retailer: Avg daily cost vs lead time
# -------------------------------
agg_cost_retailer <- aggregate(avg_cost_per_day ~ lead_time, 
                               data = retailer_full_summary, 
                               FUN = mean)

plot(agg_cost_retailer$lead_time, agg_cost_retailer$avg_cost_per_day,
     type = "b", lwd = 2, pch = 19, col = "blue",
     xlab = "Lead Time (days)",
     ylab = "Average Daily Cost",
     main = "Dynamic strat. - Retailer: Avg Daily Cost vs Lead Time",
     xlim = c(1.5, max(agg_cost_retailer$lead_time)))

# -------------------------------
# Manufacturer: Avg daily cost vs lead time
# -------------------------------
agg_cost_manufacturer <- aggregate(avg_cost_per_day ~ lead_time, 
                                   data = manufacturer_full_summary, 
                                   FUN = mean)

plot(agg_cost_manufacturer$lead_time, agg_cost_manufacturer$avg_cost_per_day,
     type = "b", lwd = 2, pch = 19, col = "red",
     xlab = "Lead Time (days)",
     ylab = "Average Daily Cost",
     main = "Dynamic strat. - Manufacturer: Avg Daily Cost vs Lead Time")

# -------------------------------
# Manufacturer: cost breakdown by lead time
# -------------------------------
agg_m <- aggregate(cbind(setup_per_day, holding_per_day, backorder_per_day, avg_cost_per_day) ~ lead_time,
                   data = manufacturer_full_summary, FUN = mean, na.rm = TRUE)

# Compute shares
agg_m$setup_share     <- ifelse(agg_m$avg_cost_per_day > 0, agg_m$setup_per_day     / agg_m$avg_cost_per_day, NA)
agg_m$holding_share   <- ifelse(agg_m$avg_cost_per_day > 0, agg_m$holding_per_day   / agg_m$avg_cost_per_day, NA)
agg_m$backorder_share <- ifelse(agg_m$avg_cost_per_day > 0, agg_m$backorder_per_day / agg_m$avg_cost_per_day, NA)

# Pretty print (rounded)
print(round(agg_m[, c("lead_time",
                      "setup_per_day","holding_per_day","backorder_per_day",
                      "avg_cost_per_day",
                      "setup_share","holding_share","backorder_share")], 3), row.names = FALSE)

# -------------------------------
# Retailer: Cost components boxplots
# -------------------------------
retailer_combined <- data.frame(
  Cost = c(retailer_full_summary$setup_per_day + retailer_full_summary$holding_per_day,
           retailer_full_summary$backorder_per_day),
  Component = factor(rep(c("Setup+Holding", "Backorder"),
                         each = nrow(retailer_full_summary)),
                     levels = c("Setup+Holding", "Backorder")) # enforce order
)

boxplot(Cost ~ Component, data = retailer_combined,
        main = "Dynamic Strat. - Retailer: Cost Component Distribution",
        ylab = "Cost per Day",
        col = c("lightgreen", "pink"))

# -------------------------------
# Manufacturer: Cost components boxplots
# -------------------------------
manufacturer_combined <- data.frame(
  Cost = c(manufacturer_full_summary$setup_per_day + manufacturer_full_summary$holding_per_day,
           manufacturer_full_summary$backorder_per_day),
  Component = factor(rep(c("Setup+Holding", "Backorder"),
                         each = nrow(manufacturer_full_summary)),
                     levels = c("Setup+Holding", "Backorder")) # enforce order
)

boxplot(Cost ~ Component, data = manufacturer_combined,
        main = "Dynamic Strat. - Manufacturer: Cost Component Distribution",
        ylab = "Cost per Day",
        col = c("lightgreen", "pink"))

# -------------------------------
# Retailer: Avg cost vs setup cost
# -------------------------------
agg_setup_retailer <- aggregate(avg_cost_per_day ~ setup, 
                                data = retailer_full_summary, FUN = mean)

plot(agg_setup_retailer$setup, agg_setup_retailer$avg_cost_per_day,
     type = "b", lwd = 2, pch = 19, col = "blue",
     xlab = "Setup Cost per Order",
     ylab = "Average Daily SC Cost",
     main = "Dynamic strat. - Retailer: Avg Daily Cost vs Setup Cost")

# -------------------------------
# Manufacturer: Avg cost vs setup cost
# -------------------------------
agg_setup_manufacturer <- aggregate(avg_cost_per_day ~ setup, 
                                    data = manufacturer_full_summary, FUN = mean)

plot(agg_setup_manufacturer$setup, agg_setup_manufacturer$avg_cost_per_day,
     type = "b", lwd = 2, pch = 17, col = "red",
     xlab = "Setup Cost per Order",
     ylab = "Average Daily SC Cost",
     main = "Dynamic strat. - Manufacturer: Avg Daily Cost vs Setup Cost")

# -------------------------------
# Extract mean inventory levels
# -------------------------------
avg_inventory <- data.frame(
  Inventory = c(retailer_full_summary$mean_inventory,
                manufacturer_full_summary$mean_inventory),
  Echelon = factor(c(rep("Retailer", nrow(retailer_full_summary)),
                     rep("Manufacturer", nrow(manufacturer_full_summary))),
                   levels = c("Retailer", "Manufacturer")) # enforce order
)

# Boxplot
boxplot(Inventory ~ Echelon, data = avg_inventory,
        main = "Dynamic strat. - Average Inventory: Retailer VS Manufacturer",
        ylab = "Average Stock (units)",
        col = c("blue", "red"))

# -------------------------------
# Extract strictly positive orders
# -------------------------------
retailer_orders      <- unlist(lapply(retailer_full_summary_tbl$orders_count, function(x) x)) # safeguard
retailer_order_sizes <- unlist(lapply(retailer_full_summary_tbl$orders, function(x) x[x > 0]))

manufacturer_order_sizes <- unlist(lapply(manufacturer_full_summary_tbl$orders, function(x) x[x > 0]))

# -------------------------------
# Plot histograms side by side
# -------------------------------
par(mfrow = c(1, 2))  # 1 row, 2 columns

hist(retailer_order_sizes,
     breaks = 30, col = "blue", border = "white",
     main = "Dynamic Strat. - Retailer: Order Size Distribution",
     xlab = "Order Size (units)", ylab = "Frequency")

hist(manufacturer_order_sizes,
     breaks = 30, col = "red", border = "white",
     main = "Dynamic Strat. - Manufacturer: Order Size Distribution",
     xlab = "Order Size (units)", ylab = "Frequency")

par(mfrow = c(1, 1))  # reset to default

# -------------------------------
# Dot plot: Avg backorder costs Retailer vs Manufacturer (with values)
# -------------------------------
val_retailer <- mean(retailer_full_summary$backorder_per_day, na.rm = TRUE)
val_manufacturer <- mean(manufacturer_full_summary$backorder_per_day, na.rm = TRUE)

# Plot the dots
plot(1:2, c(val_retailer, val_manufacturer),
     pch = 19, col = c("blue", "red"), cex = 3,
     xaxt = "n", xlab = "",
     ylab = "Avg Backorder Cost per Day",
     main = "Dynamic Strat. - Avg Backorder Costs")
axis(1, at = 1:2, labels = c("Retailer", "Manufacturer"))

# Add numeric values beside both dots
text(x = 1, y = val_retailer,
     labels = round(val_retailer, 0),
     pos = 4, cex = 1, col = "black")

text(x = 2, y = val_manufacturer,
     labels = round(val_manufacturer, 0),
     pos = 4, cex = 1, col = "black")

# -------------------------------
# Retailer: Avg backorders vs lead time
# -------------------------------
agg_bo_retailer <- aggregate(backorder_per_day ~ lead_time,
                             data = retailer_full_summary, FUN = mean)

plot(agg_bo_retailer$lead_time, agg_bo_retailer$backorder_per_day,
     type = "b", pch = 19, lwd = 2, col = "blue",
     xlab = "Lead Time (days)",
     ylab = "Average Backorders per Day",
     main = "Dynamic strat. - Retailer: Avg Backorders vs Lead Time",
     xlim = c(1.5, max(agg_cost_retailer$lead_time)))

# -------------------------------
# Manufacturer: Avg backorders vs lead time
# -------------------------------
agg_bo_manufacturer <- aggregate(backorder_per_day ~ lead_time,
                                 data = manufacturer_full_summary, FUN = mean)

plot(agg_bo_manufacturer$lead_time, agg_bo_manufacturer$backorder_per_day,
     type = "b", pch = 17, lwd = 2, col = "red",
     xlab = "Lead Time (days)",
     ylab = "Average Backorders per Day",
     main = "Dynamic strat. - Manufacturer: Avg Backorders vs Lead Time")

# -------------------------------
# Total costs over 1 year (365 days)
# -------------------------------

days_year <- 365

# Average daily cost = mean of scenario averages
avg_daily_cost_retailer <- mean(retailer_full_summary$avg_cost_per_day)
avg_daily_cost_manufacturer <- mean(manufacturer_full_summary$avg_cost_per_day)

# Scale to 365 days
total_year_retailer <- avg_daily_cost_retailer * days_year
total_year_manufacturer <- avg_daily_cost_manufacturer * days_year
total_year_supply_chain <- total_year_retailer + total_year_manufacturer

# Print nicely
cat("Retailer Cost over 1 year:", round(total_year_retailer, 2), "\n")
cat("Manufacturer Cost over 1 year:", round(total_year_manufacturer, 2), "\n")
cat("Supply Chain Cost over 1 year:", round(total_year_supply_chain, 2), "\n")
