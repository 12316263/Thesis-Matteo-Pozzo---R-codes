# STATIC-DYNAMIC UNCERTAINTY STRATEGY

rm(list = ls())         # Remove all objects from the workspace to start clean
graphics.off()          # Close any open plotting devices

# -------------------------------------
# Experimental Grid parameters
# -------------------------------------
## --- Retailer ---
lead_time_retailer_vals <- 1:3                     # Candidate retailer inbound lead times (days)
setup_retailer_vals     <- 45:55                   # Candidate retailer fixed setup costs per order
hold_retailer_vals      <- seq(0.1, 0.4, by = 0.1) # Candidate retailer holding costs per unit per day
backorder_retailer_vals <- 2:5                     # Candidate retailer backorder penalty per unit per day

# Build retailer grid table with every combination of the parameters above
Experimental_grid_table_retailer <- expand.grid(
  lead_time_retailer = lead_time_retailer_vals,    # column: L_r
  setup_retailer     = setup_retailer_vals,        # column: K_r
  hold_retailer      = hold_retailer_vals,         # column: h_r
  backorder_retailer = backorder_retailer_vals,    # column: b_r
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE # keep table simple (no row-name attributes, no factors)
)

## --- Manufacturer ---
lead_time_manufacturer_vals <- 3:5                 # Candidate manufacturer inbound lead times (days)
setup_manufacturer_vals     <- 190:210             # Candidate manufacturer setup costs per order
hold_manufacturer_vals      <- c(0.3, 0.5, 0.7)    # Candidate manufacturer holding costs per unit per day
backorder_manufacturer_vals <- 1:3                 # Candidate manufacturer backorder penalties

# Build manufacturer grid table with every combination
Experimental_grid_table_manufacturer <- expand.grid(
  lead_time_manufacturer = lead_time_manufacturer_vals, # column: L_m
  setup_manufacturer     = setup_manufacturer_vals,     # column: K_m
  hold_manufacturer      = hold_manufacturer_vals,      # column: h_m
  backorder_manufacturer = backorder_manufacturer_vals, # column: b_m
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)

# -------------------------------
# Additional parameters
# -------------------------------
T_perf     <- 365       # Number of days to EVALUATE performance/costs on (reporting window)
max_cycle  <- 14        # Maximum replenishment cycle length the heuristic may choose

# -------------------------------
# Demand + service level for reports
# -------------------------------
set.seed(123)           # Fix RNG seed so demand realizations are reproducible
demand_mean <- 100      # Mean of end-customer daily demand
demand_sd   <- 20       # Standard deviation of end-customer daily demand
alpha       <- 0.95     # Service level used to form protective quantiles (for planning/reporting)

# === Helpers ===
per_day_q <- function(x, alpha) {
  mu <- mean(x); sdv <- sd(x)                        # Empirical mean and SD of a series
  if (!is.finite(sdv) || sdv == 0) sdv <- sqrt(max(mu, 1))  # Fallback SD if flat/degenerate series
  q <- qnorm(alpha, mean = mu, sd = sdv)             # Normal quantile at level alpha
  max(0, q)                                          # Truncate at zero (no negative demand)
}

# AH objective: Holding + Backorder per day (setup added outside)
calc_cycle_cost_AH <- function(a, S, holding_cost, backorder_cost, q_per_day) {
  cost <- 0                                          # Accumulator for cycle cost
  for (k in 1:a) {                                   # For each day k in the cycle (1..a)
    cum_dem <- q_per_day * k                         # Approx cumulative demand by day k
    on_hand <- max(S - cum_dem, 0)                   # Approx end-of-day on-hand
    backord <- max(cum_dem - S, 0)                   # Approx end-of-day backlog
    cost <- cost + holding_cost * on_hand +          # Holding cost contribution
      backorder_cost * backord                       # Backorder penalty contribution
  }
  cost                                               # Return heuristic cycle cost (no setup here)
}

# Reporting cycle cost (setup counted once, holding only) — clipped later to window
calc_cycle_cost <- function(length, base_stock, setup_cost, holding_cost, demand_estimate) {
  cost <- setup_cost                                  # Charge setup once per (intersecting) cycle
  for (k in 1:length) {                               # For the clipped length of the cycle
    on_hand <- max(base_stock - demand_estimate * k, 0) # Approx end-of-day on-hand in day k
    cost <- cost + holding_cost * on_hand             # Accumulate holding
  }
  cost                                                # Return setup + holding for the clipped cycle
}

# AH planner (protects a+L, objective penalizes shortages)
approximation_heuristic <- function(demand, setup, holding, backorder,
                                    lead_time = 0, max_cycle = 14, alpha_local = alpha) {
  N <- length(demand)                                 # Heuristic planning horizon length
  repl_days <- c(); base_stocks <- c(); t <- N        # Outputs (release days & S) + backward cursor
  q_local <- per_day_q(demand, alpha_local)           # Protective daily quantile from this demand
  while (t > 0) {                                     # Allocate cycles backwards until the start
    best_cost <- Inf; best_a <- 1; best_S <- 0        # Best (cycle length a, base-stock S) so far
    max_a <- min(max_cycle, t)                        # Limit a to remaining days and max_cycle
    for (a in 1:max_a) {                              # Try each feasible cycle length
      start <- t - a + 1                              # Calendar start day of the candidate cycle
      if ((start - lead_time) < 1) next               # If release day < 1, skip this a
      base_start <- ceiling(q_local * (a + lead_time) + q_local) # S lower bound: protect a+L plus bump
      max_y      <- ceiling(base_start * 1.5)         # S upper bound: +50% headroom
      for (S in seq(base_start, max_y, by = 5)) {     # Grid search S in steps of 5 units
        hbo <- calc_cycle_cost_AH(a, S, holding, backorder, q_local) # Holding+BO cost for this (a,S)
        cost <- setup + hbo                           # Add setup once per cycle
        if (cost < best_cost) {                       # Keep the best (a,S)
          best_cost <- cost; best_a <- a; best_S <- S
        }
      }
    }
    order_day <- t - best_a + 1 - lead_time           # Calendar release day for the chosen cycle
    if (order_day >= 1) {                             # Record only valid release days
      repl_days <- c(order_day, repl_days)            # Prepend to keep chronological order
      base_stocks <- c(best_S, base_stocks)           # Prepend matching base-stock
    }
    t <- t - best_a                                   # Jump left by the selected cycle length
  }
  list(repl_days = repl_days, base_stocks = base_stocks) # Return planned releases and S
}

# Inventory simulation with backlog (orders raise INVENTORY POSITION to S)
simulate_inventory <- function(demand, repl_days, base_stocks, lead_time, backorder_cost_per_unit_day,
                               eval_T) {
  n <- length(demand)                                 # Simulation horizon length
  inv <- rep(0, n); bklg <- rep(0, n); orders <- rep(0, n) # Traces: on-hand, backlog, releases
  stock <- 0; backlog <- 0                            # Current on-hand and backlog
  pipeline <- if (lead_time > 0) rep(0, lead_time) else numeric(0) # Pipeline vector of length L
  repl_index <- 1; backorder_cost_total <- 0          # Next release pointer; BO cost accumulator
  for (t in 1:n) {                                    # Simulate day by day
    if (lead_time > 0) {                              # Receive pipeline arriving today
      stock <- stock + pipeline[1]
      pipeline <- c(pipeline[-1], 0)                  # Shift pipeline forward
    }
    if (repl_index <= length(repl_days) && t == repl_days[repl_index]) { # If today is a planned release
      inv_pos  <- stock + (if (length(pipeline)) sum(pipeline) else 0) - backlog # Inventory position
      target_S <- base_stocks[repl_index]            # Base-stock target for this cycle
      order_qty <- max(target_S - inv_pos, 0)        # Order up to S in IP space (nonnegative)
      if (lead_time > 0) pipeline[lead_time] <- pipeline[lead_time] + order_qty else stock <- stock + order_qty
      orders[t] <- order_qty; repl_index <- repl_index + 1 # Record order and advance cycle pointer
    }
    total_required <- backlog + demand[t]             # Demand to serve = old backlog + new demand
    served <- min(stock, total_required)              # Ship what we can
    stock <- stock - served                           # Decrement on-hand by shipped amount
    backlog <- max(total_required - served, 0)        # Unserved becomes new backlog
    inv[t] <- stock; bklg[t] <- backlog               # Save end-of-day states
    if (t <= eval_T)                                  # Only accumulate BO penalties in performance window
      backorder_cost_total <- backorder_cost_total + backorder_cost_per_unit_day * backlog
  }
  list(inventory = inv, backlog = bklg, orders = orders, backorder_cost_total = backorder_cost_total)
}

# Reporting cost within window (clip cycles to window; setup once per intersecting cycle)
calc_total_cost <- function(repl_days, base_stocks, setup, holding, demand_estimate,
                            eval_T, horizon_T) {
  total_cost <- 0                                     # Accumulator for setup+holding reporting cost
  cycle_starts <- repl_days                           # Cycle starts = release days
  cycle_ends   <- c(repl_days[-1] - 1, horizon_T)     # Cycle ends = day before next start; last ends at horizon
  for (i in seq_along(cycle_starts)) {                # For each cycle
    start <- cycle_starts[i]; end <- cycle_ends[i]    # Calendar bounds of the cycle
    clip_start <- max(start, 1)                       # Clip lower bound to window start
    clip_end   <- min(end, eval_T)                    # Clip upper bound to window end
    if (clip_end >= clip_start) {                     # If cycle intersects window
      clip_len <- clip_end - clip_start + 1           # Days in the clipped portion
      total_cost <- total_cost +                      # Add setup+holding for clipped portion
        calc_cycle_cost(clip_len, base_stocks[i], setup, holding, demand_estimate)
    }
  }
  total_cost                                          # Return windowed setup+holding cost
}

# -------------------------------------
# Create ONE long demand series to reuse across scenarios
# (long enough for max retailer L and max cycle)
# -------------------------------------
T_base_ext <- T_perf + max(lead_time_retailer_vals) + max_cycle # Minimum extended horizon needed
demand_retailer_ext_base <- pmax(round(rnorm(T_base_ext, mean = demand_mean, sd = demand_sd)), 0) # One reusable demand path
q_customer <- per_day_q(demand_retailer_ext_base[1:T_perf], alpha)  # Protective quantile used for surrogate demand

# ============================================================
# RETAILER PRESCREEN (plan + sim + costs in window)
# ============================================================
n_r <- nrow(Experimental_grid_table_retailer)        # Number of retailer scenarios to evaluate
pb_r <- txtProgressBar(min = 0, max = n_r, style = 3) # Progress bar

# Preallocate compact prescreen results (one row per retailer scenario)
retailer_prescreen <- data.frame(
  row_id              = integer(n_r),                # Scenario index
  lead_time_retailer  = integer(n_r),                # L_r used
  setup_retailer      = numeric(n_r),                # K_r used
  hold_retailer       = numeric(n_r),                # h_r used
  backorder_retailer  = numeric(n_r),                # b_r used
  retailer_cost_SH    = numeric(n_r),                # Setup+holding within window
  retailer_cost_BO    = numeric(n_r),                # Backorder penalties within window
  retailer_cost       = numeric(n_r),                # Total = SH + BO
  retailer_orders_cnt = integer(n_r),                # Count of positive order releases (diagnostic)
  stringsAsFactors    = FALSE
)

for (ri in seq_len(n_r)) {                           # Loop over retailer grid scenarios
  pr <- Experimental_grid_table_retailer[ri, ]       # Parameter row (L_r, K_r, h_r, b_r)
  T_ext_retailer <- T_perf + pr$lead_time_retailer + max_cycle    # Extend horizon to allow tail cycles
  demand_retailer_ext <- demand_retailer_ext_base[1:T_ext_retailer] # Clip reusable demand to this horizon
  
  # Plan retailer cycles (AH) on the extended demand
  plan_r <- approximation_heuristic(
    demand    = demand_retailer_ext,
    setup     = pr$setup_retailer,
    holding   = pr$hold_retailer,
    backorder = pr$backorder_retailer,
    lead_time = pr$lead_time_retailer,
    max_cycle = max_cycle
  )
  
  # Simulate inventory with backlog; accumulate BO penalties only within T_perf
  sim_r <- simulate_inventory(
    demand      = demand_retailer_ext,
    repl_days   = plan_r$repl_days,
    base_stocks = plan_r$base_stocks,
    lead_time   = pr$lead_time_retailer,
    backorder_cost_per_unit_day = pr$backorder_retailer,
    eval_T      = T_perf
  )
  
  # Compute setup+holding cost for cycles clipped to the reporting window
  ret_q_alpha <- per_day_q(demand_retailer_ext[1:T_perf], alpha) # Reporting demand proxy
  cost_r_SH <- calc_total_cost(
    repl_days       = plan_r$repl_days,
    base_stocks     = plan_r$base_stocks,
    setup           = pr$setup_retailer,
    holding         = pr$hold_retailer,
    demand_estimate = ret_q_alpha,
    eval_T          = T_perf,
    horizon_T       = T_ext_retailer
  )
  cost_r_BO <- sim_r$backorder_cost_total            # Backorder penalties within window
  cost_r    <- cost_r_SH + cost_r_BO                 # Total retailer cost
  
  # Store prescreen results for this scenario
  retailer_prescreen[ri, ] <- list(
    ri, pr$lead_time_retailer, pr$setup_retailer, pr$hold_retailer, pr$backorder_retailer,
    cost_r_SH, cost_r_BO, cost_r, sum(sim_r$orders[1:T_perf] > 0)
  )
  setTxtProgressBar(pb_r, ri)                        # Update progress bar
}
close(pb_r)                                          # Close progress bar

ord_r <- order(retailer_prescreen$retailer_cost)     # Rank retailer scenarios by total cost (asc)
K_retailer <- min(20, n_r)                           # Keep top-K retailer finalists -> you can change this
retailer_top <- retailer_prescreen[head(ord_r, K_retailer), ]

# ============================================================
# MANUFACTURER PRESCREEN (surrogate demand + BO)
# ============================================================
# Surrogate manufacturer demand: flat series at protective customer quantile (fast scoring)
T_sur_man <- T_perf + max(lead_time_manufacturer_vals) + max_cycle # Sufficient horizon for manufacturer
man_surrogate_demand <- rep(q_customer, T_sur_man)   # Constant-demand surrogate

n_m <- nrow(Experimental_grid_table_manufacturer)    # Number of manufacturer scenarios
pb_m <- txtProgressBar(min = 0, max = n_m, style = 3)

# Preallocate manufacturer prescreen table
manufacturer_prescreen <- data.frame(
  row_id                  = integer(n_m),            # Scenario index
  lead_time_manufacturer  = integer(n_m),            # L_m used
  setup_manufacturer      = numeric(n_m),            # K_m used
  hold_manufacturer       = numeric(n_m),            # h_m used
  backorder_manufacturer  = numeric(n_m),            # b_m used
  manufacturer_cost_SH    = numeric(n_m),            # Setup+holding (window)
  manufacturer_cost_BO    = numeric(n_m),            # Backorder penalties (window)
  manufacturer_cost       = numeric(n_m),            # Total = SH + BO
  stringsAsFactors        = FALSE
)

for (mi in seq_len(n_m)) {                           # Loop over manufacturer grid scenarios
  pm <- Experimental_grid_table_manufacturer[mi, ]   # Parameter row (L_m, K_m, h_m, b_m)
  
  # Plan manufacturer cycles on surrogate demand
  plan_m <- approximation_heuristic(
    demand    = man_surrogate_demand,
    setup     = pm$setup_manufacturer,
    holding   = pm$hold_manufacturer,
    backorder = pm$backorder_manufacturer,
    lead_time = pm$lead_time_manufacturer,
    max_cycle = max_cycle
  )
  
  # Simulate on surrogate to capture BO cost contributions
  sim_m <- simulate_inventory(
    demand      = man_surrogate_demand,
    repl_days   = plan_m$repl_days,
    base_stocks = plan_m$base_stocks,
    lead_time   = pm$lead_time_manufacturer,
    backorder_cost_per_unit_day = pm$backorder_manufacturer,
    eval_T      = T_perf
  )
  
  # Compute setup+holding cost for cycles clipped to the reporting window
  man_q_alpha <- per_day_q(man_surrogate_demand[1:T_perf], alpha)
  cost_m_SH <- calc_total_cost(
    repl_days       = plan_m$repl_days,
    base_stocks     = plan_m$base_stocks,
    setup           = pm$setup_manufacturer,
    holding         = pm$hold_manufacturer,
    demand_estimate = man_q_alpha,
    eval_T          = T_perf,
    horizon_T       = T_sur_man
  )
  cost_m_BO <- sim_m$backorder_cost_total           # Backorder penalties within window
  cost_m    <- cost_m_SH + cost_m_BO                # Total manufacturer cost
  
  # Store prescreen result
  manufacturer_prescreen[mi, ] <- list(
    mi, pm$lead_time_manufacturer, pm$setup_manufacturer, pm$hold_manufacturer, pm$backorder_manufacturer,
    cost_m_SH, cost_m_BO, cost_m
  )
  setTxtProgressBar(pb_m, mi)                       # Update progress bar
}
close(pb_m)

ord_m <- order(manufacturer_prescreen$manufacturer_cost) # Rank manufacturer scenarios
K_manufacturer <- min(20, n_m)                      # Keep top-K manufacturer finalists -> you can change this
manufacturer_top <- manufacturer_prescreen[head(ord_m, K_manufacturer), ]

# ============================================================
# JOINT REFINEMENT (top K_r × K_m on real coupled demand)
# ============================================================
K_r <- nrow(retailer_top); K_m <- nrow(manufacturer_top) # Sizes of finalist sets
run_n <- K_r * K_m                                      # Total joint scenarios to run
cat("Refining", K_r, "retailer ×", K_m, "manufacturer =", run_n, "scenarios\n")

# Preallocate joint summary (one row per retailer×manufacturer finalist pair)
full_grid_summary <- data.frame(
  row_id                    = integer(run_n),        # Joint row index
  # Retailer params kept for the row:
  lead_time_retailer        = integer(run_n),
  setup_retailer            = numeric(run_n),
  hold_retailer             = numeric(run_n),
  backorder_retailer        = numeric(run_n),
  # Manufacturer params kept for the row:
  lead_time_manufacturer    = integer(run_n),
  setup_manufacturer        = numeric(run_n),
  hold_manufacturer         = numeric(run_n),
  backorder_manufacturer    = numeric(run_n),
  # Windowed costs and diagnostics:
  retailer_cost_SH          = numeric(run_n),
  retailer_cost_BO          = numeric(run_n),
  retailer_cost             = numeric(run_n),
  manufacturer_cost_SH      = numeric(run_n),
  manufacturer_cost_BO      = numeric(run_n),
  manufacturer_cost         = numeric(run_n),
  total_cost                = numeric(run_n),
  avg_daily_cost            = numeric(run_n),
  retailer_orders_count     = integer(run_n),
  manufacturer_orders_count = integer(run_n),
  stringsAsFactors = FALSE
)

pb_j <- txtProgressBar(min = 0, max = run_n, style = 3); k <- 0L  # Progress bar + writing index

for (i in seq_len(K_r)) {                            # Loop retailer finalists
  lt_r  <- retailer_top$lead_time_retailer[i]        # Retailer LT for this finalist
  set_r <- retailer_top$setup_retailer[i]            # Retailer setup cost
  h_r   <- retailer_top$hold_retailer[i]             # Retailer holding cost
  bo_r  <- retailer_top$backorder_retailer[i]        # Retailer backorder penalty
  
  T_ext_retailer_i <- T_perf + lt_r + max_cycle      # Extended horizon for this LT
  demand_retailer_ext_i <- demand_retailer_ext_base[1:T_ext_retailer_i] # Demand slice
  
  # Plan & simulate retailer on real demand
  plan_r <- approximation_heuristic(demand_retailer_ext_i, set_r, h_r, bo_r, lt_r, max_cycle)
  sim_r  <- simulate_inventory(demand_retailer_ext_i, plan_r$repl_days, plan_r$base_stocks,
                               lt_r, bo_r, T_perf)
  ret_q_alpha_i <- per_day_q(demand_retailer_ext_i[1:T_perf], alpha)  # Reporting proxy
  cost_r_SH <- calc_total_cost(plan_r$repl_days, plan_r$base_stocks, set_r, h_r,
                               ret_q_alpha_i, T_perf, T_ext_retailer_i)
  cost_r_BO <- sim_r$backorder_cost_total
  cost_r    <- cost_r_SH + cost_r_BO
  
  r_orders <- sim_r$orders                                 # Retailer orders = manufacturer demand
  r_orders_cnt <- sum(r_orders[1:T_perf] > 0)              # Diagnostic: number of retailer orders
  
  for (j in seq_len(K_m)) {                                # Loop manufacturer finalists
    k <- k + 1L                                            # Advance joint row index
    lt_m  <- manufacturer_top$lead_time_manufacturer[j]    # Manufacturer LT
    set_m <- manufacturer_top$setup_manufacturer[j]        # Manufacturer setup
    h_m   <- manufacturer_top$hold_manufacturer[j]         # Manufacturer holding
    bo_m  <- manufacturer_top$backorder_manufacturer[j]    # Manufacturer backorder penalty
    
    T_ext_m_j <- length(r_orders)                          # Manufacturer horizon = length of orders
    # Plan & simulate manufacturer using retailer order stream
    plan_m <- approximation_heuristic(r_orders, set_m, h_m, bo_m, lt_m, max_cycle)
    sim_m  <- simulate_inventory(r_orders, plan_m$repl_days, plan_m$base_stocks,
                                 lt_m, bo_m, T_perf)
    man_q_alpha_j <- per_day_q(r_orders[1:min(T_perf, length(r_orders))], alpha) # Reporting proxy
    cost_m_SH <- calc_total_cost(plan_m$repl_days, plan_m$base_stocks, set_m, h_m,
                                 man_q_alpha_j, T_perf, T_ext_m_j)
    cost_m_BO <- sim_m$backorder_cost_total
    cost_m    <- cost_m_SH + cost_m_BO
    
    total <- cost_r + cost_m                               # Total supply chain cost (windowed)
    
    # Write the joint row into the summary table
    full_grid_summary[k, ] <- list(
      k,
      lt_r, set_r, h_r, bo_r,
      lt_m, set_m, h_m, bo_m,
      cost_r_SH, cost_r_BO, cost_r,
      cost_m_SH, cost_m_BO, cost_m,
      total, total / T_perf,
      r_orders_cnt, sum(sim_m$orders[1:T_perf] > 0)
    )
    setTxtProgressBar(pb_j, k)                             # Update progress bar
  }
}
close(pb_j)                                                # Close progress bar

# Rank and show best scenarios (ascending avg_daily_cost)
full_grid_summary <- full_grid_summary[order(full_grid_summary$avg_daily_cost), ] # Sort by performance
cat("\n=== Top 10 configurations by avg_daily_cost (windowed; with BO) ===\n")
print(head(full_grid_summary, 10))                         # Display top 10 joint configurations

#--------------------------------------------
#--------------------------------------------
# ANALYZED PLOTS
#--------------------------------------------
#--------------------------------------------

# ------------------------------------------------------------
# Avg Daily Cost vs Lead Time (Retailer)
# ------------------------------------------------------------
T_perf <- 365

avg_r_all <- aggregate(retailer_cost ~ lead_time_retailer,
                       data = retailer_prescreen, mean, na.rm = TRUE)

plot(avg_r_all$lead_time_retailer,
     avg_r_all$retailer_cost / T_perf,
     type = "b", pch = 19, col = "blue", lwd = 2,
     xlab = "Retailer Lead Time (days)",
     ylab = "Avg Daily Cost",
     main = "Static-Dynamic strat. - Retailer: Avg Daily Cost vs Lead Time")
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Avg Daily Cost vs Lead Time (Manufacturer)
# ------------------------------------------------------------
avg_m_all <- aggregate(manufacturer_cost ~ lead_time_manufacturer,
                       data = manufacturer_prescreen, mean, na.rm = TRUE)

plot(avg_m_all$lead_time_manufacturer,
     avg_m_all$manufacturer_cost / T_perf,
     type = "b", pch = 19, col = "red", lwd = 2,
     xlab = "Manufacturer Lead Time (days)",
     ylab = "Avg Daily Cost",
     main = "Static-Dynamic strat. - Manufacturer: Avg Daily Cost vs Lead Time")
grid(nx = NA, ny = NULL)

# ---------------------------
# Retailer: boxplots of cost components
# ---------------------------
T_perf <- 365

# --- Retailer per-day costs ---
cost_SH_retailer <- retailer_prescreen$retailer_cost_SH / T_perf
cost_BO_retailer <- retailer_prescreen$retailer_cost_BO / T_perf

dat_r <- data.frame(
  Setup_Holding = cost_SH_retailer,
  Backorder     = cost_BO_retailer
)

boxplot(dat_r,
        names = c("Setup+Holding", "Backorder"),
        main  = "Static-Dynamic Strat. - Retailer: Cost Component Distribution",
        ylab  = "Cost per day",
        col   = c("lightgreen", "pink"),
        notch = TRUE, las = 1)
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Manufacturer: Cost Components Distribution (per day)
# ------------------------------------------------------------
T_perf <- 365

# Compute setup cost = setup_manufacturer * number of orders
setup_cost_man <- full_grid_summary$setup_manufacturer * full_grid_summary$manufacturer_orders_count

# Holding cost = SH - setup
holding_cost_man <- full_grid_summary$manufacturer_cost_SH - setup_cost_man

# Backorder cost = already given
backorder_cost_man <- full_grid_summary$manufacturer_cost_BO

# Convert to cost per day
cost_SH_man <- (setup_cost_man + holding_cost_man) / T_perf
cost_BO_man <- backorder_cost_man / T_perf

# Combine into data.frame
dat_m <- data.frame(
  Setup_Holding = cost_SH_man,
  Backorder     = cost_BO_man
)

# Draw boxplot
boxplot(dat_m,
        names = c("Setup+Holding", "Backorder"),
        main = "Static-Dynamic - Manufacturer: Cost Component Distribution",
        ylab = "Cost per day",
        col  = c("lightgreen", "pink"),
        notch = TRUE, las = 1)
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Retailer: Avg Daily SC Cost vs Retailer Setup Cost
# ------------------------------------------------------------
T_perf <- 365

avg_cost_by_Sr <- tapply(full_grid_summary$total_cost / T_perf,
                         full_grid_summary$setup_retailer, mean, na.rm = TRUE)

plot(as.numeric(names(avg_cost_by_Sr)), avg_cost_by_Sr,
     type = "b", lwd = 2, pch = 19, col = "blue",
     xlab = "Setup Cost per Order",
     ylab = "Average Daily SC Cost",
     main = "Static-Dynamic - Retailer: Avg Daily Cost vs Setup Cost",
     ylim = range(avg_cost_by_Sr, na.rm = TRUE))
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Manufacturer: Avg Daily SC Cost vs Manufacturer Setup Cost
# ------------------------------------------------------------
avg_cost_by_Sm <- tapply(full_grid_summary$total_cost / T_perf,
                         full_grid_summary$setup_manufacturer, mean, na.rm = TRUE)

plot(as.numeric(names(avg_cost_by_Sm)), avg_cost_by_Sm,
     type = "b", lwd = 2, pch = 17, col = "red",
     xlab = "Setup Cost per Order",
     ylab = "Average Daily SC Cost",
     main = "Static-Dynamic - Manufacturer: Avg Daily Cost vs Setup Cost",
     ylim = range(avg_cost_by_Sm, na.rm = TRUE))
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Average Inventory Comparison: Retailer vs Manufacturer
# ------------------------------------------------------------
T_perf <- 365

# --- Retailer ---
setup_cost_retailer <- full_grid_summary$setup_retailer * full_grid_summary$retailer_orders_count
holding_part_retailer <- full_grid_summary$retailer_cost_SH - setup_cost_retailer
avg_inv_retailer <- holding_part_retailer / (full_grid_summary$hold_retailer * T_perf)

# --- Manufacturer ---
setup_cost_man <- full_grid_summary$setup_manufacturer * full_grid_summary$manufacturer_orders_count
holding_part_man <- full_grid_summary$manufacturer_cost_SH - setup_cost_man
avg_inv_manufacturer <- holding_part_man / (full_grid_summary$hold_manufacturer * T_perf)

# Side-by-side boxplot
boxplot(list(Retailer = avg_inv_retailer,
             Manufacturer = avg_inv_manufacturer),
        main = "Static-Dynamic Strat. - Average Inventory: Retailer vs Manufacturer",
        ylab = "Average Stock (units)",
        col = c("blue", "red")) 
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Compute Average Inventory values for Retailer & Manufacturer
# ------------------------------------------------------------
T_perf <- 365

# --- Retailer ---
setup_cost_retailer <- full_grid_summary$setup_retailer * full_grid_summary$retailer_orders_count
holding_part_retailer <- full_grid_summary$retailer_cost_SH - setup_cost_retailer
avg_inv_retailer <- holding_part_retailer / (full_grid_summary$hold_retailer * T_perf)

# --- Manufacturer ---
setup_cost_man <- full_grid_summary$setup_manufacturer * full_grid_summary$manufacturer_orders_count
holding_part_man <- full_grid_summary$manufacturer_cost_SH - setup_cost_man
avg_inv_manufacturer <- holding_part_man / (full_grid_summary$hold_manufacturer * T_perf)

cat("Retailer average inventory across scenarios:\n")
print(summary(avg_inv_retailer))

cat("\nManufacturer average inventory across scenarios:\n")
print(summary(avg_inv_manufacturer))

# ------------------------------------------------------------
# Histogram of Average Order Sizes (Retailer vs Manufacturer)
# ------------------------------------------------------------
T_perf <- 365
demand_mean <- 100
total_demand <- demand_mean * T_perf   # 36,500 units

# Compute average order size
avg_order_size_retailer <- total_demand / full_grid_summary$retailer_orders_count
avg_order_size_manufacturer <- total_demand / full_grid_summary$manufacturer_orders_count

# Set up plotting area (2 histograms side by side)
par(mfrow = c(1, 2))

# Retailer histogram
hist(avg_order_size_retailer,
     breaks = 20,
     col = "blue", border = "white",
     main = "Static-Dynamic Strat. - Retailer: Order Size Distribution",
     xlab = "Order Size (units)",
     ylab = "Frequency")

# Manufacturer histogram
hist(avg_order_size_manufacturer,
     breaks = 20,
     col = "red", border = "white",
     main = "Static-Dynamic Strat. - Manufacturer: Order Size Distribution",
     xlab = "Order Size (units)",
     ylab = "Frequency")

# Reset layout
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# Barplot: Average Backorder Cost (All Scenarios)
# ------------------------------------------------------------
T_perf <- 365

# --- Retailer ---
avg_BO_retailer <- mean(retailer_prescreen$retailer_cost_BO / T_perf, na.rm = TRUE)

barplot(avg_BO_retailer,
        names.arg = "Retailer",
        col = "blue",
        main = "Static-Dynamic Strat. - Retailer: Avg Backorder Costs Throughout All Scenarios",
        ylab = "Avg Backorder Cost per Day",
        ylim = c(0, max(avg_BO_retailer, 1.1 * avg_BO_retailer)))

# --- Manufacturer ---
avg_BO_manufacturer <- mean(manufacturer_prescreen$manufacturer_cost_BO / T_perf, na.rm = TRUE)

barplot(avg_BO_manufacturer,
        names.arg = "Manufacturer",
        col = "red",
        main = "Static-Dynamic Strat. - Manufacturer: Avg Backorder Costs Throughout All Scenarios",
        ylab = "Avg Backorder Cost per Day",
        ylim = c(0, max(avg_BO_manufacturer, 1.1 * avg_BO_manufacturer)))

# ------------------------------------------------------------
# Line plot: Average Backorders vs Lead Time
# ------------------------------------------------------------
T_perf <- 365

# --- Retailer ---
avg_backlog_retailer <- retailer_prescreen$retailer_cost_BO /
  (retailer_prescreen$backorder_retailer * T_perf)

ret_backlog_vs_LT <- aggregate(avg_backlog_retailer ~ lead_time_retailer,
                               data = retailer_prescreen, mean, na.rm = TRUE)

plot(ret_backlog_vs_LT$lead_time_retailer,
     ret_backlog_vs_LT$avg_backlog_retailer,
     type = "b", pch = 19, col = "blue", lwd = 2,
     xlab = "Lead Time (days)",
     ylab = "Average Backorders per Day",
     main = "Static-Dynamic strat. - Retailer: Avg Backorders vs Lead Time")
grid(nx = NA, ny = NULL)

# --- Manufacturer ---
avg_backlog_manufacturer <- manufacturer_prescreen$manufacturer_cost_BO /
  (manufacturer_prescreen$backorder_manufacturer * T_perf)

man_backlog_vs_LT <- aggregate(avg_backlog_manufacturer ~ lead_time_manufacturer,
                               data = manufacturer_prescreen, mean, na.rm = TRUE)

plot(man_backlog_vs_LT$lead_time_manufacturer,
     man_backlog_vs_LT$avg_backlog_manufacturer,
     type = "b", pch = 17, col = "red", lwd = 2,
     xlab = "Lead Time (days)",
     ylab = "Average Backorders per Day",
     main = "Static-Dynamic strat. - Manufacturer: Avg Backorders vs Lead Time")
grid(nx = NA, ny = NULL)

# ------------------------------------------------------------
# Average 1-Year Costs (365 days) across all scenarios
# ------------------------------------------------------------

avg_retailer_year <- mean(full_grid_summary$retailer_cost, na.rm = TRUE)
avg_manufacturer_year <- mean(full_grid_summary$manufacturer_cost, na.rm = TRUE)
avg_supply_chain_year <- mean(full_grid_summary$total_cost, na.rm = TRUE)

cat("Average Retailer Cost over 1 year:", avg_retailer_year, "\n")
cat("Average Manufacturer Cost over 1 year:", avg_manufacturer_year, "\n")
cat("Average Supply Chain Cost over 1 year:", avg_supply_chain_year, "\n")

