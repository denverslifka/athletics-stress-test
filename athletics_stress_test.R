# =============================================================================
# College Athletics Stress Test — Step 1: data load + distress ratios
# Denver Slifka
#
# Data: Knight-Newhouse College Athletics Database (knightnewhousedata.org)
#   - Public NCAA Division I schools, FY2005 onward, from NCAA Financial Reports
#   - Free download after a short registration; non-commercial use only and the
#     database MUST be credited ("Source: Knight-Newhouse College Athletics Database")
#
# How to use:
#   1. Custom Reports -> Data (Custom) -> variables, all institutions, years ->
#      Generate Report -> download. Save it in data/ and set KNAD_PATH below.
#   2. Run the script. If the file isn't there, it runs in DEMO mode on
#      synthetic data so you can check the pipeline end to end.
#   3. On the first real run, check the "Column mapping" printout. If a field
#      isn't matched, add its exact column name to COLUMN_OVERRIDES.
#
# Outputs (in output/):
#   ratios_panel.csv    every school-year with all ratios
#   stress_latest.csv   latest year, composite stress score + rank
#   shock_results.csv   $21.5M revenue-sharing shock + scenarios
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(readr)
  library(tidyr)
})

# ---- Config -----------------------------------------------------------------
KNAD_PATH        <- "data/Custom_Reporting_Results_Data.xlsx"   # .xlsx or .csv
OUT_DIR          <- "output"
REV_SHARE_CAP    <- 21.5e6     # House settlement revenue-sharing cap (FY2026)

# Share of the cap each tier is assumed to pay. These are YOUR assumptions:
# not every school opted in, and many outside the Power 4 pay well under the cap.
# Set them from reporting you trust and say so in the write-up.
REV_SHARE_BY_TIER <- c("Power 4" = 1.00, "Group of 5" = 0.50, "FCS / other" = 0.10)
POWER4_CONFS <- c("SEC", "Southeastern", "Big Ten", "Big 12", "ACC", "Atlantic Coast")
TREND_START_YEAR <- 2014       # window for expense-vs-revenue growth
TREND_END_YEAR   <- NA         # NA = latest year in the data

# Scenario shocks applied on top of revenue sharing (share of the line item lost)
SCENARIOS <- tibble::tribble(
  ~scenario,                      ~media_cut, ~donor_cut,
  "Rev share only",                0.00,       0.00,
  "Rev share + media -15%",        0.15,       0.00,
  "Rev share + donations -20%",    0.00,       0.20,
  "Rev share + both",              0.15,       0.20
)

# If auto-detect misses a field, put the exact column name here, e.g.
# COLUMN_OVERRIDES <- c(total_revenue = "Total Revenues")
COLUMN_OVERRIDES <- c()

# ---- Field patterns (matched against snake_case column names) --------------
# First regex that matches wins. Required fields stop the script if missing.
FIELD_PATTERNS <- list(
  school          = c("^data$", "^(institution|school|ipeds_name|name)$", "institution_name", "school_name"),
  year            = c("^(year|fiscal_year|fy)$", "fiscal_year"),
  conference      = c("fbs_conference", "^conference$", "^conference_name"),
  subdivision     = c("subdivision", "fbs_fcs", "division"),
  total_revenue   = c("^total_revenue", "grand_total_revenue", "total_athletics_revenue", "^revenue_total"),
  total_expenses  = c("^total_expense", "total_athletics_spending", "total_spending", "grand_total_expense"),
  allocated       = c("institutional_funding.*student_fees", "institutional.*government.*student",
                      "total_allocated", "allocated_revenue"),
  inst_support    = c("^institutional_support", "direct_institutional_support", "institutional_funding$"),
  student_fees    = c("^student_fees"),
  govt_support    = c("government_support", "state_.*support"),
  media_rights    = c("media_rights"),   # KNAD combines conference/NCAA distributions + media + bowls
  ncaa_conf_dist  = c("ncaa.*conference.*distribution", "conference_distribution", "ncaa_distribution"),
  donations       = c("^donor_contributions", "contributions", "donations"),
  debt_service    = c("annual_debt_service", "debt_service_leases", "facilities.*debt_service", "debt_service"),
  total_debt      = c("athletics_related_debt", "total_athletics?_debt", "debt_outstanding", "^total_debt"),
  rev_share_actual = c("nil_revenue_share", "revenue_share"),
  coach_comp      = c("^coach(es|ing)?_compensation", "compensation_for_all_coaches", "coaches_comp"),
  staff_comp      = c("non_coaching.*staff", "support_staff.*compensation", "administrative_compensation")
)
REQUIRED <- c("school", "year", "total_revenue", "total_expenses")

# ---- Helpers ---------------------------------------------------------------
to_snake <- function(x) {
  x <- tolower(x)
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

parse_money <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[$,\\s]", "", as.character(x), perl = TRUE)
  x <- sub("^\\((.*)\\)$", "-\\1", x)          # (123) -> -123
  suppressWarnings(as.numeric(x))
}

safe_div <- function(a, b) ifelse(is.na(b) | b == 0, NA_real_, a / b)

cagr <- function(v0, v1, years) {
  ifelse(is.na(v0) | is.na(v1) | v0 <= 0 | v1 <= 0 | years <= 0,
         NA_real_, (v1 / v0)^(1 / years) - 1)
}

detect_columns <- function(cols) {
  used <- character(0)
  out  <- setNames(rep(NA_character_, length(FIELD_PATTERNS)), names(FIELD_PATTERNS))
  for (f in names(FIELD_PATTERNS)) {
    if (f %in% names(COLUMN_OVERRIDES)) {
      out[f] <- to_snake(COLUMN_OVERRIDES[[f]]); used <- c(used, out[f]); next
    }
    for (p in FIELD_PATTERNS[[f]]) {
      hit <- setdiff(grep(p, cols, value = TRUE), used)
      if (length(hit)) { out[f] <- hit[1]; used <- c(used, hit[1]); break }
    }
  }
  out
}

# ---- Demo data (only used if the real file is missing) --------------------
make_demo_data <- function(n_schools = 40, years = 2014:2025, seed = 406) {
  set.seed(seed)
  tiers <- sample(c("Power 4", "Group of 5", "FCS"), n_schools, TRUE, c(.35, .35, .30))
  base  <- c("Power 4" = 140e6, "Group of 5" = 45e6, "FCS" = 22e6)
  sub   <- c("Power 4" = .06,   "Group of 5" = .45,  "FCS" = .70)
  schools <- tibble(
    Institution = sprintf("Demo State %02d", seq_len(n_schools)),
    Subdivision = ifelse(tiers == "FCS", "FCS", "FBS"),
    Conference  = ifelse(tiers == "Power 4", sample(c("SEC", "Big Ten", "Big 12", "ACC"), n_schools, TRUE),
                  ifelse(tiers == "Group of 5", sample(c("Sun Belt", "MAC", "Mountain West"), n_schools, TRUE),
                         sample(c("SoCon", "Big Sky", "CAA"), n_schools, TRUE))),
    rev0  = base[tiers] * runif(n_schools, .7, 1.3),
    subsh = pmin(.9, pmax(.01, sub[tiers] + rnorm(n_schools, 0, .08))),
    g_rev = rnorm(n_schools, .045, .015),
    g_exp = rnorm(n_schools, .055, .015),
    debt_mult = runif(n_schools, .3, 2.2)
  )
  tiers_lookup <- setNames(tiers, schools$Institution)
  tidyr::crossing(schools, Year = years) |>
    mutate(
      t = Year - min(years),
      `Total Revenue` = rev0 * (1 + g_rev)^t,
      `Total Expenses` = rev0 * 0.99 * (1 + g_exp)^t,
      `Institutional Funding/Government Support/Student Fees` = `Total Revenue` * subsh,
      `Media Rights` = `Total Revenue` * ifelse(tiers_lookup[Institution] == "Power 4", .38, .08),
      `NCAA/Conference Distributions` = `Total Revenue` * .06,
      `Donor Contributions` = `Total Revenue` * runif(n(), .10, .22),
      `Facilities, Debt Service, and Equipment` = `Total Expenses` * runif(n(), .12, .26),
      `Total Athletics Debt` = `Total Revenue` * debt_mult,
      `Coaches Compensation` = `Total Expenses` * runif(n(), .15, .22),
      `Non-Coaching Athletics Staff Compensation` = `Total Expenses` * runif(n(), .13, .19)
    ) |>
    select(-rev0, -subsh, -g_rev, -g_exp, -debt_mult, -t)
}

# ---- 1. Load ---------------------------------------------------------------
if (file.exists(KNAD_PATH)) {
  message("Loading ", KNAD_PATH)
  raw <- if (grepl("\\.xlsx?$", KNAD_PATH, ignore.case = TRUE)) {
    read_excel(KNAD_PATH, sheet = 1, guess_max = 10000)
  } else {
    read_csv(KNAD_PATH, show_col_types = FALSE, guess_max = 10000)
  }
  DEMO <- FALSE
} else {
  message("** ", KNAD_PATH, " not found: running in DEMO mode on synthetic data **")
  raw <- make_demo_data()
  DEMO <- TRUE
}
names(raw) <- to_snake(names(raw))

# ---- 2. Map columns -------------------------------------------------------
map <- detect_columns(names(raw))
message("\nColumn mapping:")
for (f in names(map)) message(sprintf("  %-15s <- %s", f, ifelse(is.na(map[f]), "(not found)", map[f])))

missing_req <- REQUIRED[is.na(map[REQUIRED])]
if (length(missing_req)) {
  message("\nAvailable columns:\n  ", paste(names(raw), collapse = "\n  "))
  stop("Required field(s) not found: ", paste(missing_req, collapse = ", "),
       ". Add them to COLUMN_OVERRIDES.", call. = FALSE)
}

found <- map[!is.na(map)]
df <- raw |>
  select(all_of(setNames(found, names(found)))) |>
  mutate(year = as.integer(gsub("\\D", "", as.character(year))))

money_fields <- setdiff(names(df), c("school", "year", "conference", "subdivision"))
df <- df |> mutate(across(all_of(money_fields), parse_money))

# Fill optional fields that weren't found with NA so the ratio code still runs
for (f in setdiff(names(FIELD_PATTERNS), names(df))) df[[f]] <- NA_real_
if (all(is.na(df$conference)))  df$conference  <- NA_character_
if (all(is.na(df$subdivision))) df$subdivision <- NA_character_

# Build "allocated" (subsidy) from parts if there's no combined column
if (all(is.na(df$allocated))) {
  df <- df |> mutate(allocated = rowSums(cbind(inst_support, student_fees, govt_support), na.rm = TRUE),
                     allocated = ifelse(is.na(inst_support) & is.na(student_fees) & is.na(govt_support),
                                        NA_real_, allocated))
}

# Service academies are funded differently (the Knight Commission excludes them too)
EXCLUDE_REGEX <- "Military Academy|Naval Academy|Air Force Academy"

df <- df |> filter(!is.na(school), !is.na(year), !is.na(total_revenue), total_revenue > 0,
                   !grepl(EXCLUDE_REGEX, school))

# Tier: Power 4 by conference name, else FBS -> Group of 5, else FCS / other
p4_regex <- paste0("\\b(", paste(POWER4_CONFS, collapse = "|"), ")\\b")
df <- df |> mutate(
  tier = case_when(
    !is.na(conference) & grepl(p4_regex, conference, ignore.case = TRUE) ~ "Power 4",
    !is.na(subdivision) & grepl("power", subdivision, ignore.case = TRUE) ~ "Power 4",
    !is.na(subdivision) & grepl("fbs|bowl|group", subdivision, ignore.case = TRUE) ~ "Group of 5",
    TRUE ~ "FCS / other"))

# ---- 3. Ratios per school-year ---------------------------------------------
ratios <- df |>
  mutate(
    generated_revenue   = total_revenue - coalesce(allocated, 0),
    subsidy_share       = safe_div(allocated, total_revenue),                    # higher = worse
    debt_service_share  = safe_div(debt_service, total_revenue),                 # higher = worse
    debt_to_revenue     = safe_div(total_debt, total_revenue),                   # higher = worse
    media_conf_share    = safe_div(coalesce(media_rights, 0) + coalesce(ncaa_conf_dist, 0),
                                   total_revenue),                               # concentration
    comp_share          = safe_div(coalesce(coach_comp, 0) + coalesce(staff_comp, 0),
                                   total_expenses),                              # higher = less flexible
    stand_alone_margin  = safe_div(generated_revenue - total_expenses, total_expenses)  # lower = worse
  ) |>
  mutate(media_conf_share = ifelse(is.na(media_rights) & is.na(ncaa_conf_dist), NA_real_, media_conf_share),
         comp_share       = ifelse(is.na(coach_comp) & is.na(staff_comp), NA_real_, comp_share))

# ---- 4. Expense growth vs revenue growth ----------------------------------
end_year <- ifelse(is.na(TREND_END_YEAR), max(ratios$year), TREND_END_YEAR)
trend <- ratios |>
  filter(year >= TREND_START_YEAR, year <= end_year) |>
  group_by(school) |>
  arrange(year, .by_group = TRUE) |>
  summarise(
    first_year = first(year), last_year = last(year),
    rev_cagr = cagr(first(total_revenue), last(total_revenue), last(year) - first(year)),
    exp_cagr = cagr(first(total_expenses), last(total_expenses), last(year) - first(year)),
    .groups = "drop"
  ) |>
  mutate(growth_gap = exp_cagr - rev_cagr)                                       # higher = worse

# ---- 5. Composite stress score (latest year per school) -------------------
latest <- ratios |>
  group_by(school) |> filter(year == max(year)) |> ungroup() |>
  left_join(trend, by = "school")

# direction: +1 means a higher value is more stressed.
# stand_alone_margin is reported but left out of the score: it is almost the
# mirror image of subsidy_share, so including both would double-count it.
score_vars <- c(subsidy_share = 1, debt_service_share = 1, debt_to_revenue = 1,
                media_conf_share = 1, comp_share = 1, growth_gap = 1)
z <- function(x) { s <- sd(x, na.rm = TRUE); if (is.na(s) || s == 0) rep(NA_real_, length(x)) else (x - mean(x, na.rm = TRUE)) / s }

# Scores are standardized WITHIN each tier, so an FCS school is compared with
# FCS peers rather than with the SEC.
latest <- latest |> group_by(tier)
for (v in names(score_vars)) latest <- latest |> mutate(!!paste0("z_", v) := score_vars[[v]] * z(.data[[v]]))
zcols <- paste0("z_", names(score_vars))
latest <- latest |>
  mutate(n_metrics    = rowSums(!is.na(across(all_of(zcols)))),
         stress_score = rowMeans(across(all_of(zcols)), na.rm = TRUE),
         stress_score = ifelse(n_metrics < 3, NA_real_, stress_score)) |>
  arrange(desc(stress_score), .by_group = TRUE) |>
  mutate(tier_rank = row_number(), tier_n = n()) |>
  ungroup() |>
  arrange(desc(stress_score)) |>
  mutate(overall_rank = row_number())

# ---- 6. Revenue-sharing shock + scenarios ----------------------------------
# Each school pays REV_SHARE_CAP x its tier's assumed share (see config).
shock <- tidyr::crossing(latest |> select(school, conference, subdivision, tier, year, total_revenue,
                                          total_expenses, allocated, generated_revenue,
                                          media_rights, donations),
                         SCENARIOS) |>
  mutate(
    lost_revenue      = coalesce(media_rights, 0) * media_cut + coalesce(donations, 0) * donor_cut,
    rev_share_paid    = REV_SHARE_CAP * unname(REV_SHARE_BY_TIER[tier]),
    shocked_expenses  = total_expenses + rev_share_paid,
    shocked_generated = generated_revenue - lost_revenue,
    shocked_margin    = safe_div(shocked_generated - shocked_expenses, shocked_expenses),
    subsidy_needed    = pmax(0, shocked_expenses - shocked_generated),
    subsidy_increase  = subsidy_needed - pmax(0, total_expenses - generated_revenue),
    rev_share_pct_rev = rev_share_paid / total_revenue
  ) |>
  arrange(scenario, shocked_margin)

# ---- 7. Save + summary ----------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE)
write_csv(ratios, file.path(OUT_DIR, "ratios_panel.csv"))
write_csv(latest, file.path(OUT_DIR, "stress_latest.csv"))
write_csv(shock,  file.path(OUT_DIR, "shock_results.csv"))

pct <- function(x) sprintf("%.1f%%", 100 * x)
message("\n", if (DEMO) "[DEMO DATA] " else "", "Schools: ", n_distinct(ratios$school),
        " | Years: ", min(ratios$year), "-", max(ratios$year))
message("Share of schools losing money without subsidies (latest year): ",
        pct(mean(latest$stand_alone_margin < 0, na.rm = TRUE)))

message("\nBaseline by tier (latest year, medians):")
print(latest |> group_by(tier) |>
        summarise(schools = n(),
                  subsidy_share = pct(median(subsidy_share, na.rm = TRUE)),
                  stand_alone_margin = pct(median(stand_alone_margin, na.rm = TRUE)),
                  debt_to_revenue = round(median(debt_to_revenue, na.rm = TRUE), 2),
                  growth_gap = pct(median(growth_gap, na.rm = TRUE)),
                  paid_rev_share = sum(coalesce(rev_share_actual, 0) > 0)))

message("\nTop 5 most stressed within each tier:")
print(latest |> filter(tier_rank <= 5) |>
        select(tier, tier_rank, school, subsidy_share, debt_to_revenue, growth_gap, stress_score) |>
        mutate(across(c(subsidy_share, growth_gap), pct),
               debt_to_revenue = round(debt_to_revenue, 2), stress_score = round(stress_score, 2)),
      n = Inf, width = Inf)

message("\nRevenue-sharing shock, median by tier:")
print(shock |> group_by(scenario, tier) |>
        summarise(median_margin = pct(median(shocked_margin, na.rm = TRUE)),
                  median_extra_subsidy_M = round(median(subsidy_increase, na.rm = TRUE) / 1e6, 1),
                  .groups = "drop"), n = Inf)

message("\nSaved to ", normalizePath(OUT_DIR), "/")
if (!DEMO) message("Source: Knight-Newhouse College Athletics Database (knightnewhousedata.org)")
