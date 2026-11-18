# =============================================================================
# eada_prep.R: Carson-Newman and South Atlantic Conference data for the app
# Denver Slifka
#
# Data: U.S. Department of Education, Equity in Athletics (EADA), 2024-25,
#   "All data combined" file (one row per institution), from
#   https://ope.ed.gov/athletics/#/datafile/list
#   Save EADA_2025.xlsx in data/eada_2025/.
#
# Writes app/data/eada_sac.csv and app/data/eada_benchmarks.csv
# =============================================================================

suppressPackageStartupMessages({ library(readxl); library(dplyr); library(readr) })

EADA_PATH <- "data/eada_2025/EADA_2025.xlsx"

# South Atlantic Conference members in 2024-25 (EADA unitid -> short name).
# Limestone University was also a member but closed in spring 2025 and is not in the file.
SAC <- c("219806" = "Carson-Newman", "217633" = "Anderson (SC)", "198215" = "Catawba",
         "217907" = "Coker", "232025" = "Emory & Henry", "198835" = "Lenoir-Rhyne",
         "220631" = "Lincoln Memorial", "198899" = "Mars Hill", "218414" = "Newberry",
         "221953" = "Tusculum", "233897" = "UVA Wise", "199962" = "Wingate")

raw <- read_excel(EADA_PATH, guess_max = 5000)

z <- function(x) ifelse(is.na(x), 0, x)
eada <- raw |>
  transmute(
    unitid, school = institution_name, state = state_cd,
    classification = trimws(classification_name), sector = sector_name,
    undergrads   = EFTotalCount,                                   # full-time undergraduates
    athletes     = z(UNDUP_CT_PARTIC_MEN) + z(UNDUP_CT_PARTIC_WOMEN), # unduplicated athletes
    revenue      = GRND_TOTAL_REVENUE,
    expenses     = GRND_TOTAL_EXPENSE,
    athletic_aid = STUDENTAID_TOTAL,
    recruiting   = RECRUITEXP_TOTAL
  ) |>
  mutate(
    athlete_share    = athletes / undergrads,
    exp_per_athlete  = expenses / athletes,
    aid_share_of_exp = athletic_aid / expenses,
    rev_equals_exp   = abs(revenue - expenses) < 1
  )

sac <- eada |>
  filter(as.character(unitid) %in% names(SAC)) |>
  mutate(short = unname(SAC[as.character(unitid)]),
         is_cn = short == "Carson-Newman")

group_of <- function(cls, sector) {
  case_when(
    grepl("^NCAA Division I-FBS|^NCAA Division I-FCS|^NCAA Division I without", cls) ~ "Division I",
    grepl("^NCAA Division II (with|without)", cls) & grepl("Private nonprofit", sector) ~ "Division II, private",
    grepl("^NCAA Division II (with|without)", cls) & grepl("Public", sector) ~ "Division II, public",
    TRUE ~ NA_character_)
}

bench <- eada |>
  mutate(group = group_of(classification, sector)) |>
  filter(!is.na(group), athletes > 0, undergrads > 0, expenses > 0) |>
  group_by(group) |>
  summarise(schools = n(),
            athlete_share = median(athlete_share, na.rm = TRUE),
            exp_per_athlete = median(exp_per_athlete, na.rm = TRUE),
            aid_share_of_exp = median(aid_share_of_exp, na.rm = TRUE),
            share_rev_equals_exp = mean(rev_equals_exp, na.rm = TRUE),
            .groups = "drop") |>
  bind_rows(
    sac |> summarise(group = "South Atlantic Conference", schools = n(),
                     athlete_share = median(athlete_share), exp_per_athlete = median(exp_per_athlete),
                     aid_share_of_exp = median(aid_share_of_exp),
                     share_rev_equals_exp = mean(rev_equals_exp)))

dir.create("app/data", recursive = TRUE, showWarnings = FALSE)
write_csv(sac,   "app/data/eada_sac.csv")
write_csv(bench, "app/data/eada_benchmarks.csv")
print(bench)
print(sac |> select(short, undergrads, athletes, athlete_share, exp_per_athlete) |> arrange(desc(athlete_share)))
