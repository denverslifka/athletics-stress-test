# =============================================================================
# College Athletics Stress Test: Shiny app (Shinylive-ready)
# Denver Slifka
#
# Reads the CSVs made by athletics_stress_test.R and eada_prep.R (in app/data/).
# Uses only packages that run in the browser through Shinylive:
# shiny, bslib, ggplot2, scales.
#
# Source: Knight-Newhouse College Athletics Database (knightnewhousedata.org)
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# ---- Data -------------------------------------------------------------------
latest <- read.csv("data/stress_latest.csv", stringsAsFactors = FALSE)
panel  <- read.csv("data/ratios_panel.csv",  stringsAsFactors = FALSE)

# Division II comparison (EADA 2024-25), written by eada_prep.R
sac   <- read.csv("data/eada_sac.csv", stringsAsFactors = FALSE)
bench <- read.csv("data/eada_benchmarks.csv", stringsAsFactors = FALSE)
cn    <- sac[sac$short == "Carson-Newman", ][1, ]
b_get <- function(g, col) bench[[col]][bench$group == g]

TIERS <- c("Power 4", "Group of 5", "FCS / other")
latest$tier <- factor(latest$tier, levels = TIERS)
panel$tier  <- factor(panel$tier,  levels = TIERS)
LATEST_YEAR   <- max(latest$year, na.rm = TRUE)
REV_SHARE_CAP <- 21.5e6

# ---- Palette (research note) -------------------------------------------------
INK    <- "#16202B"   # text
LEDGER <- "#1D4E89"   # accent: report blue (Power 4 / primary series)
MID    <- "#5E86B5"   # Group of 5
SKY    <- "#9DB7D5"   # FCS / other
RED    <- "#B4442F"   # losses
ROSE   <- "#E6B3A8"   # mild losses
STRIPE <- "#F3F6FA"   # table stripe
PAPER  <- "#FFFFFF"   # background
GREY   <- "#7D8894"   # secondary
TIER_COL <- c("Power 4" = LEDGER, "Group of 5" = MID, "FCS / other" = SKY)

# ---- Formatting -------------------------------------------------------------
pct <- function(x, d = 1) {
  v <- round(100 * x, d); v[!is.na(v) & v == 0] <- 0
  ifelse(is.na(x), "-", paste0(formatC(v, format = "f", digits = d), "%"))
}
mil  <- function(x, d = 1) ifelse(is.na(x), "-", paste0("$", formatC(x / 1e6, format = "f", digits = d, big.mark = ","), "M"))
xmul <- function(x) ifelse(is.na(x), "-", paste0(formatC(x, format = "f", digits = 2), "x"))
esc  <- function(x) htmltools::htmlEscape(x)

n_schools    <- nrow(latest)
share_losing <- mean(latest$stand_alone_margin < 0, na.rm = TRUE)
n_losing     <- sum(latest$stand_alone_margin < 0, na.rm = TRUE)

tier_trend <- aggregate(subsidy_share ~ tier + year, data = panel, FUN = median)
P4_2020  <- tier_trend$subsidy_share[tier_trend$tier == "Power 4" & tier_trend$year == 2020]
P4_NOW   <- tier_trend$subsidy_share[tier_trend$tier == "Power 4" & tier_trend$year == LATEST_YEAR]
P4_MULT  <- c("two", "three", "four", "five", "six")[max(1, min(5, round(P4_NOW / P4_2020) - 1))]

margin_class <- function(m) ifelse(is.na(m), "na", ifelse(m >= 0, "ok", ifelse(m > -0.25, "mild", "deep")))

theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(text = element_text(colour = INK),
          axis.text = element_text(colour = GREY),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E6EBF0", linewidth = 0.4),
          legend.position = "top", legend.justification = "left", legend.title = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.title.position = "plot")
}

METRIC_LABEL <- c(subsidy_share = "Subsidy share", debt_service_share = "Debt service share",
                  debt_to_revenue = "Debt to revenue", media_conf_share = "Media and conference share",
                  comp_share = "Compensation share", growth_gap = "Growth gap",
                  stand_alone_margin = "Margin without subsidies")
METRIC_HELP <- c(
  subsidy_share      = "Institutional and government support plus student fees, as a share of revenue. Money the athletics program did not earn.",
  debt_service_share = "Annual debt service, leases and rent on athletic facilities, as a share of revenue.",
  debt_to_revenue    = "Athletics-related debt outstanding, divided by one year of revenue.",
  media_conf_share   = "Conference and NCAA distributions, media rights and bowl revenue, as a share of revenue. Measures dependence on a few TV-driven sources.",
  comp_share         = "Coach and staff compensation as a share of expenses. These costs are hard to cut quickly.",
  growth_gap         = "Yearly expense growth minus yearly revenue growth since 2014.",
  stand_alone_margin = "What the margin would be with subsidies removed. Shown everywhere, but left out of the score because it mirrors subsidy share."
)

# ---- The 235-school grid ----------------------------------------------------
school_grid <- function() {
  rows <- lapply(TIERS, function(t) {
    d <- latest[latest$tier == t, ]
    d <- d[order(d$stand_alone_margin, decreasing = TRUE), ]
    lost <- sum(d$stand_alone_margin < 0, na.rm = TRUE)
    cells <- lapply(seq_len(nrow(d)), function(i) {
      tags$button(
        class = paste("cell", margin_class(d$stand_alone_margin[i])),
        type = "button",
        title = paste0(d$school[i], ": ", pct(d$stand_alone_margin[i]), " without subsidies"),
        `aria-label` = paste0(d$school[i], ", margin without subsidies ", pct(d$stand_alone_margin[i])),
        onclick = sprintf("Shiny.setInputValue('grid_pick', %s, {priority: 'event'})",
                          jsonlite_quote(d$school[i]))
      )
    })
    div(class = "grid-row",
        div(class = "grid-label",
            div(class = "grid-tier", t),
            div(class = "grid-count", paste0(lost, " of ", nrow(d), " lose money"))),
        div(class = "cells", cells))
  })
  div(class = "school-grid", rows)
}
jsonlite_quote <- function(s) paste0("'", gsub("'", "\\\\'", s), "'")

# ---- CSS ------------------------------------------------------------------
CSS <- sprintf("
:root { --ink:%1$s; --blue:%2$s; --red:%3$s; --rose:%4$s; --stripe:%5$s; --paper:%6$s; --grey:%7$s; --rule:#DCE2E8; }
body { background: var(--paper); color: var(--ink); font-variant-numeric: tabular-nums; }
h1, h2, .serif, .lede, .num, .school-head { font-family: 'Source Serif 4', Georgia, serif; }
.navbar, nav.navbar.navbar-default, nav.navbar.navbar-inverse {
  --bslib-navbar-default-bg: #fff; background-color: #fff !important; border-bottom: 1px solid var(--rule); }
.navbar .navbar-brand { color: var(--ink) !important; font-weight: 700; font-size: 1.02rem; margin-right: 1.6rem; }
.navbar .nav-link { color: #4A5662 !important; }
.navbar .nav-link:hover { color: var(--ink) !important; }
.navbar .nav-link.active { color: var(--blue) !important; font-weight: 700; }
.page { max-width: 1140px; margin: 0 auto; padding: 12px 8px 56px; }

/* Findings hero: text left, chart right */
.hero { display: grid; grid-template-columns: 1fr 1.2fr; gap: 48px; align-items: start; margin-top: 30px; }
.lede { font-size: clamp(1.9rem, 3.4vw, 2.75rem); font-weight: 600; line-height: 1.12; letter-spacing: -0.01em; margin: 0 0 18px; }
.prose p { font-family: 'Source Serif 4', Georgia, serif; font-size: 1.15rem; line-height: 1.65; color: #33404D; max-width: 52ch; }
.byline { color: var(--grey); font-size: .88rem; margin-top: 20px; }
.fig { border-top: 3px solid var(--ink); padding-top: 12px; }
.fig h3 { font-size: 1rem; font-weight: 700; margin: 0; }
.fig .fig-sub { color: var(--grey); font-size: .88rem; margin-bottom: 4px; }
.stats { display: grid; grid-template-columns: repeat(4, 1fr); border-top: 1px solid var(--rule); margin-top: 36px; padding-top: 18px; }
.stats > div { padding-right: 20px; }
.stats .num { font-size: 2rem; font-weight: 600; line-height: 1.1; }
.stats .lab { color: #4A5662; font-size: .9rem; line-height: 1.4; margin-top: 4px; max-width: 26ch; }

/* School grid */
.section { margin-top: 52px; }
.section h2 { font-size: 1.5rem; font-weight: 600; margin: 0 0 6px; }
.section .intro { color: #4A5662; max-width: 68ch; margin-bottom: 16px; }
.school-grid { border-top: 3px solid var(--ink); }
.grid-row { display: grid; grid-template-columns: 170px 1fr; gap: 18px; padding: 14px 0; border-bottom: 1px solid var(--rule); }
.grid-tier { font-weight: 700; }
.grid-count { color: var(--grey); font-size: .88rem; }
.cells { display: flex; flex-wrap: wrap; gap: 4px; align-content: flex-start; }
.cell { width: 17px; height: 17px; border: 0; padding: 0; border-radius: 1px; cursor: pointer; }
.cell.ok { background: var(--blue); } .cell.mild { background: var(--rose); }
.cell.deep { background: var(--red); } .cell.na { background: #CBD2D8; }
.cell:hover { outline: 2px solid var(--ink); outline-offset: 1px; }
.cell:focus-visible { outline: 3px solid var(--ink); outline-offset: 2px; }
.legend { display: flex; flex-wrap: wrap; gap: 20px; font-size: .88rem; color: #4A5662; padding: 12px 0 0; }
.legend span::before { content: ''; display: inline-block; width: 12px; height: 12px; margin-right: 7px; vertical-align: -1px; }
.legend .ok::before { background: var(--blue); } .legend .mild::before { background: var(--rose); }
.legend .deep::before { background: var(--red); }
.context { max-width: 68ch; line-height: 1.65; }

/* Panels */
.card { border-radius: 2px; border: 0; border-top: 3px solid var(--ink); box-shadow: none; }
.card-header { background: #fff; font-weight: 700; border-bottom: 1px solid var(--rule); padding-left: 0; }
.card-body { padding-left: 0; padding-right: 0; }
.bslib-sidebar-layout { border: 0 !important; border-radius: 0; }
.bslib-sidebar-layout > .sidebar { background: #F7F9FB; border-right: 1px solid var(--rule) !important; }
.form-label, .control-label { font-weight: 600; }
.irs--shiny .irs-bar, .irs--shiny .irs-single { background: var(--blue); border-color: var(--blue); }
.irs--shiny .irs-handle { border-color: var(--blue); }
.help { color: #4A5662; font-size: .9rem; line-height: 1.5; }
.section-title { font-size: 1.5rem; font-weight: 600; margin: 4px 0 12px; }

/* Tier selector as a segmented control */
#rank_tier .shiny-options-group { display: flex; flex-direction: column; border: 1px solid #C9D2DB; background: #fff; }
#rank_tier .radio, #rank_tier .form-check { margin: 0; padding: 0; position: relative; }
#rank_tier .radio + .radio, #rank_tier .form-check + .form-check { border-top: 1px solid #E1E7ED; }
#rank_tier label { display: block; margin: 0; padding: 0; cursor: pointer; }
#rank_tier input[type=radio] { position: absolute; opacity: 0; width: 1px; height: 1px; }
#rank_tier label span { display: block; padding: 9px 12px; }
#rank_tier input[type=radio]:checked + span { background: var(--blue); color: #fff; font-weight: 600; }
#rank_tier input[type=radio]:focus-visible + span { outline: 3px solid var(--ink); outline-offset: -3px; }

/* Tables */
table.shiny-table { width: 100%% !important; border-collapse: collapse; font-size: .93rem; }
table.shiny-table thead th { border-bottom: 2px solid var(--ink) !important; font-weight: 700; vertical-align: bottom; }
table.shiny-table tbody tr:nth-child(even) td { background: var(--stripe); }
table.shiny-table td { border: 0 !important; border-bottom: 1px solid #EDF1F5 !important; }
.scorebar { display: flex; align-items: center; gap: 8px; min-width: 130px; }
.scorebar i { display: block; height: 8px; background: var(--blue); }
.scorebar b { font-weight: 600; min-width: 34px; text-align: right; }
.tierpill { white-space: nowrap; }
.neg { color: var(--red); font-weight: 600; }

/* Stat row on test tab */
.statrow { display: grid; grid-template-columns: repeat(3, 1fr); border-top: 3px solid var(--ink); margin-bottom: 22px; }
.statrow > div { padding: 14px 18px 10px 0; }
.statrow > div + div { padding-left: 18px; border-left: 1px solid var(--rule); }
.statrow .num { font-size: 1.9rem; font-weight: 600; }
.statrow .lab { color: #4A5662; font-size: .9rem; }

.school-head { font-size: 1.9rem; font-weight: 600; margin: 6px 0 2px; }
.school-meta { color: #4A5662; margin-bottom: 18px; }
.method dt { font-weight: 700; margin-top: 12px; }
.method dd { margin-left: 0; color: #33404D; max-width: 72ch; }
.footer-note { color: var(--grey); font-size: .88rem; margin-top: 28px; }

@media (max-width: 860px) {
  .hero { grid-template-columns: 1fr; gap: 24px; }
  .stats { grid-template-columns: 1fr 1fr; row-gap: 18px; }
  .grid-row { grid-template-columns: 1fr; gap: 8px; }
  .statrow { grid-template-columns: 1fr; }
  .statrow > div + div { border-left: 0; padding-left: 0; border-top: 1px solid var(--rule); }
  .cell { width: 15px; height: 15px; }
}
@media (prefers-reduced-motion: reduce) { * { transition: none !important; } }
", INK, LEDGER, RED, ROSE, STRIPE, PAPER, GREY)

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  id = "nav",
  title = "College Athletics Stress Test",
  theme = bs_theme(version = 5, bg = PAPER, fg = INK, primary = LEDGER, danger = RED,
                   base_font = font_collection(font_google("Public Sans", wght = c(400, 600, 700), local = FALSE),
                                               "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
                   heading_font = font_collection(font_google("Source Serif 4", wght = c(400, 600), local = FALSE),
                                                  "Georgia", "serif"),
                   "navbar-bg" = "#FFFFFF",
                   "border-radius" = "2px", "border-radius-sm" = "1px", "border-radius-lg" = "2px"),
  fillable = FALSE,
  header = tags$head(tags$style(HTML(CSS))),

  nav_panel("Findings", div(class = "page",
    div(class = "hero",
      div(
        h1(class = "lede", paste0("Power 4 athletics departments lean on their universities ",
                                  P4_MULT, " times as much as in 2020.")),
        div(class = "prose",
          p(paste0("The median Power 4 program got ", pct(P4_2020), " of its revenue from university support and ",
                   "student fees in 2020. By ", LATEST_YEAR, " it was ", pct(P4_NOW), ", and that is before revenue sharing begins.")),
          p("Below the Power 4, subsidies are not the exception. They are the business model.")),
        div(class = "byline", paste0("Denver Slifka. Data: Knight-Newhouse College Athletics Database, FY2014-", LATEST_YEAR, "."))
      ),
      div(class = "fig",
        h3("Median subsidy share of athletics revenue"),
        div(class = "fig-sub", "Public Division I schools, by tier"),
        plotOutput("hero_plot", height = "330px"))
    ),
    div(class = "stats",
      div(div(class = "num", pct(share_losing, 0)),
          div(class = "lab", paste0("of ", n_schools, " departments lose money without subsidies"))),
      div(div(class = "num", xmul(median(latest$debt_to_revenue[latest$tier == "Power 4"], na.rm = TRUE))),
          div(class = "lab", "median Power 4 facility debt, in years of revenue")),
      div(div(class = "num", "$21.5M"),
          div(class = "lab", "this year's revenue-sharing cap per school (2026-27)")),
      div(div(class = "num", textOutput("hero_p4_shock", inline = TRUE)),
          div(class = "lab", "median Power 4 margin without subsidies if the full cap is paid"))
    ),
    div(class = "section",
      h2("Every school, one square"),
      p(class = "intro", paste0("Each square is a public Division I school, colored by its FY", LATEST_YEAR,
                                " margin once university support and student fees are taken out. Select a square to open that school.")),
      school_grid(),
      div(class = "legend",
          span(class = "ok", "Profitable without subsidies"),
          span(class = "mild", "Loses up to 25%"),
          span(class = "deep", "Loses more than 25%"))
    ),
    div(class = "section context",
      h2("Why this exists"),
      p("In November 2026, former Iowa State athletic director Jamie Pollard warned that college sports could be ",
        "heading toward the next savings-and-loan crisis. The comparison is about long-term commitments (facility ",
        "debt, multi-year contracts, and now revenue sharing) paid for with revenue that can disappear quickly, ",
        "while the reporting is too thin to show how exposed anyone is."),
      p("This project examines every public Division I athletics department the way an auditor would: six ",
        "financial ratios, a stress score compared within each tier, and a scenario test against the revenue-sharing cap.")
    )
  )),

  nav_panel("Rankings", div(class = "page",
    layout_sidebar(
      sidebar = sidebar(width = 240,
        radioButtons("rank_tier", "Tier", choices = TIERS, selected = "Power 4"),
        sliderInput("rank_n", "Schools shown", min = 10, max = 125, value = 25, step = 5),
        p(class = "help", "The stress score averages six ratios, each compared with the school's own tier. ",
          "Above zero means more stressed than the tier average.")
      ),
      h2(class = "section-title", textOutput("rank_title", inline = TRUE)),
      tableOutput("rank_table")
    )
  )),

  nav_panel("School lookup", div(class = "page",
    layout_sidebar(
      sidebar = sidebar(width = 260,
        selectInput("school", "School", choices = sort(latest$school), selected = "The University of Tennessee")
      ),
      uiOutput("school_head"),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Ratios compared with the tier median"), tableOutput("school_ratios")),
        card(card_header("Earned revenue, subsidies and expenses"), plotOutput("school_trend", height = "290px"))
      ),
      card(card_header("Subsidy share over time"), plotOutput("school_subsidy", height = "240px"))
    )
  )),

  nav_panel("Revenue-sharing test", div(class = "page",
    layout_sidebar(
      sidebar = sidebar(width = 290,
        p(class = "help", "Choose how much of this year's $21.5M cap each tier pays. Defaults and the reasoning behind them:"),
        sliderInput("rs_p4",  "Power 4 pays",     min = 0, max = 100, value = 100, step = 5, post = "%"),
        p(class = "help", "68 schools paid at or within 5% of the cap in the first year (College Sports Commission filings)."),
        sliderInput("rs_g5",  "Group of 5 pays",  min = 0, max = 100, value = 40,  step = 5, post = "%"),
        p(class = "help", "American Athletic Conference schools expected to pay about $2M to $15M. 40% is near the middle."),
        sliderInput("rs_fcs", "FCS / other pays", min = 0, max = 100, value = 10,  step = 5, post = "%"),
        p(class = "help", "92 FCS schools opted in for 2025-26, but amounts were not reported. This one is an assumption."),
        hr(),
        sliderInput("media_cut", "Media and conference revenue falls by", min = 0, max = 50, value = 0, step = 5, post = "%"),
        sliderInput("donor_cut", "Donations fall by",                     min = 0, max = 50, value = 0, step = 5, post = "%")
      ),
      uiOutput("shock_stats"),
      card(card_header("Median margin without subsidies, today and after the test"),
           plotOutput("shock_plot", height = "290px")),
      card(card_header("Hardest hit: new subsidy needed as a share of today's revenue"), tableOutput("shock_table"))
    )
  )),

  nav_panel("Carson-Newman and Division II", div(class = "page",
    div(class = "hero",
      div(
        h1(class = "lede", paste0("At Carson-Newman, ", round(10 * cn$athlete_share),
                                  " in 10 full-time undergraduates play a varsity sport.")),
        div(class = "prose",
          p("Division I athletics is a media business that most schools subsidize. At small private Division II ",
            "schools like Carson-Newman, athletics works differently: it is one of the main ways the school fills its classes."),
          p("Athletes enroll, live on campus and usually pay at least part of their tuition, since most Division II ",
            "scholarships are partial. The athletics budget is part of what it costs to recruit them. So the useful question is not whether athletics turns a profit, but how much the school ",
            "spends per athlete and how much of its student body depends on it.")),
        div(class = "byline", "Data: U.S. Department of Education, Equity in Athletics (EADA), 2024-25.")
      ),
      div(class = "fig",
        h3("Varsity athletes as a share of full-time undergraduates"),
        div(class = "fig-sub", "South Atlantic Conference, 2024-25"),
        plotOutput("sac_share_plot", height = "360px"))
    ),
    div(class = "stats",
      div(div(class = "num", pct(cn$athlete_share, 0)),
          div(class = "lab", paste0("of Carson-Newman's ", format(cn$undergrads, big.mark = ","),
                                    " full-time undergraduates are athletes (", cn$athletes, ")"))),
      div(div(class = "num", pct(b_get("Division II, private", "athlete_share"), 0)),
          div(class = "lab", "median for private Division II schools")),
      div(div(class = "num", pct(b_get("Division I", "athlete_share"), 0)),
          div(class = "lab", "median for Division I schools")),
      div(div(class = "num", paste0("$", formatC(round(cn$exp_per_athlete / 1000, 1), format = "f", digits = 1), "K")),
          div(class = "lab", paste0("Carson-Newman athletics spending per athlete, vs. $",
                                    round(b_get("Division I", "exp_per_athlete") / 1000), "K at the median Division I school")))
    ),
    div(class = "section",
      h2("South Atlantic Conference, side by side"),
      p(class = "intro", "Spending and aid figures are what each school reported to the Department of Education for 2024-25."),
      tableOutput("sac_table")
    ),
    div(class = "section context",
      h2("Why Carson-Newman is not in the stress score"),
      p(paste0("The stress score depends on separating money an athletics department earns from money its university ",
               "gives it. Private schools are not required to report that split, and in the EADA data ",
               pct(b_get("Division II, private", "share_rev_equals_exp"), 0), " of private Division II schools report ",
               "athletics revenue exactly equal to expenses, because university support fills whatever gap exists. ",
               "Ranking them alongside public Division I schools would compare different things.")),
      p("The financial pressure is still real. Limestone University, a South Atlantic Conference member in 2024-25, ",
        "closed at the end of the spring 2025 semester after it could not raise $6 million to cover a budget shortfall. ",
        "It does not appear in the data above.")
    )
  )),

  nav_panel("Method", div(class = "page", div(class = "method",
    h2(class = "section-title", "Data"),
    p(class = "context", style = "margin-top:0",
      paste0("Knight-Newhouse College Athletics Database (knightnewhousedata.org): public NCAA Division I institutions, ",
             "FY2014 to FY", LATEST_YEAR, ", as reported by each school on its NCAA Financial Report. Service academies ",
             "are excluded. Dollar figures are not adjusted for inflation. Power 4 means the SEC, Big Ten, Big 12 and ACC ",
             "in the latest year; Group of 5 is the rest of FBS; FCS / other covers FCS and non-football Division I.")),
    h2(class = "section-title", style = "margin-top:28px", "The six ratios"),
    tags$dl(lapply(names(METRIC_LABEL), function(m) tagList(tags$dt(METRIC_LABEL[[m]]), tags$dd(METRIC_HELP[[m]])))),
    p(class = "context",
      "Each of the first six ratios becomes a z-score within the school's tier, signed so that higher means more ",
      "stressed. The stress score is their average. Schools with fewer than three ratios available are not scored."),
    h2(class = "section-title", style = "margin-top:28px", "Limitations"),
    tags$ul(class = "context",
      tags$li("The data is self-reported, and some schools show one-year jumps worth checking before citing."),
      tags$li("Heavily subsidized schools often report revenue equal to expenses, so their growth gap is near zero by construction."),
      tags$li("The revenue-sharing cap was $20.5M in 2025-26 and rises about 4% a year; the test uses the 2026-27 cap of $21.5M. Default payment levels are estimates from public reporting (see the test tab), not reported school figures. The FY2025 data predates the House settlement."),
      tags$li("Schools that recently moved to Division I, such as UC San Diego in 2020, have distorted growth figures.")
    ),
    p(class = "footer-note", "Built by Denver Slifka in R and Shiny. Source: Knight-Newhouse College Athletics Database.")
  )))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  output$hero_plot <- renderPlot({
    d <- tier_trend
    lab <- d[d$year == LATEST_YEAR, ]
    ggplot(d, aes(year, subsidy_share, colour = tier)) +
      geom_line(aes(linewidth = tier == "Power 4"), show.legend = FALSE) +
      geom_text(data = lab, aes(label = tier), hjust = 0, nudge_x = 0.3, fontface = "bold", size = 4.2, show.legend = FALSE) +
      scale_linewidth_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.1)) +
      scale_colour_manual(values = TIER_COL) +
      scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = c(2014, 2017, 2020, 2023, LATEST_YEAR), expand = expansion(add = c(0.2, 2.6))) +
      labs(x = NULL, y = NULL) + theme_app() + theme(legend.position = "none")
  })

  output$hero_p4_shock <- renderText({
    p4 <- latest[latest$tier == "Power 4", ]
    m  <- (p4$generated_revenue - (p4$total_expenses + REV_SHARE_CAP)) / (p4$total_expenses + REV_SHARE_CAP)
    paste0(pct(median(p4$stand_alone_margin, na.rm = TRUE), 0), " to ", pct(median(m, na.rm = TRUE), 0))
  })

  # Carson-Newman and Division II
  output$sac_share_plot <- renderPlot({
    d <- sac[order(sac$athlete_share), ]
    d$short <- factor(d$short, levels = d$short)
    med <- b_get("Division II, private", "athlete_share")
    ggplot(d, aes(athlete_share, short, fill = is_cn)) +
      geom_col(width = 0.68) +
      geom_vline(xintercept = med, colour = INK, linetype = "22", linewidth = 0.5) +
      annotate("text", x = med, y = 0.4, label = "Private D-II median", hjust = -0.05, vjust = 0, size = 3.4, colour = INK) +
      geom_text(aes(label = scales::percent(athlete_share, accuracy = 1)), hjust = -0.15, size = 3.6, colour = INK) +
      scale_fill_manual(values = c(`TRUE` = LEDGER, `FALSE` = SKY), guide = "none") +
      scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = NULL) + theme_app() +
      theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E6EBF0", linewidth = 0.4),
            axis.text.y = element_text(colour = INK, size = 11))
  })

  output$sac_table <- renderTable({
    d <- sac[order(-sac$athlete_share), ]
    nm <- ifelse(d$is_cn, paste0("<b>", esc(d$short), "</b>"), esc(d$short))
    data.frame(School = nm,
               `Full-time undergrads` = format(d$undergrads, big.mark = ","),
               Athletes = d$athletes,
               `Athlete share` = pct(d$athlete_share, 0),
               `Athletics spending` = mil(d$expenses),
               `Per athlete` = paste0("$", format(round(d$exp_per_athlete), big.mark = ",")),
               `Athletic aid share of spending` = pct(d$aid_share_of_exp, 0),
               check.names = FALSE)
  }, sanitize.text.function = identity, spacing = "s", align = "lrrrrrr")

  # Grid square -> open that school
  observeEvent(input$grid_pick, {
    updateSelectInput(session, "school", selected = input$grid_pick)
    nav_select("nav", "School lookup")
  })

  # Rankings
  output$rank_title <- renderText(paste0(input$rank_tier, ": most stressed in FY", LATEST_YEAR))
  output$rank_table <- renderTable({
    d <- latest[latest$tier == input$rank_tier & !is.na(latest$stress_score), ]
    d <- d[order(-d$stress_score), ][seq_len(min(input$rank_n, nrow(d))), ]
    mx <- max(abs(latest$stress_score[latest$tier == input$rank_tier]), na.rm = TRUE)
    bar <- sprintf("<div class='scorebar'><b>%s</b><i style='width:%dpx;%s'></i></div>",
                   formatC(d$stress_score, format = "f", digits = 2),
                   round(70 * pmax(0, d$stress_score) / mx),
                   ifelse(d$stress_score < 0, "background:#C9D2DB", ""))
    data.frame(
      Rank = d$tier_rank, School = esc(d$school), `Stress score` = bar,
      `Subsidy share` = pct(d$subsidy_share), `Debt to revenue` = xmul(d$debt_to_revenue),
      `Debt service` = pct(d$debt_service_share), `Media share` = pct(d$media_conf_share),
      `Growth gap` = pct(d$growth_gap), Revenue = mil(d$total_revenue),
      check.names = FALSE)
  }, sanitize.text.function = identity, spacing = "s", align = "lllrrrrrr")

  # School lookup
  sel <- reactive(latest[latest$school == input$school, ][1, ])

  output$school_head <- renderUI({
    s <- sel()
    tagList(
      div(class = "school-head", s$school),
      div(class = "school-meta",
          paste0(as.character(s$tier), if (!is.na(s$conference)) paste0(", ", s$conference), ". "),
          "Stress rank ", tags$b(paste0(s$tier_rank, " of ", s$tier_n)), " in its tier. ",
          "FY", s$year, " revenue ", tags$b(mil(s$total_revenue)), ".")
    )
  })

  output$school_ratios <- renderTable({
    s <- sel(); peers <- latest[latest$tier == s$tier, ]
    m <- names(METRIC_LABEL)
    fmt <- function(v, k) if (k == "debt_to_revenue") xmul(v) else pct(v)
    data.frame(Ratio = unname(METRIC_LABEL),
               School = vapply(m, function(k) fmt(s[[k]], k), ""),
               `Tier median` = vapply(m, function(k) fmt(median(peers[[k]], na.rm = TRUE), k), ""),
               check.names = FALSE, row.names = NULL)
  }, spacing = "s", align = "lrr")

  output$school_trend <- renderPlot({
    p <- panel[panel$school == input$school, ]; p <- p[order(p$year), ]
    long <- rbind(
      data.frame(year = p$year, value = p$generated_revenue, series = "Earned revenue"),
      data.frame(year = p$year, value = p$allocated,         series = "Subsidies"),
      data.frame(year = p$year, value = p$total_expenses,    series = "Expenses"))
    long$series <- factor(long$series, levels = c("Earned revenue", "Subsidies", "Expenses"))
    ggplot(long, aes(year, value / 1e6, colour = series, linetype = series)) +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = c("Earned revenue" = LEDGER, "Subsidies" = RED, "Expenses" = GREY)) +
      scale_linetype_manual(values = c("Earned revenue" = "solid", "Subsidies" = "solid", "Expenses" = "22")) +
      scale_x_continuous(breaks = seq(2014, LATEST_YEAR, 2)) +
      scale_y_continuous(labels = function(v) paste0("$", v, "M")) +
      labs(x = NULL, y = NULL) + theme_app()
  })

  output$school_subsidy <- renderPlot({
    s <- sel()
    tier_med <- aggregate(subsidy_share ~ year, data = panel[panel$tier == s$tier, ], FUN = median)
    p <- panel[panel$school == input$school, ]
    ggplot() +
      geom_line(data = tier_med, aes(year, subsidy_share, colour = "Tier median"), linewidth = 0.9, linetype = "22") +
      geom_line(data = p, aes(year, subsidy_share, colour = "This school"), linewidth = 1.2) +
      scale_colour_manual(values = c("This school" = LEDGER, "Tier median" = GREY)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = seq(2014, LATEST_YEAR, 2)) +
      labs(x = NULL, y = NULL) + theme_app()
  })

  # Revenue-sharing test
  shock <- reactive({
    share <- c("Power 4" = input$rs_p4, "Group of 5" = input$rs_g5, "FCS / other" = input$rs_fcs) / 100
    d <- latest
    media <- ifelse(is.na(d$media_rights), 0, d$media_rights)
    don   <- ifelse(is.na(d$donations), 0, d$donations)
    d$rev_share_paid <- REV_SHARE_CAP * share[as.character(d$tier)]
    d$lost           <- media * input$media_cut / 100 + don * input$donor_cut / 100
    d$shock_exp      <- d$total_expenses + d$rev_share_paid
    d$shock_gen      <- d$generated_revenue - d$lost
    d$base_margin    <- d$stand_alone_margin
    d$shock_margin   <- (d$shock_gen - d$shock_exp) / d$shock_exp
    d$extra_subsidy  <- pmax(0, d$shock_exp - d$shock_gen) - pmax(0, d$total_expenses - d$generated_revenue)
    d[!is.na(d$shock_margin), ]
  })

  output$shock_stats <- renderUI({
    d <- shock(); p4 <- d[d$tier == "Power 4", ]
    div(class = "statrow",
      div(div(class = "num", mil(sum(d$extra_subsidy, na.rm = TRUE), 0)),
          div(class = "lab", "in new subsidies needed each year, all schools")),
      div(div(class = "num", paste0(pct(median(p4$base_margin, na.rm = TRUE), 0), " to ",
                                    pct(median(p4$shock_margin, na.rm = TRUE), 0))),
          div(class = "lab", "typical Power 4 margin without subsidies")),
      div(div(class = "num", sum(d$extra_subsidy > 10e6, na.rm = TRUE)),
          div(class = "lab", "schools needing more than $10M extra a year")))
  })

  output$shock_plot <- renderPlot({
    d <- shock()
    agg <- rbind(
      data.frame(tier = TIERS, when = "Today",
                 margin = sapply(TIERS, function(t) median(d$base_margin[d$tier == t], na.rm = TRUE))),
      data.frame(tier = TIERS, when = "After the test",
                 margin = sapply(TIERS, function(t) median(d$shock_margin[d$tier == t], na.rm = TRUE))))
    agg$tier <- factor(agg$tier, levels = TIERS)
    agg$when <- factor(agg$when, levels = c("Today", "After the test"))
    ggplot(agg, aes(tier, margin, fill = when)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.62) +
      geom_hline(yintercept = 0, colour = INK) +
      geom_text(aes(label = scales::percent(margin, accuracy = 1), vjust = ifelse(margin < 0, 1.5, -0.5)),
                position = position_dodge(width = 0.72), size = 4, colour = INK) +
      scale_fill_manual(values = c("Today" = SKY, "After the test" = LEDGER)) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.14, 0.08))) +
      labs(x = NULL, y = NULL) + theme_app()
  })

  output$shock_table <- renderTable({
    d <- shock(); d$hit <- d$extra_subsidy / d$total_revenue
    d <- d[order(-d$hit), ][1:15, ]
    data.frame(School = esc(d$school),
               Tier = sprintf("<span class='tierpill'>%s</span>", d$tier),
               `Hit` = sprintf("<span class='neg'>%s</span>", pct(d$hit)),
               `Revenue sharing` = mil(d$rev_share_paid), `Lost revenue` = mil(d$lost),
               `New subsidy` = mil(d$extra_subsidy),
               `Margin today` = pct(d$base_margin), `Margin after` = pct(d$shock_margin),
               check.names = FALSE)
  }, sanitize.text.function = identity, spacing = "s", align = "llrrrrrr")
}

shinyApp(ui, server)
